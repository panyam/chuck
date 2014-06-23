
#include "chuck.h"

#define MAX_LOOKAHEAD_TOKENS  16

struct ChuckParser
{
    ChuckScanner *scanner;
    ChuckToken laTokens[MAX_LOOKAHEAD_TOKENS];
    int laTokenStart;
    int laTokenCount;
    LUASTNode *root;
};

typedef LUASTNode *(GetNonTermFunc)(ChuckParser *);
typedef int (*TokenToOpFunc)(ChuckTokenType tokenType);
LUASTNode *get_left_derived_expr_list(ChuckParser *parser,
                                      GetNonTermFunc get_nonterm,
                                      TokenToOpFunc token_to_op,
                                      LUASTNode *lexpr);
CUList *get_delimited_expr_list(ChuckParser *parser,
                                GetNonTermFunc get_nonterm,
                                ChuckTokenType delimiter);

void parser_token_expected_error(ChuckParser *parser, int token);
void parser_invalid_token_error(ChuckParser *parser, int token);
ChuckTokenType peekToken(ChuckParser *parser);
void advanceToken(ChuckParser *parser);
LUASTNode *get_statements(ChuckParser *parser);
LUASTNode *get_statement(ChuckParser *parser);
LUASTNode *get_for_statement_tail(ChuckParser *parser);
LUASTNode *get_while_statement_tail(ChuckParser *parser, int loopType);
LUASTNode *get_do_statement_tail(ChuckParser *parser);
LUASTNode *get_class_statement_tail(ChuckParser *parser);
ChuckASTFuncDefnNode *get_function_statement_tail(ChuckParser *parser);
LUASTNode *get_if_statement_tail(ChuckParser *parser);
LUASTNode *get_return_statement(ChuckParser *parser);
LUASTNode *get_break_statement(ChuckParser *parser);
LUASTNode *get_continue_statement(ChuckParser *parser);
LUASTNode *get_empty_statement(ChuckParser *parser);
LUASTNode *get_block_statement(ChuckParser *parser);
LUASTNode *get_chuck_expr(ChuckParser *parser);
LUASTNode *get_logical_expr(ChuckParser *parser);
LUASTNode *get_comparison_expr(ChuckParser *parser);
LUASTNode *get_bitwise_expr(ChuckParser *parser);
LUASTNode *get_arithmetic_expr(ChuckParser *parser);
LUASTNode *get_cast_expr(ChuckParser *parser);
LUASTNode *get_unary_expr(ChuckParser *parser);
LUASTNode *get_postfix_expr(ChuckParser *parser);
LUASTNode *get_primary_expr(ChuckParser *parser);
LUASTNode *get_new_expr_tail(ChuckParser *parser);
LUASTNode *get_complex_expr(ChuckParser *parser);
LUASTNode *get_polar_expr(ChuckParser *parser);
CUList *get_fun_param_list(ChuckParser *parser);
int token_to_loop_type(ChuckTokenType tokenType);
int token_to_chuck_op(ChuckTokenType tokenType);
int token_to_arithmetic_op(ChuckTokenType tokenType);
int token_to_bitwise_op(ChuckTokenType tokenType);
int token_to_comparison_op(ChuckTokenType tokenType);
int token_to_logical_op(ChuckTokenType tokenType);
int token_to_unary_op(ChuckTokenType tokenType);
ChuckASTVarDeclNode *get_var_decl(ChuckParser *parser);
LUType *get_type_declaration(ChuckParser *parser);
CUList *get_array_size_decls(ChuckParser *parser);
int get_array_empty_size_decls(ChuckParser *parser);
LUASTNode *get_array_subscript_tail(ChuckParser *parser);

int ensureTokens(ChuckParser *parser, int numTokens)
{
    assert(MAX_LOOKAHEAD_TOKENS >= numTokens && "Too many lookahead tokens");
    while (parser->laTokenCount < numTokens)
    {
        chuck_scanner_scan(parser->scanner);
        if (parser->scanner->currToken.tokenType == TOKEN_EOI)
        {
            return parser->laTokenCount;
        }
        parser->laTokens[parser->laTokenCount % MAX_LOOKAHEAD_TOKENS] = parser->scanner->currToken;
        parser->laTokenCount++;
    }
    return numTokens;
}

ChuckTokenType peekToken(ChuckParser *parser)
{
    if (ensureTokens(parser, 1) <= 0)
    {
        return TOKEN_EOI;
    }
    return parser->laTokens[parser->laTokenStart].tokenType;
}

/**
 * Returns the token at the nth lookahead token.
 */
ChuckToken get_token(ChuckParser *parser, int index)
{
    ensureTokens(parser, index + 1);
    return parser->laTokens[(parser->laTokenStart + index) % MAX_LOOKAHEAD_TOKENS];
}

ChuckToken get_next_token(ChuckParser *parser, BOOL advance)
{
    ChuckToken token = parser->laTokens[parser->laTokenStart];
    if (advance)
        advanceToken(parser);
    return token;
}

void advanceToken(ChuckParser *parser)
{
    // set token type to 0 so next time the parser will scan a new token
    parser->laTokenStart++;
    parser->laTokenCount--;
    if (parser->laTokenCount <= 0)
    {
        parser->laTokenStart = parser->laTokenCount = 0;
    }
}

#define expectAndAdvanceToken(parser, token, ...) {                                 \
    ChuckTokenType peekedToken = peekToken(parser);                                 \
    if (peekedToken != token)                                                       \
    {                                                                               \
        parser_token_expected_error(parser, token);                                 \
    }                                                                               \
    else                                                                            \
    {                                                                               \
        __VA_ARGS__;                                                                \
        advanceToken(parser);                                                       \
    }                                                                               \
}

void parser_invalid_token_error(ChuckParser *parser, int token)
{
    fprintf(stderr, "Line %d, Invalid Token: %s\n",
            parser->scanner->line + 1,
            chuck_scanner_token_string(token));
    assert(false);
}

void parser_token_expected_error(ChuckParser *parser, int token)
{
    fprintf(stderr, "Line %d, Expected %s, found %s\n",
            parser->scanner->line + 1,
            chuck_scanner_token_string(token),
            chuck_scanner_token_string(peekToken(parser)));
    assert(false);
}

LUASTNode *chuck_parse_stream(CUIStream *instream)
{
    ChuckParser parser;
    bzero(&parser, sizeof(parser));
    parser.scanner = chuck_scanner_create(instream);
    LUASTNode *root = get_statements(&parser);
    chuck_scanner_destroy(parser.scanner);
    return root;
}

/**
 * Gets a left derived expression list.
 */
LUASTNode *get_left_derived_expr_list(ChuckParser *parser,
                                      GetNonTermFunc get_nonterm,
                                      TokenToOpFunc token_to_op,
                                      LUASTNode *lexpr)
{
    if (!lexpr)
        lexpr = get_nonterm(parser);
    int next_op = token_to_op(peekToken(parser));
    while (next_op >= 0)
    {
        advanceToken(parser);
        LUASTNode *rexpr = get_nonterm(parser);
        lexpr = lu_ast_create_binary_node(next_op, lexpr, rexpr);
        next_op = token_to_op(peekToken(parser));
    }
    return lexpr;
}

/**
 * Get an expression list with a given set of delimiter.
 */
CUList *get_delimited_expr_list(ChuckParser *parser,
                                GetNonTermFunc get_nonterm,
                                ChuckTokenType delimiter)
{
    CUList *out = NULL;
    LUASTNode *expr = get_nonterm(parser);
    do
    {
        if (out == NULL)
            out = (CUList *)cu_linkedlist();
        cu_list_push_back(out, expr); expr = NULL;
        if (peekToken(parser) == delimiter)
        {
            advanceToken(parser);
            expr = get_nonterm(parser);
        }
    } while (expr != NULL);
    return out;
}

/**
 * statements ::= statement | statement statements
 */
LUASTNode *get_statements(ChuckParser *parser)
{
    LUASTNode *stmts = NULL;
    LUASTNode *stmt = get_statement(parser);
    while (stmt)
    {
        if (!stmts)
            stmts = lu_ast_create_statements_node(stmt);
        else
            stmts = lu_ast_add_statement(stmts, stmt);
        if (peekToken(parser) == TOKEN_EOI)
            break ;
        stmt = get_statement(parser);
    }
    return stmts;
}

/**
 * statement ::= LTLTLT param_list GTGTGT SEMICOLON .
 * statement ::= RETURN logical_expr(input) SEMICOLON .
 * statement ::= RETURN SEMICOLON .
 * statement ::= BREAK SEMICOLON .
 * statement ::= CONTINUE SEMICOLON .
 * statement ::= SEMICOLON .
 * statement ::= LBRACE statements RBRACE .
 * statement ::= FOR for_statement_tail .
 * statement ::= (REPEAT | WHILE | UNTIL) while_statement_tail .
 * statement ::= DO do_statement_tail .
 * statement ::= IF if_statement_tail .
 * statement ::= CLASS class_statement_tail .
 * statement ::= FUN function_statement_tail .
 * statement ::= chuck_expr SEMICOLON .
 */
LUASTNode *get_statement(ChuckParser *parser)
{
    ChuckTokenType nextToken = peekToken(parser);
    LUASTNode *out = NULL;
    if (nextToken == TOKEN_LBRACE)
    {
        advanceToken(parser);
        if (peekToken(parser) != TOKEN_RBRACE)
            out = get_statements(parser);
        expectAndAdvanceToken(parser, TOKEN_RBRACE);
    }
    else if (nextToken == TOKEN_LTLTLT)
    {
        advanceToken(parser);
        CUList *param_list = NULL;
        if (peekToken(parser) != TOKEN_GTGTGT)
            param_list = get_delimited_expr_list(parser, get_chuck_expr, TOKEN_COMMA);
        expectAndAdvanceToken(parser, TOKEN_GTGTGT);
        expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
        out = chuck_ast_create_print_node(param_list);
    }
    else if (nextToken == TOKEN_RETURN)
    {
        advanceToken(parser);
        if (peekToken(parser) == TOKEN_SEMICOLON)
            out = lu_ast_create_return_node(NULL);
        else
            out = lu_ast_create_return_node(get_logical_expr(parser));
        expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
    }
    else if (nextToken == TOKEN_BREAK)
    {
        advanceToken(parser);
        expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
        out = lu_ast_create_basic_node(LUAST_BREAK_NODE);
    }
    else if (nextToken == TOKEN_CONTINUE)
    {
        advanceToken(parser);
        expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
        out = lu_ast_create_basic_node(LUAST_CONTINUE_NODE);
    }
    else if (nextToken == TOKEN_SEMICOLON)
    {
        advanceToken(parser);
        out = lu_ast_create_basic_node(LUAST_EMPTY_NODE);
    }
    else if (nextToken == TOKEN_FOR)
    {
        advanceToken(parser);
        out = get_for_statement_tail(parser);
    }
    else if (nextToken == TOKEN_IF)
    {
        advanceToken(parser);
        out = get_if_statement_tail(parser);
    }
    else if (nextToken == TOKEN_DO)
    {
        advanceToken(parser);
        out = get_do_statement_tail(parser);
    }
    else if (nextToken == TOKEN_PUBLIC)
    {
        advanceToken(parser);
        // broken spec! after "public" it could be a "class" OR 
        // it could just be a func defn WITHOUT the "fun"
        if (peekToken(parser) == TOKEN_CLASS)
        {
            advanceToken(parser);
            out = get_class_statement_tail(parser);
            ((ChuckASTClassDefnNode *)out)->is_public = TRUE;
        }
        else
        {
            out = (LUASTNode *)get_function_statement_tail(parser);
        }
    }
    else if (nextToken == TOKEN_CLASS)
    {
        advanceToken(parser);
        out = get_class_statement_tail(parser);
    }
    else if (nextToken == TOKEN_FUN)
    {
        advanceToken(parser);
        out = (LUASTNode *)get_function_statement_tail(parser);
    }
    else if (nextToken == TOKEN_REPEAT || nextToken == TOKEN_WHILE || nextToken == TOKEN_UNTIL)
    {
        advanceToken(parser);
        out = get_while_statement_tail(parser, token_to_loop_type(nextToken));
    }
    else if (nextToken != TOKEN_EOI && nextToken != TOKEN_RBRACE)
    {
        out = get_chuck_expr(parser);
        expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
    }
    return out;
}

/**
 * chuck_expr ::= logical_expr UN_CHUCK logical_expr .
 * chuck_expr ::= ( logical_expr chuck_op ) * logical_expr .  
 */
LUASTNode *get_chuck_expr(ChuckParser *parser)
{
    LUASTNode *lexpr = get_logical_expr(parser);
    if (peekToken(parser) == TOKEN_UN_CHUCK)
    {
        advanceToken(parser);
        LUASTNode *rexpr = get_logical_expr(parser);
        lexpr = lu_ast_create_assignment_node(false, lexpr, rexpr);
    }
    else
    {
        lexpr = get_left_derived_expr_list(parser, get_logical_expr, token_to_chuck_op, lexpr);
    }
    return lexpr;
}

/**
 * logical_expr ::= ( comparison_expr logical_op ) * comparison_expr . // logical and, or, xor
 */
LUASTNode *get_logical_expr(ChuckParser *parser)
{
    return get_left_derived_expr_list(parser, get_comparison_expr, token_to_logical_op, NULL);
}

/**
 * comparison_expr ::= ( bitwise_expr comparison_op ) * bitwise_expr .
 */
LUASTNode *get_comparison_expr(ChuckParser *parser)
{
    return get_left_derived_expr_list(parser, get_bitwise_expr, token_to_comparison_op, NULL);
}

/**
 * bitwise_expr ::= arithmetic_expr ( bitwise_op arithmetic_expr ) 
 */
LUASTNode *get_bitwise_expr(ChuckParser *parser)
{
    return get_left_derived_expr_list(parser, get_arithmetic_expr, token_to_bitwise_op, NULL);
}

/**
 * arithmetic_expr ::= ( cast_expr arithmetic_op ) * cast_expr 
 */
LUASTNode *get_arithmetic_expr(ChuckParser *parser)
{
    return get_left_derived_expr_list(parser, get_cast_expr, token_to_arithmetic_op, NULL);
}

/**
 * cast_expr ::= unary_expr ( CAST type_declaration ) *
 */
LUASTNode *get_cast_expr(ChuckParser *parser)
{
    LUASTNode *unary_expr = get_unary_expr(parser);
    while (peekToken(parser) == TOKEN_DOLLAR)
    {
        advanceToken(parser);
        LUType *target_type = get_type_declaration(parser);
        unary_expr = lu_ast_create_cast_node(unary_expr, target_type);
    }
    return unary_expr;
}

/**
 * unary_expr ::= PLUSPLUS unary_expr .
 * unary_expr ::= MINUSMINUS unary_expr .
 * unary_expr ::= BIT_NOT unary_expr .
 * unary_expr ::= NOT unary_expr .
 * unary_expr ::= MINUS unary_expr .
 * unary_expr ::= postfix_expr .
 */
LUASTNode *get_unary_expr(ChuckParser *parser)
{
    int unary_op = token_to_unary_op(peekToken(parser));
    if (unary_op >= 0)
    {
        advanceToken(parser);
        return lu_ast_create_unary_node(unary_op, get_unary_expr(parser), true);
    }
    else
    {
        return get_postfix_expr(parser);
    }
}

/**
 * postfix_expr ::= ( primary_expr postfix_op ) * primary_expr;
 * 
 *     postifx_op ::= LPAREN RPAREN 
 *     postifx_op ::= LPAREN param_list RPAREN 
 *     postifx_op ::= COLONCOLON IDENTIFIER 
 *     postifx_op ::= DOT IDENTIFIER 
 *     postifx_op ::= PLUSPLUS 
 *     postifx_op ::= MINUSMINUS 
 *     postifx_op ::= array_subscript 
 */
LUASTNode *get_postfix_expr(ChuckParser *parser)
{
    LUASTNode *primary_expr = get_primary_expr(parser);
    while (true)
    {
        ChuckTokenType peekedToken = peekToken(parser);
        if (peekedToken == TOKEN_LPAREN)
        {
            advanceToken(parser);
            CUList *params = NULL;
            if (peekToken(parser) != TOKEN_RPAREN)
                params = get_delimited_expr_list(parser, get_logical_expr, TOKEN_COMMA);
            expectAndAdvanceToken(parser, TOKEN_RPAREN);
            primary_expr = lu_ast_create_func_call_node(primary_expr, params);
        }
        else if (peekedToken == TOKEN_COLONCOLON)
        {
            advanceToken(parser);
            expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
                primary_expr = chuck_ast_create_time_node(primary_expr, get_next_token(parser, false).value.stringValue);
            });
        }
        else if (peekedToken == TOKEN_DOT)
        {
            advanceToken(parser);
            expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
                primary_expr = lu_ast_create_member_access_node(primary_expr, get_next_token(parser, false).value.stringValue);
            });
        }
        else if (peekedToken == TOKEN_PLUSPLUS)
        {
            advanceToken(parser);
            primary_expr = lu_ast_create_unary_node(CHUCK_OP_PLUSPLUS, primary_expr, false);
        }
        else if (peekedToken == TOKEN_MINUSMINUS)
        {
            advanceToken(parser);
            primary_expr = lu_ast_create_unary_node(CHUCK_OP_MINUSMINUS, primary_expr, false);
        }
        else if (peekedToken == TOKEN_LSQUARE)
        {
            advanceToken(parser);
            LUASTNode *subscript = get_array_subscript_tail(parser);
            primary_expr = lu_ast_create_subscript_node(primary_expr, subscript);
        }
        else
        {
            break ;
        }
    }
    return primary_expr;
}

/**
 * primary_expr ::= LPAREN logical_expr RPAREN .
 * primary_expr ::= LONG .
 * primary_expr ::= DOUBLE .
 * primary_expr ::= STRING .
 * primary_expr ::= IDENTIFIER .
 * primary_expr ::= LSQUARE param_list RSQUARE .    // array creation
 * primary_expr ::= NEW type_declaration .
 * primary_expr ::= complex_expression .
 * primary_expr ::= polar_expression .
 * primary_expr ::= var_decl .
 * primary_expr ::= SPORK TILDE logical_expr .
 */
LUASTNode *get_primary_expr(ChuckParser *parser)
{
    LUASTNode *out = NULL;
    ChuckTokenType peekedToken = peekToken(parser);
    if (peekToken(parser) == TOKEN_SPORK)
    {
        advanceToken(parser);
        expectAndAdvanceToken(parser, TOKEN_TILDE);
        LUASTNode *expr = get_logical_expr(parser);
        return chuck_ast_create_spork_node(expr);
    }
    else if (peekedToken == TOKEN_LSQUARE)
    {
        advanceToken(parser);
        CUList *param_list = get_delimited_expr_list(parser, get_logical_expr, TOKEN_COMMA);
        out = lu_ast_create_list_node(param_list);
        expectAndAdvanceToken(parser, TOKEN_RSQUARE);
    }
    else if (peekedToken == TOKEN_LPAREN)
    {
        advanceToken(parser);
        out = get_logical_expr(parser);
        expectAndAdvanceToken(parser, TOKEN_RPAREN);
    }
    else if (peekedToken == TOKEN_LONG)
    {
        out = lu_ast_create_long_node(get_next_token(parser, false).value.longValue);
        advanceToken(parser);
    }
    else if (peekedToken == TOKEN_DOUBLE)
    {
        out = lu_ast_create_double_node(get_next_token(parser, false).value.doubleValue);
        advanceToken(parser);
    }
    else if (peekedToken == TOKEN_NEW)
    {
        advanceToken(parser);
        out = get_new_expr_tail(parser);
    }
    else if (peekedToken == TOKEN_STATIC)
    {
        advanceToken(parser);
        out = (LUASTNode *)get_var_decl(parser);
        for (ChuckASTVarDeclNode *var_node = (ChuckASTVarDeclNode *)out;var_node != NULL;var_node = var_node->next)
        {
            var_node->is_static = TRUE;
        }
    }
    else if (peekedToken == TOKEN_IDENTIFIER)
    {
        ChuckTokenType nextToken = get_token(parser, 1).tokenType;
        if (nextToken == TOKEN_IDENTIFIER || nextToken == TOKEN_AT)
        {
            // we have a var decl
            out = (LUASTNode *)get_var_decl(parser);
        }
        else
        {
            CUString *name = get_next_token(parser, false).value.stringValue;
            out = lu_ast_create_variable_node(name);
            advanceToken(parser);
        }
    }
    else if (peekedToken == TOKEN_STRING)
    {
        out = lu_ast_create_string_node(get_next_token(parser, false).value.stringValue);
        advanceToken(parser);
    }
    else if (peekedToken == TOKEN_HASH)
    {
        out = get_complex_expr(parser);
    }
    else if (peekedToken == TOKEN_PERCENT)
    {
        out = get_polar_expr(parser);
    }
    else
    {
        // parser_invalid_token_error(parser, peekedToken);
    }
    return out;
}

/**
 * new_expr_tail ::= IDENTIFIER (array_subscripts) AT
 */
LUASTNode *get_new_expr_tail(ChuckParser *parser)
{
    LUType *out_type = get_type_declaration(parser);
    LUASTNode *subscripts = NULL;
    if (peekToken(parser) == TOKEN_LSQUARE)
    {
        advanceToken(parser);
        subscripts = get_array_subscript_tail(parser);
    }
    LUASTNode *out = chuck_ast_create_new_node(out_type);
    return out;
}

/**
 * complex_expr ::= HASH LPAREN logical_expr COMMA logical_expr RPAREN .
 */
LUASTNode *get_complex_expr(ChuckParser *parser)
{
    expectAndAdvanceToken(parser, TOKEN_HASH);
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *x = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_COMMA);
    LUASTNode *y = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    return chuck_ast_create_complex_node(x,y,false);
}

/**
 * polar_expr ::= HASH LPAREN logical_expr COMMA logical_expr RPAREN .
 */
LUASTNode *get_polar_expr(ChuckParser *parser)
{
    expectAndAdvanceToken(parser, TOKEN_PERCENT);
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *x = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_COMMA);
    LUASTNode *y = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    return chuck_ast_create_complex_node(x,y,true);
}

int token_to_loop_type(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_WHILE: return CHUCK_WHILE_LOOP;
        case TOKEN_FOR: return CHUCK_FOR_LOOP;
        case TOKEN_UNTIL: return CHUCK_UNTIL_LOOP;
        case TOKEN_REPEAT: return CHUCK_REPEAT_LOOP;
        default: return -1;
    }
    return -1;
}

/**
 * chuck_op ::= CHUCK .
 * chuck_op ::= UP_CHUCK .
 * chuck_op ::= RSHIFT_CHUCK .
 * chuck_op ::= LSHIFT_CHUCK .
 * chuck_op ::= MINUS_CHUCK .
 * chuck_op ::= PLUS_CHUCK .
 * chuck_op ::= DIV_CHUCK .
 * chuck_op ::= MOD_CHUCK .
 * chuck_op ::= TIMES_CHUCK .
 * chuck_op ::= BIT_AND_CHUCK .
 * chuck_op ::= BIT_OR_CHUCK .
 * chuck_op ::= BIT_XOR_CHUCK .
 * chuck_op ::= BIT_NOT_CHUCK .
 * chuck_op ::= AT_CHUCK .
 * chuck_op ::= NOT_CHUCK .
 */
int token_to_chuck_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_CHUCK: return CHUCK_OP_CHUCK;
        case TOKEN_UP_CHUCK: return CHUCK_OP_UP_CHUCK;
        case TOKEN_RSHIFT_CHUCK: return CHUCK_OP_RSHIFT_CHUCK;
        case TOKEN_LSHIFT_CHUCK: return CHUCK_OP_LSHIFT_CHUCK;
        case TOKEN_MINUS_CHUCK: return CHUCK_OP_MINUS_CHUCK;
        case TOKEN_PLUS_CHUCK: return CHUCK_OP_PLUS_CHUCK;
        case TOKEN_DIV_CHUCK: return CHUCK_OP_DIV_CHUCK;
        case TOKEN_MOD_CHUCK: return CHUCK_OP_MOD_CHUCK;
        case TOKEN_TIMES_CHUCK: return CHUCK_OP_TIMES_CHUCK;
        case TOKEN_BIT_AND_CHUCK: return CHUCK_OP_BIT_AND_CHUCK;
        case TOKEN_BIT_OR_CHUCK: return CHUCK_OP_BIT_OR_CHUCK;
        case TOKEN_BIT_XOR_CHUCK: return CHUCK_OP_BIT_XOR_CHUCK;
        case TOKEN_BIT_NOT_CHUCK: return CHUCK_OP_BIT_NOT_CHUCK;
        case TOKEN_AT_CHUCK: return CHUCK_OP_AT_CHUCK;
        case TOKEN_NOT_CHUCK: return CHUCK_OP_NOT_CHUCK;
        default: return -1;
    }
    return -1;
}


/**
 * arithmetic_op ::= PLUS .
 * arithmetic_op ::= MINUS .
 * arithmetic_op ::= DIVIDE .
 * arithmetic_op ::= TIMES .
 * arithmetic_op ::= PERCENT .
 * arithmetic_op ::= POWER .
 */
int token_to_arithmetic_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_PLUS: return CHUCK_OP_PLUS;
        case TOKEN_MINUS: return CHUCK_OP_MINUS;
        case TOKEN_DIVIDE: return CHUCK_OP_DIVIDE;
        case TOKEN_TIMES: return CHUCK_OP_TIMES;
        case TOKEN_PERCENT: return CHUCK_OP_PERCENT;
        case TOKEN_POWER: return CHUCK_OP_POWER;
        default: return -1;
    }
    return -1;
}

/**
 * bitwise_op ::= BIT_OR .
 * bitwise_op ::= BIT_AND .
 * bitwise_op ::= BIT_XOR .
 * bitwise_op ::= LSHIFT .
 * bitwise_op ::= RSHIFT .
 */
int token_to_bitwise_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_BIT_OR: return CHUCK_OP_BIT_OR;
        case TOKEN_BIT_AND: return CHUCK_OP_BIT_AND;
        case TOKEN_BIT_XOR: return CHUCK_OP_BIT_XOR;
        case TOKEN_LSHIFT: return CHUCK_OP_LSHIFT;
        case TOKEN_RSHIFT: return CHUCK_OP_RSHIFT;
        default: return -1;
    }
    return -1;
}

/**
 * comparison_op ::= CMP_LE .
 * comparison_op ::= CMP_LT .
 * comparison_op ::= CMP_EQ .
 * comparison_op ::= CMP_NE .
 * comparison_op ::= CMP_GT .
 * comparison_op ::= CMP_GE .
 */
int token_to_comparison_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_CMP_LE: return CHUCK_OP_CMP_LE;
        case TOKEN_CMP_LT: return CHUCK_OP_CMP_LT;
        case TOKEN_CMP_EQ: return CHUCK_OP_CMP_EQ;
        case TOKEN_CMP_NE: return CHUCK_OP_CMP_NE;
        case TOKEN_CMP_GT: return CHUCK_OP_CMP_GT;
        case TOKEN_CMP_GE: return CHUCK_OP_CMP_GE;
        default: return -1;
    }
    return -1;
}

/**
 * logical_op ::= LOG_AND .
 * logical_op ::= LOG_OR .
 * logical_op ::= LOG_XOR .
 */
int token_to_logical_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_LOG_AND: return CHUCK_OP_LOG_AND;
        case TOKEN_LOG_OR: return CHUCK_OP_LOG_OR;
        case TOKEN_LOG_XOR: return CHUCK_OP_LOG_XOR;
        default: return -1;
    }
    return -1;
}

/**
 * unary_op ::= PLUSPLUS .
 * unary_op ::= MINUSMINUS .
 * unary_op ::= BIT_NOT .
 * unary_op ::= NOT .
 * unary_op ::= MINUS .
 */
int token_to_unary_op(ChuckTokenType tokenType)
{
    switch (tokenType)
    {
        case TOKEN_PLUSPLUS: return CHUCK_OP_PLUSPLUS;
        case TOKEN_MINUSMINUS: return CHUCK_OP_MINUSMINUS;
        case TOKEN_BIT_NOT: return CHUCK_OP_BIT_NOT;
        case TOKEN_NOT: return CHUCK_OP_NOT;
        case TOKEN_MINUS: return CHUCK_OP_MINUS;
        default: return -1;
    }
    return -1;
}

/**
 * var_decl ::= (STATIC) IDENTIFER (AT) IDENTIFER ( array_size_delcs ) ( COMMA (AT) IDENTIFER ( array_size_delcs ) ) *
 */
ChuckASTVarDeclNode *get_var_decl(ChuckParser *parser)
{
    CUString *type_name = NULL;
    expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
        type_name = get_next_token(parser, false).value.stringValue;
    });
    LUType *base_type = lu_type_unresolved(NULL, type_name);
    ChuckASTVarDeclNode *var_node = NULL;
    ChuckASTVarDeclNode *tail = NULL;

    do
    {
        LUType *var_type = base_type;
        BOOL is_reference = false;
        CUList *array_subscripts = NULL;
        CUString *var_name = NULL;
        if (peekToken(parser) == TOKEN_AT)
        {
            is_reference = true;
            var_type = lu_type_reference(var_type);
            advanceToken(parser);
        }
        expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
            var_name = get_next_token(parser, false).value.stringValue;
        });
        if (peekToken(parser) == TOKEN_LSQUARE)
        {
            advanceToken(parser);
            array_subscripts = get_array_size_decls(parser);
            var_type = lu_type_array(var_type, cu_collection_size(array_subscripts));
        }
        ChuckASTVarDeclNode *newnode = chuck_ast_variable_decl_node(var_name, var_type, NULL, is_reference, false);
        cu_setattr(newnode, var_type, var_type);
        cu_setattr(newnode, array_subscripts, array_subscripts);
        if (tail == NULL)
        {
            tail = var_node = newnode;
        }
        else
        {
            tail->next = newnode;
            tail = tail->next;
        }
        if (peekToken(parser) != TOKEN_COMMA)
            break ;
        advanceToken(parser);   // skip over the COMMA
    } while(true);
    return var_node;
}

/**
 * cast_type_declaration ::= DOLLAR type_declaration . 
 */
LUType *get_cast_type_declaration_tail(ChuckParser *parser) 
{
    if (peekToken(parser) == TOKEN_DOLLAR)
    {
        advanceToken(parser);
        return get_type_declaration(parser);
    }
    return NULL;
}

/**
 * type_declaration ::= IDENTIFIER  .
 * type_declaration ::= IDENTIFIER  AT .
 * type_declaration ::= IDENTIFIER  ( array_empty_size_decls ) .
 * type_declaration ::= IDENTIFIER  AT ( array_empty_size_decls ) .
 */
LUType *get_type_declaration(ChuckParser *parser)
{
    CUString *type_name = NULL;
    BOOL is_ref = false;
    if (peekToken(parser) != TOKEN_IDENTIFIER)
        return NULL;
    type_name = get_next_token(parser, true).value.stringValue;
    if (peekToken(parser) == TOKEN_AT)
    {
        is_ref = true;
        advanceToken(parser);
    }
    int num_dims = get_array_empty_size_decls(parser);
    LUType *base_type = lu_type_unresolved(NULL, type_name);
    if (is_ref)
        base_type = lu_type_reference(base_type);
    return lu_type_array(base_type, num_dims);
}

/**
 * array_size_decls ::= array_subscript_tail ( LSQUARE array_subscript_tail ) * .
 */
CUList *get_array_size_decls(ChuckParser *parser)
{
    CUList *out = NULL;
    while (true)
    {
        LUASTNode *array_subscript = get_array_subscript_tail(parser);
        if (!out)
            out = (CUList *)cu_linkedlist();
        cu_list_push_back(out, array_subscript);
        if (peekToken(parser) != TOKEN_LSQUARE)
            return out;
        advanceToken(parser);
    }
}

/**
 * array_empty_size_decls ::= LSQUARE RSQUARE ( LSQUARE RSQUARE ) + .
 */
int get_array_empty_size_decls(ChuckParser *parser)
{
    int ndims = 0;
    while (peekToken(parser) == TOKEN_LSQUARE)
    {
        ChuckToken secondToken = get_token(parser, 1);
        if (secondToken.tokenType != TOKEN_RSQUARE)
            return ndims;
        advanceToken(parser);
        advanceToken(parser);
        ndims++;
    }
    return ndims;
}

/**
 * array_subscript_tail ::= logical_expr RSQUARE .
 */
LUASTNode *get_array_subscript_tail(ChuckParser *parser)
{
    LUASTNode *logexpr = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RSQUARE);
    return logexpr;
}

/**
 * for_statement_tail ::= LPAREN ( initialise ) SEMICOLON 
 *                               ( condition ) SEMICOLON 
 *                               ( post_condition ) RPAREN
 *                               statement
 */
LUASTNode *get_for_statement_tail(ChuckParser *parser)
{
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *condition = NULL;
    LUASTNode *initialiser = NULL;
    LUASTNode *post_condition = NULL;
    if (peekToken(parser) != TOKEN_SEMICOLON)
        initialiser = get_chuck_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_SEMICOLON);

    if (peekToken(parser) != TOKEN_SEMICOLON)
        condition = get_chuck_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_SEMICOLON);

    if (peekToken(parser) != TOKEN_RPAREN)
        post_condition = get_chuck_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    
    LUASTNode *body = get_statement(parser);
    LUASTNode *output = lu_ast_create_loop_node(CHUCK_FOR_LOOP, initialiser, condition, post_condition, body, false, false);
    return output;
}

LUASTNode *get_while_statement_tail(ChuckParser *parser, int loopType)
{
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *condition = get_chuck_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    LUASTNode *body = get_statement(parser);
    return lu_ast_create_loop_node(loopType, NULL, condition, NULL, body, false, false);
}

/**
 * do_statement_tail ::= statement repeat_or_while_or_until LPAREN logical_expr RPAREN SEMICOLON .
 */
LUASTNode *get_do_statement_tail(ChuckParser *parser)
{
    LUASTNode *body = get_statement(parser);
    ChuckTokenType nextToken = peekToken(parser);
    if (nextToken != TOKEN_REPEAT && nextToken != TOKEN_WHILE && nextToken != TOKEN_UNTIL)
    {
        return lu_ast_create_error_node(cu_string_fmt("Expected REPEAT, WHILE or UNTIL, Received: %s", chuck_scanner_token_string(nextToken)));
    }
    advanceToken(parser);
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *condition = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    expectAndAdvanceToken(parser, TOKEN_SEMICOLON);
    int loopType = token_to_loop_type(nextToken);
    return lu_ast_create_loop_node(loopType, NULL, condition, NULL, body, false, true);
}

/**
 * class_statement ::= IDENTIFIER statement .
 * class_statement ::= IDENTIFIER EXTENDS IDENTIFIER statement .
 */
LUASTNode *get_class_statement_tail(ChuckParser *parser)
{
    CUString *class_name = NULL;
    CUString *base_class = NULL;
    expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
        class_name = get_next_token(parser, false).value.stringValue;
    });
    if (peekToken(parser) == TOKEN_EXTENDS)
    {
        advanceToken(parser);
        expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
            base_class = get_next_token(parser, false).value.stringValue;
        });
    }
    LUASTNode *body = get_statement(parser);
    return chuck_ast_create_class_node(class_name, base_class, body);
}

/**
 * function_statement ::= type_declaration IDENTIFIER LPAREN fun_param_list RPAREN statement .
 */
ChuckASTFuncDefnNode *get_function_statement_tail(ChuckParser *parser)
{
    CUString *fun_name = NULL;
    LUType *fun_type = get_type_declaration(parser);
    expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
        fun_name = get_next_token(parser, false).value.stringValue;
    });
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    CUList *params = get_fun_param_list(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    LUASTNode *body = get_statement(parser);
    return chuck_ast_create_func_defn_node(fun_type, fun_name, params, body, false); 
}

/**
 * fun_param_list(output) ::= type_declaration(param_type) IDENTIFIER(param_name)  array_subscript 
                                    ( COMMA type_declaration(param_type) IDENTIFIER(param_name) array_subscript  ) * .
 */
CUList *get_fun_param_list(ChuckParser *parser)
{
    LUType *arg_type = NULL;
    CUString *arg_name = NULL;
    CUList *arg_list = NULL;
    while (true)
    {
        if (peekToken(parser) == TOKEN_RPAREN)
            return arg_list;
        arg_type = get_type_declaration(parser);
        expectAndAdvanceToken(parser, TOKEN_IDENTIFIER, {
            arg_name = get_next_token(parser, false).value.stringValue;
        });
        if (peekToken(parser) == TOKEN_LSQUARE)
        {
            int num_dims = get_array_empty_size_decls(parser);
            arg_type = lu_type_array(arg_type, num_dims);
        }
        if (arg_list == NULL)
            arg_list = (CUList *)cu_linkedlist();
        cu_list_push_back(arg_list, arg_type);
        cu_list_push_back(arg_list, arg_name);
        if (peekToken(parser) == TOKEN_COMMA)
        {
            advanceToken(parser);
            // TODO: ensure next token is an identifier
            ChuckTokenType peekedToken = peekToken(parser);
            if (peekedToken != TOKEN_IDENTIFIER)
            {
                parser_token_expected_error(parser, TOKEN_IDENTIFIER);
            }
        }
    }
    return NULL;
}

/**
 * if_statement_tail ::= LPAREN logical_expr RPAREN  statement 
 * if_statement_tail ::= LPAREN logical_expr RPAREN  statement ELSE statement
 */
LUASTNode *get_if_statement_tail(ChuckParser *parser)
{
    expectAndAdvanceToken(parser, TOKEN_LPAREN);
    LUASTNode *condition = get_logical_expr(parser);
    expectAndAdvanceToken(parser, TOKEN_RPAREN);
    LUASTNode *ifbody = get_statement(parser);
    LUASTNode *elsebody = NULL;
    if (peekToken(parser) == TOKEN_ELSE)
    {
        advanceToken(parser);
        elsebody = get_statement(parser);
    }
    return lu_ast_create_ifelse_node(condition, ifbody, elsebody);
}

