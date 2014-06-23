
#ifndef __LCHUCK_TOKEN_H__
#define __LCHUCK_TOKEN_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

typedef enum 
{
     TOKEN_EOI = 1,
     TOKEN_ERROR,
     TOKEN_LTLTLT,
     TOKEN_GTGTGT,
     TOKEN_SEMICOLON,
     TOKEN_RETURN,
     TOKEN_BREAK,
     TOKEN_CONTINUE,
     TOKEN_LBRACE,
     TOKEN_RBRACE,
     TOKEN_LPAREN,
     TOKEN_RPAREN,
     TOKEN_CONST,
     TOKEN_REPEAT,
     TOKEN_WHILE,
     TOKEN_UNTIL,
     TOKEN_FOR,
     TOKEN_IF,
     TOKEN_ELSE,
     TOKEN_DO,
     TOKEN_UN_CHUCK,
     TOKEN_SPORK,
     TOKEN_TILDE,
     TOKEN_IDENTIFIER,
     TOKEN_AT,
     TOKEN_STATIC,
     TOKEN_PUBLIC,
     TOKEN_DOLLAR,
     TOKEN_LSQUARE,
     TOKEN_RSQUARE,
     TOKEN_PLUSPLUS,
     TOKEN_MINUSMINUS,
     TOKEN_BIT_NOT,
     TOKEN_NOT,
     TOKEN_MINUS,
     TOKEN_COLONCOLON,
     TOKEN_DOT,
     TOKEN_NEW,
     TOKEN_LONG,
     TOKEN_DOUBLE,
     TOKEN_STRING,
     TOKEN_HASH,
     TOKEN_COMMA,
     TOKEN_PERCENT,
     TOKEN_CLASS,
     TOKEN_EXTENDS,
     TOKEN_FUN,
     TOKEN_CHUCK,
     TOKEN_UP_CHUCK,
     TOKEN_RSHIFT_CHUCK,
     TOKEN_LSHIFT_CHUCK,
     TOKEN_MINUS_CHUCK,
     TOKEN_PLUS_CHUCK,
     TOKEN_DIV_CHUCK,
     TOKEN_MOD_CHUCK,
     TOKEN_TIMES_CHUCK,
     TOKEN_BIT_AND_CHUCK,
     TOKEN_BIT_OR_CHUCK,
     TOKEN_BIT_XOR_CHUCK,
     TOKEN_BIT_NOT_CHUCK,
     TOKEN_AT_CHUCK,
     TOKEN_NOT_CHUCK,
     TOKEN_PLUS,
     TOKEN_DIVIDE,
     TOKEN_TIMES,
     TOKEN_POWER,
     TOKEN_BIT_OR,
     TOKEN_BIT_AND,
     TOKEN_BIT_XOR,
     TOKEN_LSHIFT,
     TOKEN_RSHIFT,
     TOKEN_CMP_LE,
     TOKEN_CMP_LT,
     TOKEN_CMP_EQ,
     TOKEN_CMP_NE,
     TOKEN_CMP_GT,
     TOKEN_CMP_GE,
     TOKEN_LOG_AND,
     TOKEN_LOG_OR,
     TOKEN_LOG_XOR,
} ChuckTokenType;

struct ChuckToken
{
    ChuckTokenType tokenType;
    union
    {
        long longValue;
        double doubleValue;
        CUString *stringValue;
    } value;
};

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

#endif
