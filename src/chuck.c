
#include "chuck.h"

#define MAKE_NODE(struct_type, node_type, proto)   ((struct_type *)lu_ast_create_node(sizeof(struct_type), node_type, proto))

LUASTNode *chuck_ast_create_complex_node(LUASTNode *x, LUASTNode *y, BOOL is_polar)
{
    ChuckASTComplexNode *node = MAKE_NODE(ChuckASTComplexNode, CHUCK_AST_COMPLEX_NODE, chuck_ast_complex_node_prototype());
    node->is_polar = is_polar;
    cu_setattr(node, x, x);
    cu_setattr(node, y, y);
    return (LUASTNode *)node;
}

LUASTNode *chuck_ast_create_new_node(LUType *type)
{
    ChuckASTNewNode *node = MAKE_NODE(ChuckASTNewNode, CHUCK_AST_NEW_NODE, chuck_ast_new_node_prototype());
    cu_setattr(node, type, type);
    return (LUASTNode *)node;
}

LUASTNode *chuck_ast_create_spork_node(LUASTNode *value)
{
    ChuckASTSporkNode *node = MAKE_NODE(ChuckASTSporkNode, CHUCK_AST_SPORK_NODE, chuck_ast_spork_node_prototype());
    cu_setattr(node, value, value);
    return (LUASTNode *)node;
}

LUASTNode *chuck_ast_create_time_node(LUASTNode *source, CUString *units)
{
    ChuckASTTimeNode *node = MAKE_NODE(ChuckASTTimeNode, CHUCK_AST_TIME_NODE, chuck_ast_time_node_prototype());
    cu_setattr(node, source, source);
    cu_setattr(node, units, units);
    return (LUASTNode *)node;
}

LUASTNode *chuck_ast_create_print_node(CUList *param_list)
{
    ChuckASTPrintNode *node = MAKE_NODE(ChuckASTPrintNode, CHUCK_AST_PRINT_NODE, chuck_ast_print_node_prototype());
    cu_setattr(node, param_list, param_list);
    return (LUASTNode *)node;
}

ChuckASTFuncDefnNode *chuck_ast_create_func_defn_node(LUType *return_type,
                                           CUString *name,
                                           CUList *params,
                                           LUASTNode *body,
                                           BOOL is_static)
{
    ChuckASTFuncDefnNode *node = MAKE_NODE(ChuckASTFuncDefnNode, CHUCK_AST_FUNC_DEFN_NODE, chuck_ast_func_defn_node_prototype());
    node->is_static = is_static;
    cu_setattr(node, return_type, return_type);
    cu_setattr(node, name, name);
    cu_setattr(node, params, params);
    cu_setattr(node, body, body);
    return node;
}

ChuckASTVarDeclNode *chuck_ast_variable_decl_node(CUString *var_name,
                                                  LUType *var_type,
                                                  LUASTNode *array_subscripts,
                                                  BOOL is_reference,
                                                  BOOL is_static)
{
    ChuckASTVarDeclNode *node = MAKE_NODE(ChuckASTVarDeclNode, CHUCK_AST_VAR_DECL_NODE, chuck_ast_var_decl_node_prototype());
    cu_setattr(node, var_name, var_name);
    cu_setattr(node, var_type, var_type);
    cu_setattr(node, array_subscripts, array_subscripts);
    node->is_static = is_static;
    node->is_reference = is_reference;
    return node;
}

LUASTNode *chuck_ast_create_class_node(CUString *name,
                                       CUString *parent_name,
                                       LUASTNode *body)
{
    ChuckASTClassDefnNode *node = MAKE_NODE(ChuckASTClassDefnNode, CHUCK_AST_CLASS_DEFN_NODE, chuck_ast_class_defn_node_prototype());
    cu_setattr(node, name, name);
    cu_setattr(node, parent_name, parent_name);
    cu_setattr(node, body, body);
    return (LUASTNode *)node;
}

DECLARE_PROTO_FUNC(chuck_ast_complex_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_new_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_spork_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_time_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_print_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_var_decl_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_class_defn_node_prototype, LUASTNodePrototype, NULL);
DECLARE_PROTO_FUNC(chuck_ast_func_defn_node_prototype, LUASTNodePrototype, NULL);
