
#ifndef __CHUCK_PUBLIC_H__
#define __CHUCK_PUBLIC_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "chuck_fwddefs.h"
#include "tokens.h"
#include "parser.h"
#include "scanner.h"

enum {
    CHUCK_AST_COMPLEX_NODE,
    CHUCK_AST_VAR_DECL_NODE,
    CHUCK_AST_NEW_NODE,
    CHUCK_AST_PRINT_NODE,
    CHUCK_AST_SPORK_NODE,
    CHUCK_AST_TIME_NODE,
    CHUCK_AST_FUNC_DEFN_NODE,
    CHUCK_AST_CLASS_DEFN_NODE,
};

enum {
    CHUCK_FUNC_STATIC   =   1 << 0,
    CHUCK_FUNC_CLASS    =   1 << 1,
    CHUCK_FUNC_PUBLIC   =   1 << 2,
    CHUCK_FUNC_PRIVATE  =   1 << 3,
};

LU_DEFINE_AST_NODE(ChuckASTVarDeclNode,
    CUString *var_name;         // name of the variable
    LUType *var_type;           // variable type including array dimensions
    CUList *array_subscripts;   // only used for initialisation
    BOOL is_reference;          // is this a reference or value
    BOOL is_static;
    ChuckASTVarDeclNode *next;
);

LU_DEFINE_AST_NODE(ChuckASTNewNode, 
    LUType *type;
);

LU_DEFINE_AST_NODE(ChuckASTPrintNode, 
    CUList *param_list;
);

LU_DEFINE_AST_NODE(ChuckASTSporkNode, 
    LUASTNode *value;
);

LU_DEFINE_AST_NODE(ChuckASTTimeNode, 
    LUASTNode *source;
    CUString *units;
);

LU_DEFINE_AST_NODE(ChuckASTFuncDefnNode, 
    LUType *return_type;    // Return type
    CUString *name;         // name of the function
    CUList *params;         // params
    LUASTNode *body;        // function body
    BOOL is_static;
);

LU_DEFINE_AST_NODE(ChuckASTClassDefnNode, 
    CUString *name;             // name of the class
    CUString *parent_name;      // name of the parent
    BOOL is_public;
    LUASTNode *body;
);

LU_DEFINE_AST_NODE(ChuckASTComplexNode, 
    BOOL is_polar;
    LUASTNode *x;
    LUASTNode *y;
);

extern LUASTNode *chuck_ast_create_complex_node(LUASTNode *x, LUASTNode *y, BOOL is_polar);
extern LUASTNode *chuck_ast_create_class_node(CUString *name, CUString *parent_name, LUASTNode *body);
extern LUASTNode *chuck_ast_create_new_node(LUType *type);
extern LUASTNode *chuck_ast_create_spork_node(LUASTNode *node);
extern LUASTNode *chuck_ast_create_time_node(LUASTNode *source, CUString *units);
extern ChuckASTFuncDefnNode *chuck_ast_create_func_defn_node(LUType *return_type,
                                                  CUString *func_name,
                                                  CUList *params,
                                                  LUASTNode *body,
                                                  BOOL is_static);
extern ChuckASTVarDeclNode *chuck_ast_variable_decl_node(CUString *var_name,
                                                         LUType *var_type,
                                                         LUASTNode *array_subscript_decls,
                                                         BOOL is_reference,
                                                         BOOL is_static);
extern LUASTNode *chuck_ast_create_print_node(CUList *param_list);

extern LUASTNodePrototype *chuck_ast_complex_node_prototype();
extern LUASTNodePrototype *chuck_ast_new_node_prototype();
extern LUASTNodePrototype *chuck_ast_spork_node_prototype();
extern LUASTNodePrototype *chuck_ast_time_node_prototype();
extern LUASTNodePrototype *chuck_ast_print_node_prototype();
extern LUASTNodePrototype *chuck_ast_var_decl_node_prototype();
extern LUASTNodePrototype *chuck_ast_class_defn_node_prototype();
extern LUASTNodePrototype *chuck_ast_func_defn_node_prototype();

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

#endif

