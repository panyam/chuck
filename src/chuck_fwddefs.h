
#ifndef __LCHUCK_FWDDEFS_H__
#define __LCHUCK_FWDDEFS_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#include <langutils.h>

#include "chuck_operators.h"

typedef enum {
    CHUCK_FOR_LOOP,
    CHUCK_WHILE_LOOP,
    CHUCK_UNTIL_LOOP,
    CHUCK_REPEAT_LOOP
} ChuckLoopType;


typedef struct ChuckToken ChuckToken;
typedef struct ChuckScanner ChuckScanner;
typedef struct ChuckParser ChuckParser;

typedef struct ChuckASTComplexNode ChuckASTComplexNode;
typedef struct ChuckASTVarDeclNode ChuckASTVarDeclNode;
typedef struct ChuckASTNewNode ChuckASTNewNode;
typedef struct ChuckASTClassDefnNode ChuckASTClassDefnNode;
typedef struct ChuckASTSporkNode ChuckASTSporkNode;
typedef struct ChuckASTTimeNode ChuckASTTimeNode;
typedef struct ChuckASTPrintNode ChuckASTPrintNode;
typedef struct ChuckASTFuncDefnNode ChuckASTFuncDefnNode;

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

#endif

