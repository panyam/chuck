
#ifndef __CHUCK_CODEGEN_H__
#define __CHUCK_CODEGEN_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "chuck_fwddefs.h"

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

/**
 * Generates LLVM IR code for a given node.
 */
extern Value *chuck_codegen(LUASTNode *node);

#endif

