
#ifndef __CHUCK_PARSER_H__
#define __CHUCK_PARSER_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "chuck_fwddefs.h"

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

/**
 * Parses a chuck source file and returns an AST node corresponding to the file.
 */
extern LUASTNode *chuck_parse_stream(CUIStream *instream);

#endif

