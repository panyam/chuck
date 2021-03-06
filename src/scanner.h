

#ifndef __CHUCK_SCANNER_H__
#define __CHUCK_SCANNER_H__

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#include "chuck_fwddefs.h"

// Refine this based on how many tokens are generated by the parser.
#define MAX_TOKENS  200

struct ChuckScanner
{
    CUIStream *instream;
    // Start of the input buffer
    uchar *bot;
    // "End" of the memory occupied by input buffer, 
    // ie bot + totalBufferCapacity
    uchar *top;

    // This is the pointer to the "last" byte in the buffer - this may not necessarily be "top"
    // ie if the buffer size is 1024 and only 512 bytes have been read so far, top would be at 1024
    // and lim would be at 512
    uchar *lim;

    // The pointer in the buffer where the current token starts
    uchar *tok;
    // The current char that the scanner is pointing to?
    uchar *cur;
    uchar *ptr;
    uchar *pos;

    // eof is *only* set and points to "lim" if no more data is available in the input
    uchar *eof;

    uint line;
    ChuckToken currToken;
};

extern ChuckScanner *chuck_scanner_create(CUIStream *instream);
extern void chuck_scanner_destroy(ChuckScanner *scanner);
extern int chuck_scanner_scan(ChuckScanner *scanner);
extern const char *chuck_scanner_token_string(int token);

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

#endif

