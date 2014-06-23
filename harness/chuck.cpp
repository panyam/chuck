
#include "chuck.h"
#include <stdlib.h>
#include <iostream>
#include <string.h>

using namespace std;

void test_tokenizer(int argc, char *argv[]);
void test_parser(int argc, char *argv[]);
void usage();

void usage()
{
    cout << "Usage: chuck command options files" << endl;
    cout << "   Commands:" << endl;
    cout << "   help        Show help." << endl;
    cout << "   scan        Test scanner on given input files." << endl;
    cout << "   parse       Test parser on given input files." << endl;
    cout << "   compile     Generate byte code for given input files." << endl;
    exit(0);
}

int main(int argc, char *argv[])
{
    if (argc < 2 || strcasecmp(argv[1], "help") == 0)
    {
        usage();
    }
    else if (strcasecmp(argv[1], "scan") == 0)
    {
        test_tokenizer(argc - 2, argv + 2);
    }
    else if (strcasecmp(argv[1], "parse") == 0)
    {
        test_parser(argc - 2, argv + 2);
    }
    return 0;
}

BOOL tokenize_stream(CUIStream *instream, int index, const char *path, BOOL stop_on_error)
{
    printf("%3d Tokenizing: %s\n", index, path ? path : "stdin");
    int t;
    ChuckScanner *scanner = chuck_scanner_create(instream);
    while((t = chuck_scanner_scan(scanner)) != TOKEN_EOI){
        printf("%d\t%s\t%.*s\n", t, chuck_scanner_token_string(t),
                    (int)(scanner->cur - scanner->tok), scanner->tok);
        if ((t == TOKEN_ERROR) && stop_on_error)
        {
            printf("Token Error in: %s\n", path ? path : "stdin");
            assert(false && "Found error token. Stopping");
            return FALSE;
        }
    }
    printf("Successfully tokenized: %s\n", path ? path : "stdin");
    chuck_scanner_destroy(scanner);
    return TRUE;
}

void test_tokenizer(int argc, char *argv[])
{
    BOOL stop_on_error = TRUE;
    if (argc == 0)
    {
        CUIStream *instream = cu_fileistream_create_with_stream(stdin);
        tokenize_stream(instream, 1, NULL, stop_on_error);
    }
    for (int i = 0;i < argc;i++)
    {
        CUIStream *instream = cu_fileistream_create(argv[i]);
        tokenize_stream(instream, i + 1, argv[i], stop_on_error);
    }
}

BOOL parse_stream(CUIStream *instream, int index, const char *path, BOOL stop_on_error)
{
    printf("%3d Parsing: %s\n", index, path ? path : "stdin");
    LUASTNode *parseTree = chuck_parse_stream(instream);
    cu_decref(parseTree);
    return TRUE;
}

void test_parser(int argc, char *argv[])
{
    BOOL stop_on_error = TRUE;
    if (argc == 0)
    {
        CUIStream *instream = cu_fileistream_create_with_stream(stdin);
        parse_stream(instream, 1, NULL, stop_on_error);
    }
    for (int i = 0;i < argc;i++)
    {
        CUIStream *instream = cu_fileistream_create(argv[i]);
        parse_stream(instream, i + 1, argv[i], stop_on_error);
    }
}

void test_compiler(int argc, char *argv[])
{
}

