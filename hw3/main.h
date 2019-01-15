#pragma once

#include <list>
#include <string>

using std::list;
using std::string;

extern "C"
{
    int yyerror(const char *msg);
    extern int yylex(void);
}

enum types {
    INT_T, REAL_T, BOOL_T, STR_T, ARR_T, VOID_T
};

enum kind {
    VAR_K, PARAM_K, CONST_K, FUNC_K, PROG_K
};

struct sym_t;
struct sym_attr_t;
struct type_t;

struct type_t {
    int type;
    int arr_type;
    list<sym_t> dims;
};

struct sym_attr_t {
    int ival;
    double rval;
    bool bval;
    string sval;
    int arr_from, arr_to;
    list<sym_t> params;
};

struct sym_t {
    sym_attr_t attr;
    type_t type;
    int kind;
    string name;
};

#define YYSTYPE sym_t
