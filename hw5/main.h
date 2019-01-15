#pragma once

#include <list>
#include <string>
#include <deque>

using std::list;
using std::string;
using std::deque;

extern "C"
{
    int yyerror(const char *msg);
    extern int yylex(void);
}

enum types {
    INT_T, REAL_T, BOOL_T, STR_T, ARR_T, VOID_T, ERR_T
};

enum kind {
    VAR_K, PARAM_K, CONST_K, FUNC_K, PROG_K, RVAL_K, LOOP_VAR_K
};

enum cond {
    LT_C, LE_C, EQ_C, GE_C, GT_C, NEQ_C
};

struct sym_t;
struct sym_attr_t;
struct type_t;

struct type_t {
    int type;
    int arr_type;
    deque<sym_t> dims;
    bool is_scalar_type() const {
        return type == INT_T || type == REAL_T || type == BOOL_T || type == STR_T;
    }
};

struct sym_attr_t {
    int ival;
    double rval;
    bool bval;
    string sval;
    int arr_from, arr_to;
    deque<sym_t> params;
    int dim_size() {
        return arr_to - arr_from;
    }
};

struct sym_t {
    sym_attr_t attr;
    type_t type;
    list<string> code;
    int kind;
    string name;
    bool expr_invalid = false;
    int arr_ref = 0;
    int var_num;
    int cond;
    bool check_expr_kind() const {
        return kind == CONST_K or kind == VAR_K or kind == RVAL_K or kind == PARAM_K;
    }
    bool check_expr() const {
        return check_expr_kind() and type.is_scalar_type();
    }
};

#define YYSTYPE sym_t
