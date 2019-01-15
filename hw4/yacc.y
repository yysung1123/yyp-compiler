%{
#include <cstdio>
#include <cstdlib>
#include "main.h"
#include <iostream>
#include <list>
#include <unordered_set>
#include <vector>
#include <deque>
#include <utility>

using namespace std;

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
int Opt_D = 1;
list<list<sym_t> > sym_table;
vector<unordered_set<string> > sym_name;
vector<sym_t> ident_list;
vector<sym_t> arg_list;
deque<sym_t> array_list;
vector<sym_t> loop_vars;
int func_decl = 0;
bool func_valid;
int array_decl = 0;
sym_t func;
bool in_func = false;
void dumpsymbol();
bool check_valid(sym_t&);
bool check_valid_loop_var(sym_t&);
void init_scope();
void destroy_scope();
string type_str(const type_t&);
string filename;
vector<int> array_ref;
vector<vector<sym_t> > param_list;
bool no_error = true;
void error(const string&);
pair<bool, sym_t> find_sym(const string&);
string name_list(const vector<sym_t>&);
sym_t check_arithmetic(const sym_t&, const sym_t&, char op);
sym_t check_mod(const sym_t&, const sym_t&);
sym_t check_cmp(const sym_t&, const sym_t&, const string&);
void check_print(const sym_t&);
void check_read(const sym_t&);
sym_t check_not(const sym_t&);
sym_t check_bool(const sym_t&, const sym_t&, const string&);
string attr_str(sym_t&);
string kind_str(int);
pair<bool, sym_t> find_sym_func(const string&);
%}

%token SEMICOLON END IDENT VAR COLON INTEGER REAL STRING BOOLEAN ARRAY TO OF COMMA INTEGER_LITERAL REAL_LITERAL STRING_LITERAL TRUE FALSE LEFT_PARENTHESES RIGHT_PARENTHESES BGN ASSIGNMENT PRINT READ LEFT_BRACKET RIGHT_BRACKET ADD MINUS MUL DIV MOD LT LE EQU GE GT NE NOT AND OR IF THEN ELSE WHILE FOR DO RETURN
%left OR
%left AND
%left NOT
%nonassoc LT LE EQU GE GT NE
%left ADD MINUS
%left MUL DIV MOD
%left unary

%%

program		:
                {
                    init_scope();
                }
                programname
                {
                    sym_t sym;
                    sym.type.type = VOID_T;
                    sym.kind = PROG_K;
                    sym.name = $2.name;
                    sym_table.back().push_back(sym);
                    if (filename != sym.name) error("program beginning ID inconsist with file name");
                }
                SEMICOLON programbody END IDENT
                {
                    dumpsymbol();
                    destroy_scope();
                    if ($7.name != $2.name) error("program end ID inconsist with the beginning ID");
                    if ($7.name != filename) error("program end ID inconsist with file name");
                }
                ;

programname	: identifier
                {
                    $$ = $1;
                }
                ;

identifier      : IDENT
                {
                    sym_t sym = $1;
                    auto& name = sym.name;
                    name.resize(min((int)name.size(), 32));
                    $$ = sym;
                }
                ;

programbody     : variable_declarations function_declarations compound_statement
                ;

variable_declarations   : variable_declaration variable_declarations
                        |
                        ;

function_declarations   : function_declaration function_declarations
                        |
                        ;

variable_declaration    : VAR identifier_list COLON type SEMICOLON
                        {
                            bool dim_error = false;
                            if ($4.type.type == ARR_T) {
                                for (auto dim : $4.type.dims) {
                                    if (dim.attr.dim_size() <= 0) {
                                        dim_error = true;
                                    }
                                }
                            }
                            if (dim_error) {
                                error(string("wrong dimension declaration for array ") + name_list(ident_list));
                            }
                            if (not ($4.type.type == ARR_T and dim_error)) {
                                for (auto& ident : ident_list) {
                                    if (not check_valid(ident)) continue;
                                    sym_t sym;
                                    sym.type = $4.type;
                                    sym.kind = VAR_K;
                                    sym.name = ident.name;
                                    sym_table.back().push_back(sym);
                                }
                            }
                            ident_list.clear();
                        }
                        | VAR identifier_list COLON sign_literal_constant SEMICOLON
                        {
                            for (auto& ident : ident_list) {
                                if (not check_valid(ident)) continue;
                                sym_t sym;
                                sym.type = $4.type;
                                sym.kind = CONST_K;
                                sym.name = ident.name;
                                sym.attr = $4.attr;
                                sym_table.back().push_back(sym);
                            }
                            ident_list.clear();
                        }
                        ;

identifier_list :
                identifier
                {
                    ident_list.push_back($1);
                }
                next_identifier
                ;

next_identifier : COMMA identifier_list
                |
                ;

type            : scalar_type { $$ = $1; }
                | { array_list.clear(); array_decl = 1; }
                array
                {
                    array_decl = 0;
                    sym_t sym;
                    sym.type.type = ARR_T;
                    sym.type.arr_type = array_list.back().type.type;
                    array_list.pop_back();
                    while (not array_list.empty()) {
                        sym.type.dims.push_back(array_list.front());
                        array_list.pop_front();
                    }
                    array_list.clear();
                    $$ = sym;
                }
                ;

scalar_type     : INTEGER { $$ = $1; if (array_decl) array_list.push_back($1); }
                | REAL { $$ = $1; if (array_decl) array_list.push_back($1); }
                | STRING { $$ = $1; if (array_decl) array_list.push_back($1); }
                | BOOLEAN { $$ = $1; if (array_decl) array_list.push_back($1); }
                ;

array           :
                ARRAY INTEGER_LITERAL TO INTEGER_LITERAL
                {
                    sym_t sym;
                    sym.attr.arr_from = $2.attr.ival;
                    sym.attr.arr_to = $4.attr.ival;
                    array_list.push_back(sym);
                }
                OF nested_array
                ;

nested_array    : scalar_type
                | array
                ;

literal_constant        : INTEGER_LITERAL
                        | REAL_LITERAL
                        | STRING_LITERAL
                        | TRUE
                        | FALSE
                        ;

sign_literal_constant   : literal_constant
                        | MINUS INTEGER_LITERAL { $$ = $2; }
                        | MINUS REAL_LITERAL { $$ = $2; }

function_declaration    :
                        identifier
                        {
                            func.attr.params.clear();
                            func.name = $1.name;
                            func.kind = FUNC_K;
                            func_valid = check_valid(func);
                        }
                        LEFT_PARENTHESES
                        {
                            func_decl = 1;
                            init_scope();
                        }
                        argument_list
                        {
                            for (auto& arg : arg_list) {
                                sym_table.back().push_back(arg);
                                func.attr.params.push_back(arg);
                            }
                            arg_list.clear();
                        }
                        RIGHT_PARENTHESES return_type
                        {
                            func.type = $8.type;
                            if (func.type.type == ARR_T) error("a function cannot return an array type");
                            if (func_valid && func.type.type != ARR_T) {
                                sym_table.front().push_back(func); // function should always be in scope level 0
                            }
                            in_func = true;
                        }
                        SEMICOLON compound_statement END identifier
                        {
                            in_func = false;
                            func.attr.params.clear();
                            if (func.name != $13.name) error("the end of the functionName mismatch");
                        }
                        ;

argument_list   : identifier_list COLON type
                {
                    bool dim_error = false;
                    if ($3.type.type == ARR_T) {
                        for (auto dim : $3.type.dims) {
                            if (dim.attr.dim_size() <= 0) {
                                dim_error = true;
                            }
                        }
                    }
                    if (dim_error) {
                        error(string("wrong dimension declaration for array ") + name_list(ident_list));
                    }
                    if (not ($3.type.type == ARR_T and dim_error)) {
                        for (auto ident : ident_list) {
                            if (not check_valid(ident)) continue;
                            sym_t sym;
                            sym.name = ident.name;
                            sym.kind = PARAM_K;
                            sym.type = $3.type;
                            arg_list.push_back(move(sym));
                        }
                    }
                    ident_list.clear();
                }
                next_argument_list
                |
                ;

next_argument_list      : SEMICOLON argument_list
                        |
                        ;

return_type     : COLON type { $$ = $2; }
                | { sym_t sym; sym.type.type = VOID_T; $$ = sym; }
                ;

compound_statement      :
                        {
                            if (func_decl != 1) init_scope();
                            func_decl = 0;
                        }
                        BGN variable_declarations statements END
                        {
                            dumpsymbol();
                            destroy_scope();
                        }
                        ;

statements      : statement statements
                |
                ;

statement       : compound_statement
                | simple_statement
                | conditional_statement
                | while_statement
                | for_statement
                | return_statement
                | function_invocation
                ;

simple_statement        : variable_reference ASSIGNMENT expression SEMICOLON
                        {
                            //cout << kind_str($1.kind) << " " << type_str($1.type) << endl;
                            if ($1.type.type != ERR_T) {
                                if ($1.kind == LOOP_VAR_K) {
                                    error(string("loop variable '") + $1.name + "' cannot be assigned");
                                } else if ($3.type.type == ERR_T) {
                                    error("error type in RHS of assignment");
                                } else {
                                    if ($1.kind == CONST_K) {
                                        error("constant '" + $1.name + "' cannot be assigned");
                                    } else if ($1.type.type == ARR_T) {
                                        //cout << "# " << $1.type.dims.size() << " " << $3.type.dims.size() << endl;
                                        if ($3.type.type == ARR_T) {
                                            if ($1.type.dims.size() != $3.type.dims.size() || $1.type.arr_type != $3.type.arr_type) {
                                                error("type mismatch, LHS= " + type_str($1.type) + ", RHS= " + type_str($3.type));
                                            } else {
                                                error("array assignment is not allowed");
                                            }
                                        } else {
                                            error("type mismatch, LHS= " + type_str($1.type) + ", RHS= " + type_str($3.type));
                                        }
                                    } else if ($1.type.type != $3.type.type) {
                                        if (not ($1.type.type == REAL_T and $3.type.type == INT_T)) {
                                            error("type mismatch, LHS= " + type_str($1.type) + ", RHS= " + type_str($3.type));
                                        }
                                    }
                                }
                            }
                        }
                        | PRINT expression SEMICOLON { check_print($2); }
                        | READ variable_reference SEMICOLON { check_read($2); }
                        ;

variable_reference      : identifier
                        {
                            bool ok;
                            sym_t sym;
                            tie(ok, sym)  = find_sym($1.name);
                            if (ok) {
                                $$ = sym;
                            } else {
                                error("'" + $1.name + "' is not declared");
                                $$.type.type = ERR_T;
                            }
                        }
                        | array_reference
                        ;

array_reference :
                identifier
                {
                    array_ref.push_back(0);
                }
                index
                {
                    bool ok;
                    sym_t sym;
                    tie(ok, sym)  = find_sym($1.name);
                    if (ok) {
                        if (array_ref.back() == sym.type.dims.size()) {
                            $$.type.type = sym.type.arr_type;
                        } else if (array_ref.back() > sym.type.dims.size()) {
                            error("'" + $1.name + "' is " + to_string(sym.type.dims.size()) + " dimension(s), but reference in " + to_string(array_ref.back()) + " dimension(s)");
                            $$.type.type = ERR_T;
                        } else {
                            $$.type = sym.type;
                            $$.arr_ref = array_ref.back();
                            for (int i = 0; i < $$.arr_ref; ++i) {
                                $$.type.dims.pop_front();
                            }
                        }
                    } else {
                        error("'" + $1.name + "' is not declared");
                        $$.type.type = ERR_T;
                    }
                    array_ref.pop_back();
                }
                ;

index           : LEFT_BRACKET expression RIGHT_BRACKET
                {
                    if ($2.type.type != INT_T) error("array index is not integer");
                    array_ref.back()++;
                }
                next_index
                ;

next_index      : index
                |
                ;

expression      : LEFT_PARENTHESES expression RIGHT_PARENTHESES { $$ = $2; }
                | MINUS expression %prec unary { $$ = $2; }
                | expression ADD expression { $$ = check_arithmetic($1, $3, '+'); }
                | expression MINUS expression { $$ = check_arithmetic($1, $3, '-'); }
                | expression MUL expression { $$ = check_arithmetic($1, $3, '*'); }
                | expression DIV expression { $$ = check_arithmetic($1, $3, '/'); }
                | expression MOD expression { $$ = check_mod($1, $3); }
                | expression LT expression { $$ = check_cmp($1, $3, ">"); }
                | expression LE expression { $$ = check_cmp($1, $3, ">="); }
                | expression EQU expression { $$ = check_cmp($1, $3, "="); }
                | expression GE expression { $$ = check_cmp($1, $3, ">="); }
                | expression GT expression { $$ = check_cmp($1, $3, ">"); }
                | expression NE expression { $$ = check_cmp($1, $3, "<>"); }
                | NOT expression { $$ = check_not($2); }
                | expression AND expression { $$ = check_bool($1, $3, "and"); }
                | expression OR expression { $$ = check_bool($1, $3, "or"); }
                | variable_reference { $$ = $1; $$.kind = RVAL_K; }
                | procedure_call { $$ = $1; $$.kind = RVAL_K; }
                | literal_constant { $$ = $1; $$.kind = RVAL_K; }
                ;

conditional_statement   : IF expression
                        {
                            if ($2.type.type != BOOL_T) error("operand of if statement is not boolean type");
                        }
                        THEN statements else_clause END IF
                        ;

else_clause     : ELSE statements
                |
                ;

while_statement : WHILE expression
                {
                    if ($2.type.type != BOOL_T) error("operand of while statement is not boolean type");
                }
                DO statements END DO
                ;

for_statement   : FOR identifier
                {
                    auto ident = $2;
                    ident.kind = LOOP_VAR_K;
                    check_valid_loop_var(ident);
                    loop_vars.push_back(ident);
                }
                ASSIGNMENT INTEGER_LITERAL TO INTEGER_LITERAL
                {
                    if ($6.attr.ival < 0 or $7.attr.ival < 0) error("lower or upper bound of loop parameter < 0");
                    else if ($6.attr.ival > $7.attr.ival) error("loop parameter's lower bound > uppper bound");
                }
                DO statements END DO
                {
                    loop_vars.pop_back();
                }
                ;

return_statement        : RETURN expression SEMICOLON
                        {
                            if (in_func) {
                                if ($2.type.type == ARR_T && func.type.type == ARR_T) {
                                    if ($2.type.arr_type != func.type.arr_type) {
                                        error("return type mismatch");
                                    } else if ($2.type.dims.size() != func.type.dims.size()) {
                                        error("return dimension number mismatch");
                                    } else {
                                        for (int i = 0; i < func.type.dims.size(); ++i) {
                                            if ($2.type.dims[i].attr.dim_size() != func.type.dims[i].attr.dim_size()) {
                                                error("size of #" + to_string(i) + "-th dimension in return statement mismatch");
                                            }
                                        }
                                    }
                                } else if ($2.type.type != func.type.type) {
                                    error("return type mismatch");
                                }
                            } else {
                                error("program cannot be returned");
                            }
                        }
                        ;

function_invocation     : procedure_call SEMICOLON
                        ;

procedure_call  : identifier
                {
                    param_list.emplace_back();
                }
                LEFT_PARENTHESES parameters RIGHT_PARENTHESES
                {
                    bool ok;
                    sym_t sym;
                    tie(ok, sym)  = find_sym_func($1.name);
                    auto& cur_param_list = param_list.back();
                    if (ok) {
                        $$.type.type = sym.type.type;
                        if (sym.attr.params.size() < cur_param_list.size()) {
                            error(string("too many arguments to function '") + sym.name + "'");
                            $$.type.type = ERR_T;
                        } else if (sym.attr.params.size() > cur_param_list.size()) {
                            error(string("too few arguments to function '") + sym.name + "'");
                            $$.type.type = ERR_T;
                        } else {
                            bool param_error = false;
                            for (int i = 0; i < sym.attr.params.size(); ++i) {
                                auto& param_l = sym.attr.params[i];
                                auto& param_r = cur_param_list[i];
                                // cout << kind_str(cur_param_list[i].kind) << " " << type_str(cur_param_list[i].type) << endl;
                                if (param_r.kind == FUNC_K || param_r.kind == PROG_K) {
                                    param_error = true;
                                } else if (param_r.type.type == ARR_T) {
                                    auto& dims_l = param_l.type.dims;
                                    auto& dims_r = param_r.type.dims;
                                    if (dims_l.size() != dims_r.size()) {
                                        param_error = true;
                                    } else if (param_l.type.arr_type != param_r.type.arr_type) {
                                        param_error = true;
                                    } else {
                                        for (int i = 0; i < dims_l.size(); ++i) {
                                            if (dims_l[i].attr.dim_size() != dims_r[i].attr.dim_size()) {
                                                param_error = true;
                                            }
                                        }
                                    }
                                } else if (cur_param_list[i].type.type != sym.attr.params[i].type.type) {
                                    if (not (cur_param_list[i].type.type == INT_T and sym.attr.params[i].type.type == REAL_T)) {
                                        param_error = true;
                                    }
                                }
                            }
                            if (param_error) {
                                $$.type.type = ERR_T;
                                error("parameter type mismatch");
                            }
                        }
                    } else {
                        error("'" + $1.name + "' is not declared");
                        $$.type.type = ERR_T;
                    }
                    param_list.pop_back();
                }
                ;

parameters      : expression
                {
                    param_list.back().push_back($1);
                }
                next_expression
                |
                ;

next_expression : COMMA expression
                {
                    param_list.back().push_back($2);
                }
                next_expression
                |
                ;

%%

int yyerror(const char *msg)
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
        fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, buf );
        fprintf( stderr, "|\n" );
        fprintf( stderr, "| Unmatched token: %s\n", yytext );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        exit(-1);
}

string type_str(const type_t& type) {
    switch (type.type) {
    case INT_T:
        return "integer";
    case REAL_T:
        return "real";
    case BOOL_T:
        return "boolean";
    case STR_T:
        return "string";
    case ARR_T:
    {
        string res;
        type_t arr;
        arr.type = type.arr_type;
        res += type_str(arr) + " ";
        for (auto& sym : type.dims) {
            res += "[" + to_string(sym.attr.arr_to - sym.attr.arr_from + 1) + "]";
        }
        return res;
    }
    case VOID_T:
        return "void";
    case ERR_T:
        return "err";
    default:
        return "";
    }
}

string kind_str(int kind) {
    switch (kind) {
    case VAR_K:
        return "variable";
    case PARAM_K:
        return "parameter";
    case CONST_K:
        return "constant";
    case FUNC_K:
        return "function";
    case PROG_K:
        return "program";
    case RVAL_K:
        return "rvalue";
    default:
        return "";
    }
}

string level_str(int level) {
    switch (level) {
    case 0:
        return "(global)";
    default:
        return "(local)";
    }
}

string attr_str(sym_t& sym) {
    if (sym.kind == FUNC_K) {
        string res;
        bool first = true;
        for (auto& param : sym.attr.params) {
            if (not first) res += ", ";
            first = false;
            res += type_str(param.type);
        }
        return res;
    }
    if (sym.kind != CONST_K) return "";
    auto& attr = sym.attr;
    switch (sym.type.type) {
    case INT_T:
        return to_string(attr.ival);
    case REAL_T:
        return to_string(attr.rval);
    case BOOL_T:
        return attr.bval ? "true" : "false";
    case STR_T:
        return "\"" + attr.sval + "\"";
    default:
        return "";
    }
}

void dumpsymbol()
{
    if (Opt_D == 0) return;
    int i;
    for(i=0;i< 110;i++) {
        printf("=");
    }
    printf("\n");
    printf("%-33s%-11s%-11s%-17s%-11s\n","Name","Kind","Level","Type","Attribute");
    for(i=0;i< 110;i++) {
        printf("-");
    }
    printf("\n");
/*
    printf("%-33s", "func");
    printf("%-11s", "function");
    printf("%d%-10s", 0,"(global)");
    printf("%-17s", "boolean");
    printf("%-11s", "integer, real [2][3]");
    printf("\n");
*/
    for (auto& sym : sym_table.back()) {
        printf("%-33s", sym.name.c_str());
        printf("%-11s", kind_str(sym.kind).c_str());
        printf("%d%-10s", sym_table.size() - 1, level_str(sym_table.size() - 1).c_str());
        printf("%-17s", type_str(sym.type).c_str());
        // printf("%-11s", attr_str(sym).c_str());
        printf("%s", attr_str(sym).c_str());
        printf("\n");
    }
    for(i=0;i< 110;i++) {
        printf("-");
    }
    printf("\n");
}

bool check_loop_var(const string& name) {
    for (auto& loop_var : loop_vars) {
        if (name == loop_var.name) return true;
    }
    return false;
}

void error_msg(const string& name) {
    no_error = false;
    cout << "<Error> found in Line " << linenum << ": symbol " << name << " is redeclared" << endl;
}

bool check_valid(sym_t& sym) {
    auto& name = sym.name;
    if (sym_name.back().find(name) != sym_name.back().end() || check_loop_var(name)) {
      error_msg(name);
        return false;
    } else {
        sym_name.back().insert(name);
        return true;
    }
}

bool check_valid_loop_var(sym_t& sym) {
    auto& name = sym.name;
    if (check_loop_var(name)) {
        error_msg(name);
        return false;
    }
    return true;
}

void init_scope()
{
    sym_table.emplace_back();
    sym_name.emplace_back();
}

void destroy_scope()
{
    sym_table.pop_back();
    sym_name.pop_back();
}

void error(const string& msg)
{
    no_error = false;
    cout << "<Error> found in Line " << linenum << ": " << msg << endl;
}

pair<bool, sym_t> find_sym(const string& name)
{
    for (auto& sym : loop_vars) {
        if (sym.name == name) return make_pair(true, sym);
    }
    for (auto it = sym_table.rbegin(); it != sym_table.rend(); ++it) {
        for (auto& sym : *it) {
            if (sym.name == name) return make_pair(true, sym);
        }
    }
    return make_pair(false, sym_t());
}

pair<bool, sym_t> find_sym_func(const string& name)
{
    for (auto& sym : sym_table.front()) {
        if (sym.kind == FUNC_K and sym.name == name) return make_pair(true, sym);
    }
    return make_pair(false, sym_t());
}
string name_list(const vector<sym_t>& sym_list)
{
    string res;
    bool first = true;
    for (auto& sym : sym_list) {
        if (!first) res += ", ";
        first = false;
        res += sym.name;
    }
    return res;
}

sym_t check_arithmetic(const sym_t& l, const sym_t& r, char op)
{
    sym_t sym;
    sym.kind = RVAL_K;
    if (not l.check_expr() or not r.check_expr()) {
        if (not l.check_expr()) error(string("error in left operand of '") + op + "' operator");
        if (not r.check_expr()) error(string("error in right operand of '") + op + "' operator");
        sym.type.type = ERR_T;
    } else if (l.type.type == ARR_T or r.type.type == ARR_T) {
        error(string("one of the operands of operator '") + op + "' is array type");
        sym.type.type = ERR_T;
    } else if (l.type.type == r.type.type) {
        if (op != '+' && l.type.type == STR_T) {
            error(string("one of the operands of operator '") + op + "' is string type");
            sym.type.type = ERR_T;
        } else {
            sym.type.type = l.type.type;
        }
    } else if ((l.type.type == REAL_T and r.type.type == INT_T) or
               (l.type.type == INT_T and r.type.type == REAL_T)) {
        sym.type.type = REAL_T;
    } else {
        error(string("operands of operator ") + op + " are not both integer or both real");
        sym.type.type = ERR_T;
    }
    return sym;
}

sym_t check_mod(const sym_t& l, const sym_t& r)
{
    sym_t sym;
    sym.kind = RVAL_K;
    if (not l.check_expr() or not r.check_expr()) {
        if (not l.check_expr()) error("error in left operand of 'mod' operator");
        if (not r.check_expr()) error("error in right operand of 'mod' operator");
        sym.type.type = ERR_T;
    } else if (l.type.type == INT_T && r.type.type == INT_T) {
        sym.type.type = INT_T;
    } else {
        error("one of the operands of operator 'mod' is not integer");
        sym.type.type = ERR_T;
    }
    return sym;
}

sym_t check_cmp(const sym_t& l, const sym_t& r, const string& rel_op)
{
    sym_t sym;
    sym.kind = RVAL_K;
    if (not l.check_expr() or not r.check_expr()) {
        if (not l.check_expr()) error(string("error in left operand of '") + rel_op + "' operator");
        if (not r.check_expr()) error(string("error in right operand of '") + rel_op + "' operator");
        sym.type.type = ERR_T;
    } else if ((l.type.type == INT_T and r.type.type == INT_T) or
               (l.type.type == REAL_T and r.type.type == REAL_T)) {
        sym.type.type = BOOL_T;
    } else {
        error(string("operands of operator '") + rel_op + "' are not both integer or both real");
        sym.type.type = ERR_T;
    }
    return sym;
}

void check_print(const sym_t& sym)
{
    if (not sym.check_expr()) error("operand of print statement can't be printed");
}

void check_read(const sym_t& sym)
{
    if (not sym.check_expr()) error("operand of read statement can't be saved");
}

sym_t check_not(const sym_t& s)
{
    sym_t sym;
    sym.kind = RVAL_K;
    if (s.check_expr_kind() and s.type.type == BOOL_T) {
        sym.type.type = BOOL_T;
    } else {
        error("one of the operands of operator 'not' is not boolean");
        sym.type.type = ERR_T;
    }
    return sym;
}

sym_t check_bool(const sym_t& l, const sym_t& r, const string& bool_op)
{
    sym_t sym;
    sym.kind = RVAL_K;
    if (l.check_expr_kind() and l.type.type == BOOL_T and
        r.check_expr_kind() and r.type.type == BOOL_T) {
        sym.type.type = BOOL_T;
    } else {
        error("one of the operands of operator '" + bool_op + "' is not boolean");
        sym.type.type = ERR_T;
    }
    return sym;
}

int  main( int argc, char **argv )
{
    if( argc != 2 ) {
        fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
        exit(0);
    }

    FILE *fp = fopen( argv[1], "r" );
    filename = string(argv[1]);
    filename = filename.substr(0, filename.find('.'));

    if( fp == NULL )  {
        fprintf( stdout, "Open  file  error\n" );
        exit(-1);
    }

    yyin = fp;
    yyparse();

    if (no_error) {
        fprintf( stdout, "\n" );
        fprintf( stdout, "|---------------------------------------------|\n" );
        fprintf( stdout, "|  There is no syntactic and semantic error!  |\n" );
        fprintf( stdout, "|---------------------------------------------|\n" );
    }
    exit(0);
}
