%{
#include <cstdio>
#include <cstdlib>
#include "main.h"
#include <iostream>
#include <list>
#include <unordered_set>
#include <vector>
#include <deque>

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
vector<string> loop_vars;
int func_decl = 0;
bool func_valid;
int array_decl = 0;
sym_t func;
void dumpsymbol();
bool check_valid(sym_t&);
bool check_valid_loop_var(sym_t&);
void init_scope();
void destroy_scope();
string type_str(const type_t&);
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

program		    :
                {
                    init_scope();
                }
                programname
                {
                    sym_t sym;
                    sym.type.type = VOID_T;
                    sym.kind = PROG_K;
                    sym.name = $2.name;
                    sym_table.back().push_back(move(sym));
                }
                SEMICOLON programbody END IDENT
                {
                    dumpsymbol();
                    destroy_scope();
                }
    		    ;

programname	    : identifier
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

programbody     :
                variable_declarations function_declarations compound_statement
                ;

variable_declarations   : variable_declaration variable_declarations
                        |
                        ;

function_declarations   : function_declaration function_declarations
                        |
                        ;

variable_declaration    : VAR identifier_list COLON type SEMICOLON
                        {
                            for (auto& ident : ident_list) {
                                if (not check_valid(ident)) continue;
                                sym_t sym;
                                sym.type = $4.type;
                                sym.kind = VAR_K;
                                sym.name = ident.name;
                                sym_table.back().push_back(move(sym));
                            }
                            ident_list.clear();
                        }
                        | VAR identifier_list COLON literal_constant SEMICOLON
                        {
                            for (auto& ident : ident_list) {
                                if (not check_valid(ident)) continue;
                                sym_t sym;
                                sym.type = $4.type;
                                sym.kind = CONST_K;
                                sym.name = ident.name;
                                sym.attr = $4.attr;
                                sym_table.back().push_back(move(sym));
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

literal_constant        : INTEGER_LITERAL { $$ = $1; }
                        | REAL_LITERAL { $$ = $1; }
                        | STRING_LITERAL { $$ = $1; }
                        | TRUE { $$ = $1; }
                        | FALSE { $$ = $1; }
                        ;

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
                        RIGHT_PARENTHESES return_type SEMICOLON compound_statement END identifier
                        {
                            func.type = $8.type;
                            if (func_valid) {
                                sym_table.back().push_back(func);
                            }
                            func.attr.params.clear();
                        }
                        ;

argument_list   : identifier_list COLON type
                {
                    for (auto ident : ident_list) {
                        if (not check_valid(ident)) continue;
                        sym_t sym;
                        sym.name = ident.name;
                        sym.kind = PARAM_K;
                        sym.type = $3.type;
                        arg_list.push_back(move(sym));
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
                        | PRINT expression SEMICOLON
                        | READ variable_reference SEMICOLON
                        ;

variable_reference      : identifier
                        | array_reference
                        ;

array_reference : identifier index
                ;

index           : LEFT_BRACKET expression RIGHT_BRACKET next_index 
                ;

next_index      : index
                |
                ;

expression      : LEFT_PARENTHESES expression RIGHT_PARENTHESES 
                | MINUS expression %prec unary
                | expression ADD expression
                | expression MINUS expression
                | expression MUL expression
                | expression DIV expression
                | expression MOD expression
                | expression LT expression
                | expression LE expression
                | expression EQU expression
                | expression GE expression
                | expression GT expression
                | expression NE expression
                | NOT expression
                | expression AND expression
                | expression OR expression
                | variable_reference
                | procedure_call
                | literal_constant
                ;

conditional_statement   : IF expression THEN statements ELSE statements END IF
                        | IF expression THEN statements END IF
                        ;

while_statement : WHILE expression DO statements END DO
                ;

for_statement   : FOR identifier
                {
                    auto ident = $2;
                    check_valid_loop_var(ident);
                    loop_vars.push_back(ident.name);
                }
                ASSIGNMENT INTEGER_LITERAL TO INTEGER_LITERAL DO statements END DO
                {
                    loop_vars.pop_back();
                }
                ;

return_statement        : RETURN expression SEMICOLON
                        ;

function_invocation     : procedure_call SEMICOLON
                        ;

procedure_call  : identifier LEFT_PARENTHESES parameters RIGHT_PARENTHESES
                ;

parameters      : expression next_expression
                |
                ;

next_expression : COMMA expression next_expression
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
    for (auto& lv_name : loop_vars) {
        if (name == lv_name) return true;
    }
    return false;
}

void error_msg(const string& name) {
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

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	/*
	fprintf( stdout, "\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	fprintf( stdout, "|  There is no syntactic error!  |\n" );
	fprintf( stdout, "|--------------------------------|\n" );
	*/
	exit(0);
}
