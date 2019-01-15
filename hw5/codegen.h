#pragma once

#include <list>
#include <iostream>
#include "main.h"

using namespace std;

string code_init(const string& filename) {
    return ".class public " + filename + "\n"
      ".super java/lang/Object\n"
      ".field public static _sc Ljava/util/Scanner;\n";
}

string code_main(const string& filename) {
    return ".method public static main([Ljava/lang/String;)V\n"
        ".limit stack 100\n"
        ".limit locals 100\n"
        "new java/util/Scanner\n"
        "dup\n"
        "getstatic java/lang/System/in Ljava/io/InputStream;\n"
        "invokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n"
        "putstatic " + filename + "/_sc Ljava/util/Scanner;\n";
}

string code_end() {
    return "return\n"
      ".end method\n";
}


void code_inspect(const list<string>& code) {
    for (auto& str : code) {
        cout << str << endl;
    }
}

string code_gen(list<string>& code) {
    string res;
    for (auto& str: code) {
        res += str + "\n";
    }
    return res;
}

string code_type(const type_t& type) {
    int t = type.type;
    string type_desc;
    if (t == ARR_T) {
        for (int i = 0; i < type.dims.size(); ++i) {
            type_desc += "[";
        }
        t = type.arr_type;
    }
    switch (t) {
    case INT_T:
        type_desc += "I";
        break;
    case REAL_T:
        type_desc += "F";
        break;
    case BOOL_T:
        type_desc += "Z";
        break;
    case STR_T:
        type_desc += "Ljava/lang/String";
        break;
    case VOID_T:
        type_desc += "V";
    default:
        break;
    }
    return type_desc;
}

string code_global_var(const sym_t& sym) {
    list<string> res;
    string type_desc, value, code;
    type_desc = code_type(sym.type);
    code = ".field public static " + sym.name + " " + type_desc;
    /*
    auto& attr = sym.attr;
    if (sym.kind == CONST_K) {
        switch (sym.type.type) {
        case INT_T:
            value = to_string(attr.ival);
            break;
        case REAL_T:
            value = to_string(attr.rval);
            break;
        case BOOL_T:
            value = attr.bval ? "true" : "false";
            break;
        case STR_T:
            value = attr.sval;
            break;
        default:
            break;
        }
        code += " " + value;
    }
    */
    return code;
}

string function(const sym_t& func) {
    string decl = func.name + "(";
    for (auto& param : func.attr.params) {
        decl += code_type(param.type);
    }
    decl += ")" + code_type(func.type);
    return decl;
}

string code_function(const sym_t& func) {
    string res, decl, func_decl, end;
    end = "return\n.end method\n";
    decl += ".method public static ";
    decl += function(func) + "\n";
    res += decl;
    res += ".limit stack 100\n"; // TODO
    res += ".limit locals 100\n"; // TODO
    for (auto& code : func.code) {
        res += code;
        if (code.back() != '\n') res += "\n";
    }
    res += end;
    return res;
}

string code_literal_constant(const sym_t& sym) {
    string code;
    switch (sym.type.type) {
    case INT_T:
        code = "ldc " + to_string(sym.attr.ival);
        break;
    case REAL_T:
        code = "ldc " + to_string(sym.attr.rval);
        break;
    case BOOL_T:
        code = sym.attr.bval ? "iconst_1" : "iconst_0";
        break;
    case STR_T:
        code = "ldc \"" + sym.attr.sval + "\"";
    default:
        break;
    }
    return code;
}

list<string> code_arithmetic(const sym_t& res, sym_t& l, sym_t& r, char op) {
    list<string> code;
    code.splice(code.end(), l.code);
    if (res.type.type == REAL_T and l.type.type == INT_T) {
        code.push_back("i2f");
    }
    code.splice(code.end(), r.code);
    if (res.type.type == REAL_T and r.type.type == INT_T) {
        code.push_back("i2f");
    }
    if (res.type.type == INT_T) {
        switch (op) {
        case '+':
            code.push_back("iadd");
            break;
        case '-':
            code.push_back("isub");
            break;
        case '*':
            code.push_back("imul");
            break;
        case '/':
            code.push_back("idiv");
            break;
        }
    } else if (res.type.type == REAL_T) {
        switch (op) {
        case '+':
            code.push_back("fadd");
            break;
        case '-':
            code.push_back("fsub");
            break;
        case '*':
            code.push_back("fmul");
            break;
        case '/':
            code.push_back("fdiv");
            break;
        }
    }
    return code;
}

list<string> code_mod(const sym_t& res, sym_t& l, sym_t& r) {
    list<string> code;
    code.splice(code.end(), l.code);
    code.splice(code.end(), r.code);
    code.push_back("irem");
    return code;
}

list<string> code_cmp(sym_t& l, sym_t r, const string& rel_op, int label_num) {
    list<string> code;
    code.splice(code.end(), l.code);
    code.splice(code.end(), r.code);
    if (l.type.type == INT_T) {
        code.push_back("isub");
    } else if (l.type.type == REAL_T) {
        code.push_back("fcmpl");
    }
    if (rel_op == "<") {
        code.push_back("iflt Ltrue_" + to_string(label_num));
    } else if (rel_op == "<=") {
        code.push_back("ifle Ltrue_" + to_string(label_num));
    } else if (rel_op == "=") {
        code.push_back("ifeq Ltrue_" + to_string(label_num));
    } else if (rel_op == ">=") {
        code.push_back("ifge Ltrue_" + to_string(label_num));
    } else if (rel_op == ">") {
        code.push_back("ifgt Ltrue_" + to_string(label_num));
    } else if (rel_op == "<>") {
        code.push_back("ifne Ltrue_" + to_string(label_num));
    }
    code.push_back("iconst_0");
    code.push_back("goto Lfalse_" + to_string(label_num));
    code.push_back("Ltrue_" + to_string(label_num) + ":");
    code.push_back("iconst_1");
    code.push_back("Lfalse_" + to_string(label_num) + ":");
    return code;
}

void code_neg(sym_t& res) {
    if (res.type.type == INT_T) {
        res.code.push_back("ineg"); 
    } else if (res.type.type == REAL_T) {
        res.code.push_back("fneg");
    }
}

list<string> code_not(sym_t& sym) {
    list<string> code;
    code.splice(code.end(), sym.code);
    code.push_back("iconst_1");
    code.push_back("ixor");
    return code;
}

list<string> code_bool(sym_t& l, sym_t& r, const string& bool_op) {
    list<string> code;
    code.splice(code.end(), l.code);
    code.splice(code.end(), r.code);
    if (bool_op == "and") {
        code.push_back("iand");
    } else if (bool_op == "or") {
        code.push_back("ior");
    }
    return code;
}

list<string> code_var_ref(const sym_t& var, int var_num) {
    list<string> code;
    if (var.kind == VAR_K or var.kind == PARAM_K or var.kind == LOOP_VAR_K) {
        if (var.type.type == INT_T or var.type.type == BOOL_T) {
            code.push_back("iload " + to_string(var_num));
        } else if (var.type.type == REAL_T) {
            code.push_back("fload " + to_string(var_num));
        }
    } else if (var.kind == CONST_K) {
        code.push_back(code_literal_constant(var));
    }
    return code;
}

list<string> code_var_ref_global(const sym_t& var, const string& filename) {
    list<string> code;
    if (var.type.type == INT_T) {
        code.push_back("getstatic " + filename + "/" + var.name + " I");
    } else if (var.type.type == REAL_T) {
        code.push_back("getstatic " + filename + "/" + var.name + " F");
    } else if (var.type.type == BOOL_T) {
        code.push_back("getstatic " + filename + "/" + var.name + " Z");
    }
    return code;
}

void code_proc_call(sym_t& proc, const string& filename) {
    proc.code.push_back("invokestatic " + filename + "/" + function(proc));
}

list<string> code_return(sym_t& expr) {
    list<string> code;
    code.splice(code.end(), expr.code);
    if (expr.type.type == INT_T or expr.type.type == BOOL_T) {
        code.push_back("ireturn");
    } else if (expr.type.type == REAL_T) {
        code.push_back("freturn");
    }
    return code;
}

list<string> code_statements(sym_t& lstat, sym_t& rstat) {
    list<string> code;
    code.splice(code.end(), lstat.code);
    code.splice(code.end(), rstat.code);
    return code;
}

list<string> code_read(const sym_t& var, const string& filename, bool is_global) {
    list<string> code;
    code.push_back("getstatic " + filename + "/_sc Ljava/util/Scanner;"); 
    if (var.type.type == INT_T) {
        code.push_back("invokevirtual java/util/Scanner/nextInt()I");
        if (is_global) {
            code.push_back("putstatic " + filename + "/" + var.name + " I");
        } else {
            code.push_back("istore " + to_string(var.var_num));
        }
    } else if (var.type.type == REAL_T) {
        code.push_back("invokevirtual java/util/Scanner/nextFloat()F");
        if (is_global) {
            code.push_back("putstatic " + filename + "/" + var.name + " F");
        } else {
            code.push_back("fstore " + to_string(var.var_num));
        }
    } else if (var.type.type == BOOL_T) {
        code.push_back("invokevirtual java/util/Scanner/nextBoolean()Z");
        if (is_global) {
            code.push_back("putstatic " + filename + "/" + var.name + " Z");
        } else {
            code.push_back("istore " + to_string(var.var_num));
        }
    }
    return code;
}

list<string> code_print(sym_t& expr) {
    list<string> code;
    code.push_back("getstatic java/lang/System/out Ljava/io/PrintStream;");
    code.splice(code.end(), expr.code);
    if (expr.type.type == INT_T) {
        code.push_back("invokevirtual java/io/PrintStream/print(I)V");
    } else if (expr.type.type == REAL_T) {
        code.push_back("invokevirtual java/io/PrintStream/print(F)V");
    } else if (expr.type.type == BOOL_T) {
        code.push_back("invokevirtual java/io/PrintStream/print(Z)V");
    } else if (expr.type.type == STR_T) {
        code.push_back("invokevirtual java/io/PrintStream/print(Ljava/lang/String;)V");
    }
    return code;
}

list<string> code_assignment(const sym_t& var, sym_t& expr, int var_num) {
    list<string> code;
    code.splice(code.end(), expr.code);
    if (var.type.type == INT_T or var.type.type == BOOL_T) {
        code.push_back("istore " + to_string(var_num));
    } else if (var.type.type == REAL_T) {
        if (expr.type.type == INT_T) {
            code.push_back("i2f");
        }
        code.push_back("fstore " + to_string(var_num));
    }
    return code;
}

list<string> code_assignment_global(const sym_t& var, sym_t& expr, const string& filename) {
    list<string> code;
    code.splice(code.end(), expr.code);
    if (var.type.type == INT_T) {
        code.push_back("putstatic " + filename + "/" + var.name + " I");
    } else if (var.type.type == REAL_T) {
        if (expr.type.type == INT_T) {
            code.push_back("i2f");
        }
        code.push_back("putstatic " + filename + "/" + var.name + " F");
    } else if (var.type.type == BOOL_T) {
        code.push_back("putstatic " + filename + "/" + var.name + " Z");
    }
    return code;
}

list<string> code_if(sym_t& cond_expr, sym_t& true_stmts, sym_t& else_stmts, int branch_num) {
    list<string> code;
    code.splice(code.end(), cond_expr.code);
    code.push_back("ifeq Lelse_" + to_string(branch_num));
    code.splice(code.end(), true_stmts.code);
    code.push_back("goto Lexit_" + to_string(branch_num));
    code.push_back("Lelse_" + to_string(branch_num) + ":");
    code.splice(code.end(), else_stmts.code);
    code.push_back("Lexit_" + to_string(branch_num) + ":");
    return code;
}

list<string> code_for(int loop_var_num, int label_num, int low, int high, sym_t& stmts) {
    list<string> code;
    code.push_back("ldc " + to_string(low));
    code.push_back("istore " + to_string(loop_var_num));
    code.push_back("Lbegin_" + to_string(label_num) + ":");
    code.push_back("iload " + to_string(loop_var_num));
    code.push_back("ldc " + to_string(high));
    code.push_back("isub");
    code.push_back("ifle Ltrue_" + to_string(label_num));
    code.push_back("goto Lexit_" + to_string(label_num));
    code.push_back("Ltrue_" + to_string(label_num) + ":");
    code.splice(code.end(), stmts.code);
    code.push_back("iload " + to_string(loop_var_num));
    code.push_back("sipush 1");
    code.push_back("iadd");
    code.push_back("istore " + to_string(loop_var_num));
    code.push_back("goto Lbegin_" + to_string(label_num));
    code.push_back("Lexit_" + to_string(label_num) + ":");
    return code;
}

list<string> code_while(sym_t& expr, sym_t& stmts, int label_num) {
    list<string> code;
    code.push_back("Lbegin_" + to_string(label_num) + ":");
    code.splice(code.end(), expr.code);
    code.push_back("ifeq Lexit_" + to_string(label_num));
    code.splice(code.end(), stmts.code);
    code.push_back("goto Lbegin_" + to_string(label_num));
    code.push_back("Lexit_" + to_string(label_num) + ":");
    return code;
}
