Compiler Project Assignment 4
==

## Semantic definitions
### Program 
- [x] The identifier after the end of a program declaration must be the same identifier as the name given
:::danger
error message: program beginning ID inconsist with file name
:::
- [x] Program has no return value, thus any return statement appeared in the main block of program is not legal.
:::danger
error message: can't return in non-function block
:::

### Function
- [x] The identifier after the end of a function declaration must be the same identifier as the name given at the beginning of the declaration.
:::danger
error message: function end ID inconsist with the beginning ID
:::
- [x] The parameter passing mechanism is call-by-value.
- [x] Parameters could be a scalar or array type, but the return value could only be a scalar type.
:::danger
error message: a function cannot return an array type
:::
- [x] The type of the return statement inside the function must be the same as the return type of the function declaration.
:::danger
error message: return type mismatch
:::


### Variable/Constant Declarations and References
- [x] Assignments to constants are not allowed, and constants cannot be declared in terms of other named constants.
:::danger
error message: constant 'symbol' can't be assign
:::
- [x] In an array declaration, the index of the lower bound must be smaller than that of the upper bound. Both of the indexes must be greater than or equal to zero.
:::danger
error message:
array lower bound greater then upper bound
array boundary should both greater than 0
:::

### Variable References, Array References and Expressions
- [x] A variable reference is either an identifier reference or an array reference.
- [x] Each index of array references must be an integer. Bounds checking is not performed at compile time as in C language.
:::danger
error message: array address should be integer
:::
- [x] Two arrays are considered to be the same type if they have the same number of elements — more specifically, they have the same number of dimensions and the same size for each dimension — and the types of elements are the same.
:::danger
error message: 'symbol' N dimension(s), but reference in M dimension(s)
:::
- [x] Array types can be used in parameter passing. However, array arithmetic is not allowed. Notice that a function cannot return an array type, but it can return an element of an array. For example,
```
foo(): integer;
begin
    var a: array 1 to 3 of array 1 to 3 of integer;
    var b: array 1 to 5 of array 1 to 3 of integer;
    var i, j: integer;
    a[1][1] := i; // legal
    i := a[1][1] + j; // legal
    a[1][1] := b[1][2]; // legal
    a := b; // illegal: array arithmetic
    a[1] := b[2]; // illegal: array arithmetic
    return a[1][1]; // legal: ‘a[1][1]’ is a scalar type, but ‘a’ is an array type.
end
end foo
```

- [x] For an arithmetic operotor (+, -, *, or /), the operands must be integer or real types, and the operation produces an integer or real value. The type of the operands of an operation may be different. Please see Section 2.4 for more details.
:::danger
error message: imcompatible type
:::
- [x] For a mod operator, the operands must be integer types, and it produces an integer value.
:::danger
error message: imcompatible type
:::
- [x] For a Boolean operator (and, or, or not), the operands must be Boolean types, and the operation produces only Boolean value.
:::danger
error message: imcompatible type
:::
- [x] For a relational operator (<, <=, =, >=, >, or <>), the operands must be integer or real types, and the operation produces only Boolean value. Operands must be of the same type.
:::danger
error message: imcompatible type
:::
- [x] String operands can only appear in“+” operations (string concatenations), assignment statements, print statements and read statements. Notice that when doing string concatenation, both operands
must be string types.
## Type Coercion
- [x] An integer type can be implicit converted into a real type due to several situations, such as 
    - [x] assignments, 
    - [x] parameter passing, 
    - [x] arithmetic expressions.
- [x] The result of an arithmetic operation will be real type if at least one of the operands is real type. For example, 1.2+1, 3-1.2, and 1.2*3.4 are all result in real type.
## Statements
There are seven distinct types of statements: compound, simple, conditional, while, for, return, and
procedure call.
### compound
- [x] A compound statement forms an inner scope. Note that declarations inside a compound statement are local to the statements in the block and no longer exist after the block is exited.
### simple
- [x] Variable references of print or read statements must be scalar type.
- [x] In assignment statements, the type of the left-hand side must be the same as that of the right-hand side unless type coercion is permitted.
### conditional and while
- [x] The conditional expression part of conditional and while statements must be Boolean types.
### for
- [x] A counting iterative control statement has a variable, called the loop variable, in which the count value is maintained. The scope of the loop variable is the range of the loop. The variable is implicitly declared at the for statement and implicitly undeclared after loop termination.
:::danger
error message: loop-var 'symbol' can't be assign
:::
- [x] The value of the loop variable cannot be changed inside the loop.
- [x] In a nested loop, each loop must maintain a distinct loop variable.
- [x] The loop parameters used to compute an iteration count must be in the incremental order and must be greater than or equal to zero. For example,
```
for i := 3 to 5 do
// statements
end do
3
is a legal for statement, but
for i := 5 to 3 do
// statements
end do
is illegal.
```
### function invocation
- [x] A procedure is a function that has no return value.
- [x] The types of the actual parameters and the return value must be identcial to the types of the formal parameters and the return type in the function declaration.

## The platform to run the parser

OS : ArchLinux 4.14.4 (linux1.cs.nctu.edu.tw Linux1 Workstation)

## How to run the parser

```
make all
./parser [filename]
```