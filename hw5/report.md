compiler hw5
==

## Code Generation
### Initialization
- [x] A P program is translated into a Java class.
- [x] a method main that is declared public and static must be generated for the compound statement in the program body

### Declarations for Variables and Constants
- [x] Global variables will be modeled as  elds of classes in Java assembly language. Fields will be declared right after class name declaration. Each global variable will be declared as a static field by the form
```
.field public static <field-name> <type descriptor>
```
- [x] Local variables in P will be translated into local variables of methods in Java assembly.

### Store Constants in Symbol Table
- [x] Constant variables in P will not be transformed into  elds or local variables in Java assembly. The values of constant variables will be stored in symbol tables.

### Expressions and Statements
- [x] An expression can be either an variable, a constant variable, an arithmetic expression, or a boolean expression.
- [x] Variables
- [x] Constants

- [x] Boolean expressions with relational operators and integer operands will be modeled by a subtraction instruction followed by a conditional jump.
```
isub  ; a and b are at stack top
iflt L1
iconst_0  ; false = 0
goto L2
L1:
iconst_1  ; true = 1
L2:
```
- [x] Boolean expressions with relational operators and  oating-point operands will be modeled by a  oating-point comparison instruction (fcmpl) followed by a conditional jump.
```
fcmpl  ; a and b are at stack top
iflt L1
iconst_0  ; false = 0
goto L2
L1:
iconst_1  ; true = 1
L2:
```
- [x] Assignments
- [x] PRINT Statements
- [x] READ Statements

### If Statements and While/For Loops
- [x] If Statements 
- [x] For Statements
- [x] While Statements
### Procedure Declaration and Invocation
- [x] Procedure Declaration
- [x] Procedure Invocation