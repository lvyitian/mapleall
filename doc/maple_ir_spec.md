#
# Copyright (c) [2020] Huawei Technologies Co.,Ltd.All rights reserved.
#
# OpenArkCompiler is licensed under the Mulan PSL v1.
# You can use this software according to the terms and conditions of the Mulan PSL v1.
# You may obtain a copy of Mulan PSL v1 at:
#
#     http://license.coscl.org.cn/MulanPSL
#
# THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY KIND, EITHER
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO NON-INFRINGEMENT, MERCHANTABILITY OR
# FIT FOR A PARTICULAR PURPOSE.
# See the Mulan PSL v1 for more details.
#

MAPLE IR Specification
======================

Contents {#contents .TOC-Heading}
========

[1 Introduction 7](#introduction)

[2 Program Representation in MAPLE IR
8](#program-representation-in-maple-ir)

[2.1 Symbol Tables 9](#symbol-tables)

[2.2 Primitive Types 9](#primitive-types)

[2.3 Constants 10](#constants)

[2.4 Identifiers (\$ % &) 10](#identifiers)

[2.5 Pseudo-registers (%) 11](#pseudo-registers)

[2.6 Special Registers (%%) 11](#special-registers)

[2.7 Statement Labels (@) 11](#statement-labels)

[2.8 Storage Accesses 11](#storage-accesses)

[2.9 Aggregates 12](#aggregates)

[2.9.1 Arrays 12](#arrays)

[2.9.2 Structures 12](#structures)

[3 Instruction Specification 13](#instruction-specification)

[3.1 Storage Access Instructions 13](#storage-access-instructions)

[3.1.1 dassign 13](#dassign)

[3.1.2 dread 14](#dread)

[3.1.3 iassign 14](#iassign)

[3.1.4 iread 14](#iread)

[3.1.5 iassignoff 14](#iassignoff)

[3.1.6 iassignfpoff 14](#iassignfpoff)

[3.1.7 iassignpcoff 14](#iassignpcoff)

[3.1.8 ireadoff 15](#ireadoff)

[3.1.9 ireadfpoff 15](#ireadfpoff)

[3.1.10 ireadpcoff 15](#ireadpcoff)

[3.1.11 regassign 15](#regassign)

[3.1.12 regread 15](#regread)

[3.2 Leaf Opcodes 15](#leaf-opcodes)

[3.2.1 addrof 15](#addrof)

[3.2.2 addroflabel 16](#addroflabel)

[3.2.3 addroffunc 16](#addroffunc)

[3.2.4 addroffpc 16](#addroffpc)

[3.2.5 conststr 16](#conststr)

[3.2.6 conststr16 16](#conststr16)

[3.2.7 constval 16](#constval)

[3.2.8 sizeoftype 16](#sizeoftype)

[3.3 Unary Expression Opcodes 16](#unary-expression-opcodes)

[3.3.1 abs 16](#abs)

[3.3.2 bnot 17](#bnot)

[3.3.3 extractbits 17](#extractbits)

[3.3.4 iaddrof 17](#iaddrof)

[3.3.5 lnot 17](#lnot)

[3.3.6 neg 17](#neg)

[3.3.7 recip 17](#recip)

[3.3.8 sext 17](#sext)

[3.3.9 sqrt 17](#sqrt)

[3.3.10 zext 17](#zext)

[3.4 Type Conversion Expression Opcodes
18](#type-conversion-expression-opcodes)

[3.4.1 ceil 18](#ceil)

[3.4.2 cvt 18](#cvt)

[3.4.3 floor 18](#floor)

[3.4.4 Retype 18](#retype)

[3.4.5 round 18](#round)

[3.4.6 trunc 18](#trunc)

[3.5 Binary Expression Opcodes 19](#binary-expression-opcodes)

[3.5.1 add 19](#add)

[3.5.2 ashr 19](#ashr)

[3.5.3 band 19](#band)

[3.5.4 bior 19](#bior)

[3.5.5 bxor 19](#bxor)

[3.5.6 cand 19](#cand)

[3.5.7 cior 19](#cior)

[3.5.8 cmp 19](#cmp)

[3.5.9 cmpg 19](#cmpg)

[3.5.10 cmpl 20](#cmpl)

[3.5.11 depositbits 20](#depositbits)

[3.5.12 div 20](#div)

[3.5.13 eq 20](#eq)

[3.5.14 ge 20](#ge)

[3.5.15 gt 20](#gt)

[3.5.16 land 20](#land)

[3.5.17 lior 20](#lior)

[3.5.18 le 20](#le)

[3.5.19 lshr 20](#lshr)

[3.5.20 lt 21](#lt)

[3.5.21 max 21](#max)

[3.5.22 min 21](#min)

[3.5.23 mul 21](#mul)

[3.5.24 ne 21](#ne)

[3.5.25 rem 21](#rem)

[3.5.26 shl 21](#shl)

[3.5.27 sub 21](#sub)

[3.6 Ternary Expression Opcodes 21](#ternary-expression-opcodes)

[3.6.1 select 21](#select)

[3.7 N-ary Expression Opcodes 22](#n-ary-expression-opcodes)

[3.7.1 array 22](#array)

[3.7.2 intrinsicop 22](#intrinsicop)

[3.7.3 intrinsicopwithtype 22](#intrinsicopwithtype)

[3.8 Control Flow Statements 22](#control-flow-statements)

[3.8.1 Hierarchical control flow statements
22](#hierarchical-control-flow-statements)

[3.8.1.1 doloop 23](#doloop)

[3.8.1.2 dowhile 23](#dowhile)

[3.8.1.3 foreachelem 23](#foreachelem)

[3.8.1.4 if 23](#if)

[3.8.1.5 while 23](#while)

[3.8.2 Flat control flow statements 24](#flat-control-flow-statements)

[3.8.2.1 brfalse 24](#brfalse)

[3.8.2.2 brtrue 24](#brtrue)

[3.8.2.3 goto 24](#goto)

[3.8.2.4 multiway 24](#multiway)

[3.8.2.5 return 24](#return)

[3.8.2.6 switch 24](#switch)

[3.8.2.7 rangegoto 25](#rangegoto)

[3.8.2.8 indexgoto 25](#indexgoto)

[3.9 Call Statements 25](#call-statements)

[3.9.1 call 25](#call)

[3.9.2 callinstant 26](#callinstant)

[3.9.3 icall 26](#icall)

[3.9.4 intrinsiccall 26](#intrinsiccall)

[3.9.5 intrinsiccallwithtype 26](#intrinsiccallwithtype)

[3.9.6 xintrinsiccall 26](#xintrinsiccall)

[3.10 Java Call Statements 26](#java-call-statements)

[3.10.1 virtualcall 26](#virtualcall)

[3.10.2 superclasscall 26](#superclasscall)

[3.10.3 interfacecall 26](#interfacecall)

[3.11 Calls with Return Values Assigned
27](#calls-with-return-values-assigned)

[3.11.1 callassigned 27](#callassigned)

[3.12 Exceptions Handling 27](#exceptions-handling)

[3.12.1 jstry 28](#jstry)

[3.12.2 javatry 28](#javatry)

[3.12.3 cpptry 29](#cpptry)

[3.12.4 throw 29](#throw)

[3.12.5 jscatch 29](#jscatch)

[3.12.6 javacatch 29](#javacatch)

[3.12.7 cppcatch 29](#cppcatch)

[3.12.8 finally 29](#finally)

[3.12.9 cleanuptry 30](#cleanuptry)

[3.12.10 endtry 30](#endtry)

[3.12.11 gosub 30](#gosub)

[3.12.12 retsub 30](#retsub)

[3.13 Memory Allocation and Deallocation
30](#memory-allocation-and-deallocation)

[3.13.1 alloca 30](#alloca)

[3.13.2 decref 31](#decref)

[3.13.3 decrefreset 31](#decrefreset)

[3.13.4 free 31](#free)

[3.13.5 gcmalloc 31](#gcmalloc)

[3.13.6 gcmallocjarray 31](#gcmallocjarray)

[3.13.7 gcpermalloc 31](#gcpermalloc)

[3.13.8 incref 32](#incref)

[3.13.9 malloc 32](#malloc)

[3.13.10 stackmalloc 32](#stackmalloc)

[3.13.11 stackmallocjarray 32](#stackmallocjarray)

[3.14 Other Statements 32](#other-statements)

[3.14.1 assertge 32](#assertge)

[3.14.2 assertlt 32](#assertlt)

[3.14.3 assertnonnull 32](#assertnonnull)

[3.14.4 eval 33](#eval)

[3.14.5 checkpoint 33](#checkpoint)

[3.14.6 membaracquire 33](#membaracquire)

[3.14.7 membarrelease 33](#membarrelease)

[3.14.8 membarfull 33](#membarfull)

[3.14.9 syncenter 33](#syncenter)

[3.14.10 syncexit 33](#syncexit)

[4 Declaration Specification 34](#declaration-specification)

[4.1 Module Declaration 34](#module-declaration)

[4.1.1 entryfunc 34](#entryfunc)

[4.1.2 flavor 34](#flavor)

[4.1.3 globalmemmap 34](#globalmemmap)

[4.1.4 globalmemsize 34](#globalmemsize)

[4.1.5 globalwordstypetagged 34](#globalwordstypetagged)

[4.1.6 globalwordsrefcounted 35](#globalwordsrefcounted)

[4.1.7 id 35](#id)

[4.1.8 import 35](#import)

[4.1.9 importpath 35](#importpath)

[4.1.10 numfuncs 35](#numfuncs)

[4.1.11 srclang 35](#srclang)

[4.2 Variable Declaration 35](#variable-declaration)

[4.3 Pseudo-register Declarations 36](#pseudo-register-declarations)

[4.4 Type Specification 36](#type-specification)

[4.4.1 Incomplete Type Specification 37](#incomplete-type-specification)

[4.5 Type Declaration 37](#type-declaration)

[4.6 Java Class and Interface Declaration
38](#java-class-and-interface-declaration)

[4.7 Function Declaration 39](#function-declaration)

[4.7.1 funcsize 39](#funcsize)

[4.7.2 framesize 39](#framesize)

[4.7.3 moduleid 39](#moduleid)

[4.7.4 upformalsize 39](#upformalsize)

[4.7.5 formalwordstypetagged 40](#formalwordstypetagged)

[4.7.6 formalwordsrefcounted 40](#formalwordsrefcounted)

[4.7.7 localwordstypetagged 40](#localwordstypetagged)

[4.7.8 localwordsrefcounted 40](#localwordsrefcounted)

[4.8 Initializations 40](#initializations)

[4.9 Type Parameters 41](#type-parameters)

Introduction
============

MAPLE IR is an internal program representation to support program
compilation, analysis, optimization and execution. The name MAPLE comes
from the acronym "Multiple Architecture and Programming Language
Environment". Because information in the source program may be useful
for program analysis and optimization, MAPLE IR aims to provide
information about the source program that is as complete as possible.

Program information is represented in two parts: the declaration part
for defining the program constructs and the execution part specifying
the program code. The former is often collectively referred to as the
symbol table, though there can be different types of tables.

MAPLE IR is target-independent. It is not pre-disposed towards any
specific target processor or processor characteristic.

MAPLE IR can be regarded as the ISA of a virtual machine (VM). The MAPLE
VM can be regarded as a general purpose processor that takes MAPLE IR as
input and directly executes the program portion of the MAPLE IR.

MAPLE IR is the common representation for programs compiled from
different programming languages, which include general purpose languages
like C, C++, Java and Javascript. MAPLE IR is extensible. As additional
languages, including domain-specific languages, are compiled into MAPLE
IR, more constructs will be added to represent constructs unique to each
language.

The compilation process can be viewed as a gradual lowering of the
program representation from high level human perceivable form to low
level machine executable form. MAPLE IR is capable of supporting all
known program analyses and optimizations, owing to its flexibility of
being able to represent program code at multiple semantic levels. At the
highest level, MAPLE IR exhibits more variety of program constructs,
corresponding to abstract language operations or constructs unique to
the language it is translated from. As a result, the code sequences at
the higher levels are shorter. Language or domain specific optimizations
are best performed at the higher levels. At the higher levels, there are
also constructs that are hierarchical in nature, like nested blocks and
expression trees. Nearly all program information is retained at the
highest level.

As compilation proceeds, MAPLE IR is gradually lowered so that the
granularity of its operations corresponds more closely to the
instructions of a general purpose processor. The code sequences become
longer as many high-level constructs are disallowed. At the same time,
the program constructs become less hierarchical. It is at the lower
levels where general purpose optimizations are performed. In particular,
at the lowest level, MAPLE IR instructions map nearly one-to-one to the
machine instructions of the mainstream processor ISAs. This is where the
effects of optimizations at the IR level are maximized, as each
eliminated operation will have the corresponding effect on the target
machine. At the lowest level, all operations, including type conversion
operations, are explicitly expressed at the IR level so that they can be
optimized by the compiler. The lowest level of MAPLE IR is also its
executable form, where the program structure is flat to allow the
sequential form of program execution. Expression trees are laid out in
prefix form and they are evaluated by the execution engine using the
stack machine model.

Program Representation in MAPLE IR
==================================

The internal representation of MAPLE IR consists of tables for the
declaration part and tree nodes for the execution part. In the execution
part, each tree node represents one MAPLE IR instruction, and is just
large enough to store the instruction's contents. Externally, MAPLE IR
can exist in either binary or ASCII formats. The binary format is a
straightforward dump of the byte contents of the internal program data
structures. The ASCII form is editable, which implies that it is
possible to program in MAPLE IR directly. The ASCII form of MAPLE IR has
a layout similar to the C language. Declarations are followed by
executable code. Expressions are displayed in in-fix notation, using
parentheses to explicitly indicate the nesting relationships.

The language front-end compiles a source file into a MAPLE IR file.
Thus, each MAPLE IR file corresponds to a compilation unit, referred to
as a *module*. A module is made up of declarations at the global scope.
Among the declarations are functions. Inside each function are
declarations at the local scope followed by the executable code of the
function.

There are three kinds of executable nodes in MAPLE IR:

1.  Leaf nodes - Also called terminal nodes, these nodes denote a value
    at execution time, which may be a constant or the value of a storage
    unit.

2.  Expression nodes - An expression node performs an operation on its
    operands to compute a result. Its result is a function of the values
    of its operands and nothing else. Each operand can be either a leaf
    node or another expression node. Expression nodes are the internal
    nodes of expression trees.

3.  Statement nodes - These represent the flow of control. Execution
    starts at the entry of the function and continues sequentially
    statement by statement until a control flow statement is executed.
    Apart from modifying control flow, statements can also modify data
    storage in the program. A statement node has operands that can be
    leaf, expression or statement nodes.

In all the executable nodes, the opcode field specifies the operation of
the node, followed by additional field specification relevant to the
opcode. The operands for the node are specified inside parentheses
separated by commas. The general form is:

```
        opcode fields (opnd0, opnd1, opnd2)
```

For example, the C statement \"a = b\" is translated to the **dassign**
node that assigns the right hand side operand b to a .

```
        dassign $a (dread i32 $b)
```

In the declaration part, each declaration is a statement. Each
declaration or execution statement must start a new line, and each line
cannot contain more than one statement node. A statement node can occupy
more than one line. The character \'\#\' can appear anywhere to indicate
that the contents from the \'\#\' to the end of the line are comments.

Symbol Tables
-------------

Program information that is of declarative nature is stored in the
symbol table portion of the IR. Having the executable instructions refer
to the symbol tables reduces the amount of information that needs to be
stored in the executable instructions. For each declaration scope, there
is a main table called the Symbol Table that manages all the
user-defined names in that scope. This implies one global Symbol Table
and a local Symbol Table for each function declared in the file. The
various types of symbol entries correspond to the kinds of constructs
that can be assigned names, including:

1.  Types

2.  Variables

3.  Functions (either prototypes or with function bodies)

In the ASCII format, the IR instructions refer to the various symbols by
their names. In the binary representation, the symbols are referred to
via their table indices..

Primitive Types
---------------

Primitive types can be regarded as pre-defined types supported by the
execution engine such that they can be directly operated on. They also
play a part in conveying the semantics of operations, as addresses are
distinct from unsigned integers. The number in the primitive type name
indicates the storage size in bits.

The primitive types are:

```
      no type - void
      signed integers - i8, i16, i32, i64
      unsigned integers - u8, u16, u32, u64
      booleans- u1
      addresses - ptr, ref, a32, a64
      floating point numbers - f32, f64
      complex numbers - c64, c128
      aggregates - agg
      javascript types:
        dynany
        dynu32
        dyni32
        dynundef
        dynnull
        dynhole
        dynbool
        dynptr
        dynf64
        dynf32
        dynstr
        dynob
      SIMD types -- v2i64, v4i32, v8i16, v16i8, v2f64, v4f32
      unknown
```

An instruction that produces or operates on values must specify the
primitive type in the instruction, as the type is not necessarily
implied by the opcode. There is the distinction between result type and
operand type. Result type can be regarded as the type of the value as it
resides in a machine register, because arithmetic operations in the
mainstream processor architectures are mostly register-based. When an
instruction only specifies a single primitive type, the type specified
applies to both the operands and the result. In the case of instructions
where the operand and result types may differ, the primitive type
specified is the result type, and a second field is added to specify the
operand type.

Some opcodes are applicable to non-primitive (or derived) types, as in
an aggregate assignment. When the type is derived, agg can be used. In
such cases, the data size can be found by looking up the type of the
symbol .

The primitive types *ptr* and *ref* are the target-independent types for
addresses. *ref* conveys the additional semantics that the address is a
reference to a run-time managed block of memory or object in the heap.
Uses of ptr or ref instead of a32 or a64 allow the IR to be independent
of the target machine by not manifesting the size of addresses until the
later target-dependent compilation phases.

The primitive type unknown is used by the language front-end when the
type of a field in an object has not been fully resolved because the
full definition resides in a different compilation unit.

Constants
---------

Constants in MAPLE IR are always of one of the primitive types.

Integer and address (pointer) types can be specified in decimal or in
hexadecimal using the 0x prefix.

Floating point types can be specified in hexadecimal or as floating
point literal as in standard C.

Single characters enclosed in single quotes can be used for i8 and u8
constants.

String literals are enclosed in double quotes.

For the complex and SIMD types, the group of values are enclosed in
square brackets separated by commas.

Identifiers (\$ % &)
--------------------

In ASCII MAPLE IR, standalone identifier names are regarded as keywords
of the MAPLE IR language. To refer to entries in the symbol tables,
identifier names must be prefixed.

**\$** - Identifiers prefixed with \'\$\' are global variables and will
be looked up in the global Symbol Table.

**%** - Identifiers prefixed with \'%\' are local variables and will be
looked up in the local Symbol Table.

**&** - Identifiers prefixed with \'&\' are function or method names and
will be looked up in the Functions Table. The major purpose of these
prefixes is to avoid name clash with the keywords (opcode names, etc.)
in the IR.

Pseudo-registers (%)
--------------------

Pseudo-registers can be regarded as local variables of a primitive type
whose addresses are never taken. They can be declared implicitly by
their appearances. The primitive type associated with a pseudo-register
is sticky.

Because pseudo-registers can only be created to store primitive types,
the use of field IDs does not apply to them. Pseudo-registers are
referred to by the \'%\' prefix followed by a number. This distinguishes
them from other local variables that are not pseudo-registers, as their
names cannot start with a number.

The compiler will promote variables to pseudo-registers. To avoid the
loss of high level type information when a variable is promoted to
pseudo-registers, reg declarations are used to provide the type
information associated with the pseudo-registers.

Special Registers (%%)
----------------------

Special registers are registers with special meaning. They are all
specified using %% as prefix. **%%SP** is the stack pointer and **%%FP**
the frame pointer in referencing the stack frame of the current
function. **%%GP** is the global pointer used for addressing global
variables. **%%PC** is the program counter. **%%thrownval** stores the
thrown value after throwing an exception.

Special registers **%%retval0**, **%%retval1**, **%%retval2**, etc. are
used for fetching the multiple values returned by a call. They are
overwritten by each call, and should only be read at most once after
each call. They can assume whatever is the type of the return value.

Statement Labels (@)
--------------------

Label names are prefixed with \'@\' which serves to identify them. Any
statement beginning with a label name defines that label as referring to
that text position. Labels are only referred to locally by goto and
branch statements.

Storage Accesses
----------------

Since MAPLE IR is target-independent, it does not exhibit any
pre-disposition as to how storage are allocated for the program
variables. It only applies rules defined by the language regarding
storage.

In general, there are two distinct kinds of storage accesses: direct and
indirect. Direct accesses do not require any run-time computation to
determine the exact address location. Indirect accesses require address
arithmetic before the location can be determined. Indirect accesses are
associated with pointer dereferences and arrays. Direct accesses are
associated with scalar variables and fixed fields inside structures.

Direct accesses can be mapped to pseudo-register if the variable or
field has no alias. Indirect accesses cannot be mapped to
pseudo-registers unless the computed address does not change.

In MAPLE IR, **dassign** and **dread** are the opcodes for direct
assignments and direct references; **iassign** and **iread** are the
opcodes for indirect assignments and indirect references.

Aggregates
----------

Aggregates (or composites) are either structures or arrays. They both
designate a grouping of storage elements. In structures, the storage
elements are designated by field names and can be of different types and
sizes. Classes and objects are special kinds of structures in
object-oriented programming languages. In arrays, the same storage
element is repeated a number of times and the elements are accessed via
index (or subscript).

### Arrays

Array subscripting designates address computation. Since making the
subscripts stand out facilitate data dependency analysis and other loop
oriented optimizations, MAPLE IR represents array subscripting using the
special **array** opcode, which returns the address resulting from the
subscripting operation. For example, \"a\[i\] = i\" is:
```
    # <* [10] i32> is pointer to array of 10 ints
    iassign <* i32> (
      array a32 <* [10] i32> (addrof a32 $a, dread i32 $i),
      dread i32 \$i)
```

and \"x = a\[i,j\]\" is:


```
    # <* [10] [10] i32 indicates pointer to a 10x10 matrix of ints
    dassign $x (
      iread i32 <* i32> (
      array a32 <* [10] [10] i32> (addrof a32 $a, dread i32 $i, dread i32 $j)))
```

### Structures

Fields in a structure can be accessed directly, but use of **dassign**
or **dread** on a structure would refer to the entire structure as an
aggregate. Thus, we extend **dassign**, **dread**, **iassign** and
**iread** to take an additional parameter called field-ID.

In general, for a top level structure, unique field-IDs can be assigned
to all the fields contained inside the structure. Field-ID 0 is assigned
to the top level structure (the entire structure). Field-ID is also 0 if
it is not a structure. As each field is visited, the field-ID is
incremented by 1. If a field is a structure, that structure is assigned
a unique field-ID, and then field-ID assignments continue with the
fields inside the nested structure. If a field is an array, the array is
assigned only one field-ID.

Note that if a structure exists both standalone and nested inside
another structure, the same field inside the structure will be assigned
different field-IDs because field-ID assignment always starts from the
top level structure.

Three kinds of structures are supported: **struct**, **class** and
**interface**.

A **struct** corresponds to the struct type in C, and is specified by
the **struct** keyword followed by a list of field declarations enclosed
by braces, as in:
```
    struct {
      @f1 i32,
      @f2 <$structz> } # $structz is the type name of another struct
```
A **class** corresponds to the class type in Java, to provide single
inheritances. The syntax is the same as struct except for an additional
type name specified after the class keyword that specifies the class it
inherits from. Fields in the parent class are also referred to via
field-IDs, as if the first field of the derived class is the parent
class. In other words, the parent class is treated like a sub-structure.
```
    class <$classz> { # $classz is the parent of this class being defined
      @f1 i32,
      @f2 f32 }
```
Unrelated to storage, structures can contain member function prototypes.
The list of member function prototypes must appear after all the fields
have been specified. Each member function name starts with &, which
indicates that it is a function prototype. The prototype specification
follows the same syntax as ordinary function prototypes.

An **interface** corresponds to the interface type in Java, and has the
same form as class, except that it cannot be instantiated via a var
declaration, and fields declared inside it are always statically
allocated. More details are provided later in this document.

Instruction Specification
=========================

In MAPLE IR expression trees, we use parentheses or braces to
distinguish operands from the other fields of an instruction, to
facilitate visualization of the nested structure of MAPLE IR. The
expression operands of each instruction are always enclosed by
parentheses, using commas to separate the operands. Statement operands
are indicated via braces.

Storage Access Instructions
---------------------------

A memory access instruction either loads a memory location to a register
for further processing, or store a value from register to memory. For
load instructions, the result type given in the instruction is the type
of the loaded value residing in register. If the memory location is of
size smaller than the register size, the value being loaded must be of
integer type, and there will be implicit zero- or sign-extension
depending on the signedness of the result type.

### dassign

syntax: dassign \<var-name\> \<field-id\> (\<rhs-expr\>)

\<rhs-expr\> is computed to return a value, which is then assigned to
variable \<var-name\>. If \<field-id\> is not 0, then the variable must
be a structure, and the assignment only applies to the specified field.
If \<rhs-expr\> is of type agg, then the size of the structure must
match. If \<rhs-expr\> is a primitive integer type, the assigned
variable may be smaller, resulting in a truncation. If\<field-id\> is
not specified, it is assumed to be 0.

### dread

syntax: dread \<prim-type\> \<var-name\> \<field-id\>

Variable \<var-name\> is read from its storage location. If the variable
is a structure, then \<prim-type\> should specify agg. If \<field-id\>
is not 0, then the variable must be a structure, and instead of reading
the entire variable, only the specified field is read. If the field
itself is also a structure, then \<prim-type\> should also specify agg.
If \<field-id\> is not specified, it is assumed to be 0.

### iassign

syntax: iassign \<type\> \<field-id\> (\<addr-expr\>, \<rhs-expr\>)

\<addr-expr\> is computed to return an address. \<type\> gives the high
level type of \<addr-expr\> and must be a pointer type. \<rhs-expr\> is
computed to return a value, which is then assigned to the location
specified by \<addr-expr\>. If \<field-id\> is not 0, then the computed
address must correspond to a structure, and the assignment only applies
to the specified field. If \<rhs-expr\> is of type agg, then the size of
the structure must match. The size of the location affected by the
assignment is determined by what \<type\> points to. If \<rhs-expr\> is
a primitive integer type, the assigned location (according to what
\<type\> points to) may be smaller, resulting in a truncation. If
\<field-id\> is not specified, it is assumed to be 0.

### iread

syntax: iread \<prim-type\> \<type\> \<field-id\> (\<addr-expr\>)

The content of the location specified by the address computed from the
address expression \<addr-expr\> is read (dereferenced) as the given
primitive type. \<type\> gives the high level type of \<addr-expr\> and
must be a pointer type. If the content dereferenced is a structure (as
given by what \<type\> points to), then \<prim-type\> should specify
agg. If \<field-id\> is not 0, then \<type\> must specify pointer to a
structure, and instead of reading the entire structure, only the
specified field is read. If the field itself is also a structure, then
\<prim-type\> should also specify agg. If \<field-id\> is not specified,
it is assumed to be 0.

### iassignoff

syntax: iassignoff \<prim-type\> \<offset\> (\<addr-expr\>,
\<rhs-expr\>)

\<rhs-expr\> is computed to return a scalar value, which is then
assigned to the memory location formed by the addition of \<offset\> in
bytes to \<addr-expr\>. \<prim-type\> gives the type of the stored-to
location, which also specifies the size of the affected memory location.

### iassignfpoff

syntax: iassignfpoff \<prim-type\> \<offset\> (\<rhs-expr\>)

\<rhs-expr\> is computed to return a scalar value, which is then
assigned to the memory location formed by the addition of \<offset\> in
bytes to %%FP. \<prim-type\> gives the type of the stored-to location,
which also specifies the size of the affected memory location. This is
the same as iassignoff where its \<addr-expr\> is %%FP.

### iassignpcoff

syntax: iassignpcoff \<prim-type\> \<offset\> (\<rhs-expr\>)

\<rhs-expr\> is computed to return a scalar value, which is then
assigned to the memory location formed by the addition of \<offset\> in
bytes to %%PC.  \<prim-type\> gives the type of the stored-to location,
which also specifies the size of the affected memory location

### ireadoff

syntax: ireadoff \<prim-type\> \<offset\> (\<addr-expr\>)

\<prim-type\> must be of scalar type. \<offset\> in bytes is added to
\<addr-expr\> to form the address of the memory location to be read as
the specified scalar type.

### ireadfpoff

syntax: ireadfpoff \<prim-type\> \<offset\>

\<prim-type\> must be of scalar type. \<offset\> in bytes is added to
%%FP to form the address of the memory location to be read as the
specified scalar type. This is the same as ireadoff where its
\<addr-expr\> is %%FP.

### ireadpcoff

syntax: ireadpcoff \<prim-type\> \<offset\>

\<prim-type\> must be of scalar type.  \<offset\> in bytes is added to
%%PC to form the address location to be read as the specified scalar
type.

### regassign

syntax: regassign \<prim-type\> \<register\> (\<rhs-expr\>)

\<rhs-expr\> is computed to return a scalar value, which is then
assigned to the pseudo or special register given by \<register\>.
\<prim-type\> gives the type of the register, which also specifies the
size of the value being assigned.

### regread

syntax: regread \<prim-type\> \<register\>

The given pseudo or special register is read in the scalar type
specified by \<prim-type\>.

Leaf Opcodes
------------

dread and regread are leaf opcodes for reading the contents of
variables. The following are additional leaf opcodes:

### addrof

syntax: addrof \<prim-type\> \<var-name\> \<field-id\>

The address of the variable \<var-name\> is returned. \<prim-type\> must
be either ptr, a32 or a64. If \<field-id\> is not 0, then the variable
must be a structure, and the address of the specified field is returned
instead.

### addroflabel

syntax: addroflabel \<prim-type\> \<label\>

The text address of the label is returned. \<prim-type\> must be either
a32 or a64.

### addroffunc

syntax: addroffunc \<prim-type\> \<function-name\>

The text address of the function is returned. \<prim-type\> must be
either a32 or a64.

### addroffpc

syntax: addroffpc \<prim-type\> \<offset\>

\<prim-type\> must be either a32 or a64.  \<offset\> in bytes is added
to %%PC to form the address value being returned.

### conststr

syntax: conststr \<prim-type\> \<string literal\>

The address of the string literal is returned. \<prim-type\> must be
either ptr, a32 or a64. The string must be stored in read-only memory.

### conststr16

syntax: conststr16 \<prim-type\> \<string literal\>

The address of the string literal composed of 16-bit wide characters is
returned. \<prim-type\> must be either ptr, a32 or a64. The string must
be stored in read-only memory.

### constval

syntax: constval \<prim-type\> \<const-value\>

The specified constant value of the given primitive type is returned.
Since floating point values cannot be represented in ASCII without loss
of accuracy, they can be specified in hexadecimal form, in which case
\<prim-type\> indicates the floating point type.

### sizeoftype

syntax: sizeoftype \<int-prim-type\> \<type\>

The size in bytes of \<type\> is returned as an integer constant value.
Since type size is in general target-dependent, use of this opcode
preserves the target independence of the program code.

Unary Expression Opcodes
------------------------

These are opcodes with a single operand.

### abs

syntax: abs \<prim-type\> (\<opnd0\>)

The absolute value of the operand is returned.

### bnot

syntax: bnot \<prim-type\> (\<opnd0)

Each bit in the operand is reversed and the resulting value is returned.

### extractbits

syntax: extractbits \<int-type\> \<boffset\> \<bsize\> (\<opnd0\>)

The bitfield starting at bit position \<boffset\> with \<bsize\> number
of bits is extracted and then sign- or zero- extended to form the
primitive integer given by \<int-type\>. The operand must be of integer
type and must be large enough to contain the specified bitfield.

###  iaddrof

syntax: iaddrof \<prim-type\> \<type\> \<field-id\>(\<addr-expr\>)

\<type\> gives the high level type of \<addr-expr\> and must be a
pointer type. The address of the pointed-to item is returned.
\<prim-type\> must be either ptr, a32 or a64. If \<field-id\> is not 0,
then \<type\> must specify a pointer to a structure, and the address of
the specified field in the structure is returned instead. This operation
is of no utility if \<fielid\_id\> is 0, as it will just return the
value of \<addr-expr\>.

### lnot

syntax: lnot \<prim-type\> (\<opnd0\>)

If the operand is not 0, 0 is returned. If the operand is 0, 1 is
returned.

### neg

syntax: neg \<prim-type\> (\<opnd0\>)

The operand value is negated and returned.

### recip

syntax: recip \<prim-type\> (\<opnd0\>)

The reciprocal of the operand is returned. \<prim-type\> must be a
floating-point type.

### sext

syntax: sext \<signed-int-type\> \<bsize\> (\<opnd0\>)

Sign-extend the integer by treating the integer size as being \<bsize\>
bits. This can be regarded as a special case of extractbits where the
bitfield is in the lowest bits. The primitive type \<signed-int-type\>
stays the same.

### sqrt

syntax: sqrt \<prim-type\> (\<opnd0\>)

The square root of the operand is returned. \<prim-type\> must be a
floating-point type.

### zext

syntax: zext \<unsigned-int-type\> \<bsize\> (\<opnd0\>)

Zero-extend the integer by treating the integer size as being \<bsize\>
bits. This can be regarded as a special case of extractbits where the
bitfield is in the lowest bits. The primitive type \<unsigned-int-type\>
stays the same.

Type Conversion Expression Opcodes
----------------------------------

Type conversion opcodes are unary in nature. With the exception of
retype, they all require specifying both the from and to types in the
instruction. Conversions between integer types of different sizes
require the cvt opcode.

Conversion between signed and unsigned integers of the same size does
not require any operation, not even retype.

### ceil

syntax: ceil \<prim-type\> \<float-type\> (\<opnd0\>)

The floating point value is rounded towards positive infinity.

### cvt

syntax: cvt \<to-type\> \<from-type\> (\<opnd0\>)

Convert the operand\'s value from \<from-type\> to \<to-type\>. This
instruction must not be used If the sizes of the two types are the same
and the conversion does not result in altering the bit contents.

### floor

syntax: floor \<prim-type\> \<float-type\> (\<opnd0\>)

The floating point value is rounded towards negative infinity.

### Retype

syntax: retype \<prim-type\> \<type\> (\<opnd0\>)

\<opnd0\> is converted to \<prim-type\> which has derived type \<type\>
without changing any bits. The size of \<opnd0\> and \<prim-type\> must
be the same. \<opnd0\> may be of aggregate type.

### round

syntax: round \<prim-type\> \<float-type\> (\<opnd0\>)

The floating point value is rounded to the nearest integer.

### trunc

syntax: trunc \<prim-type\> \<float-type\> (\<opnd0\>)

The floating point value is rounded towards zero.

Binary Expression Opcodes
-------------------------

These are opcodes with two operands.

### add

syntax: add \<prim-type\> (\<opnd0\>, \<opnd1\>)

Perform the addition of the two operands.

### ashr

syntax: ashr \<int-type\> (\<opnd0\>, \<opnd1\>)

Return \<opnd0\> with its bits shifted to the right by \<opnd1\> bits.
The high order bits shifted in are set according to the original sign
bit.

### band

syntax: band \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the bitwise AND of the two operands.

### bior

syntax: bior \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the bitwise inclusive OR of the two operands.

### bxor

syntax: bxor \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the bitwise exclusive OR of the two operands.

### cand

syntax: cand \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the logical AND of the two operands via short-circuiting. If
\<opnd0\> yields 0, \<opnd1\> is not to be evaluated. The result is
either 0 or 1.

### cior

syntax: cior \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the logical inclusive OR of the two operands via
short-circuiting. If \<opnd0\> yields 1, \<opnd1\> is not to be
evaluated. The result is either 0 or 1.

### cmp

syntax: cmp \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

Performs a comparison between \<opnd0\> and \<opnd1\>. If the two
operands are equal, return 0. If \<opnd0\> is less than \<opnd1\>,
return -1. Otherwise, return +1.

### cmpg

syntax: cmpg \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

Same as cmp, except 1 is returned if any operand is NaN.

### cmpl

syntax: cmpl \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

Same as cmp, except -1 is returned if any operand is NaN.

### depositbits

syntax: depositbits \<int-type\> \<boffset\> \<bsize\> (\<opnd0\>,
\<opnd1\>)

Creates a new integer value by depositing the value of \<opnd1\> into
the range of bits in \<opnd0\> that starts at bit position \<boffset\>
and runs for \<bsize\> bits. \<opnd0\> must be large enough to contain
the specified bitfield.

Depending on the size of \<opnd1\> relative to the bitfield, there may
be truncation. The rest of the bits in \<opnd0\> remains unchanged.

### div

syntax: div \<prim-type\> (\<opnd0\>, \<opnd1\>)

Perform \<opnd0\> divided by \<opnd1\> and return the result.

### eq

syntax: eq \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If the two operands are equal, return 1. Otherwise, return 0.

### ge

syntax: ge \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If \<opnd0\> is greater than or equal to \<opnd1\>, return 1. Otherwise,
return 0.

### gt

syntax: ge \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If \<opnd0\> is greater than \<opnd1\>, return 1. Otherwise, return 0.

### land

syntax: land \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the logical AND of the two operands. The result is either 0 or
1.

### lior

syntax: lior \<int-type\> (\<opnd0\>, \<opnd1\>)

Perform the logical inclusive OR of the two operands. The result is
either 0 or 1.

### le

syntax: le \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If \<opnd0\> is less than or equal to \<opnd1\>, return 1. Otherwise,
return 0.

### lshr

syntax: lshr \<int-type\> (\<opnd0\>, \<opnd1\>)

Return \<opnd0\> with its bits shifted to the right by \<opnd1\> bits.
The high order bits shifted in are set to 0.

### lt

syntax: lt \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If \<opnd0\> is less than \<opnd1\>, return 1. Otherwise, return 0.

### max

syntax: max \<prim-type\> (\<opnd0\>, \<opnd1\>)

Return the maximum of \<opnd0\> and \<opnd1\>.

### min

syntax: min \<prim-type\> (\<opnd0\>, \<opnd1\>)

Return the minimum of \<opnd0\> and \<opnd1\>.

### mul

syntax: mul \<prim-type\> (\<opnd0\>, \<opnd1\>)

Perform the multiplication of the two operands.

### ne

syntax: ne \<int-type\> \<opnd-prim-type\> (\<opnd0\>, \<opnd1\>)

If the two operands are not equal, return 1. Otherwise, return 0.

### rem

syntax: rem \<prim-type\> (\<opnd0\>, \<opnd1\>)

Return the remainder when \<opnd0\> is divided by \<opnd1\>.

### shl

syntax: shl \<int-type\> (\<opnd0\>, \<opnd1\>)

Return \<opnd0\> with its bits shifted to the left by \<opnd1\> bits.
The low order bits shifted in are set to 0.

### sub

syntax: sub \<prim-type\> (\<opnd0\>, \<opnd1\>)

Subtract \<opnd1\> from \<opnd0\> and return the result.

Ternary Expression Opcodes
--------------------------

These are opcodes with three operands.

### select

syntax: select \<prim-type\> (\<opnd0\>, \<opnd1\>, \<opnd2\>)

\<opnd0\> must be of integer type. \<opnd1\> and \<opnd2\> must be of
the type given by \<prim-type\>. If \<opnd0\> is not 0, return
\<opnd1\>. Otherwise, return \<opnd2\>.

N-ary Expression Opcodes
------------------------

These are opcodes that can have any number of operands.

### array

syntax: array \<check-flag\> \<addr-type\> \<array-type\> (\<opnd0\>,
\<opnd1\>, . . . , \<opndn\>)

\<opnd0\> is the base address of an array in memory. \<check-flag\> is
either 0 or 1, indicating bounds-checking needs not be performed or
needs to be performed respectively. \<array-type\> gives the high-level
type of a pointer to the array. Return the address resulting from
row-major order multi-dimentional indexing operation, with the indices
represented by \<opnd1\> onwards. Since arrays must have at least one
dimension, this opcode must have at least two operands.

### intrinsicop

syntax: intrinsicop \<prim-type\> \<intrinsic\> (\<opnd0\>, \...,
\<opndn\>)

\<intrinsic\> indicates an intrinsic function that has no side effect
whose return value depends only on the arguments (a pure function), and
thus can be represented as an expression opcode.

### intrinsicopwithtype

syntax: intrinsicopwithtype \<prim-type\> \<type\> \<intrinsic\>
(\<opnd0\>, \..., \<opndn\>)

This is the same as intrinsicop except that it takes on an additional
high level type argument specified by \<type\>.

Control Flow Statements
-----------------------

Program control flows can be represented by either hierarchical
statement structures or a flat list of statements. Hierarchical
statement structures are mostly derived from constructs in the source
language. Flat control flow statements correspond more closely to
processor instructions. Thus, hierarchical statements exist only in high
level MAPLE IR. They are lowered to the flat control flow statements in
the course of compilation.

A statement block is indicated by multiple statements enclosed inside
the braces \'{\' and \'}\'. Such statement blocks can appear any where
that a statement is allowed. In hierarchical statements, nested
statements are specified by such statement blocks. Statement blocks
should only associated with hierarchical control flow statements.

In MAPLE IR, each statement must start on a new line. The use of
semicolon is not needed, or even allowed, to indicate the end of
statements.

### Hierarchical control flow statements

#### doloop

syntax:
```
    doloop <do-var> (<start-expr>, <cont-expr>, <incr-amt>) {
      <body-stmts> }
```

\<do-var\> specifies a local integer scalar variable with no alias.
\<incr-amt\> must be an integer expression. \<do-var\> is initialized
with \<start-expr\>. \<cont-expr\> must be a single comparison operation
representing the termination test. The loop body is represented by
\<body-stmts\>, which specifies the list of statements to be executed as
long as \<cont-expr\> evaluates to true. After each execution of the
loop body, \<do-var\> is incremented by \<incr-amt\> and the loop is
tested again for termination.

#### dowhile

syntax:
```
    dowhile {
      <body-stmts>
    } (<cond-expr>)
```
Execute the loop body represented by \<body-stmts\>, and while
\<cond-expr\> evaluates to non-zero, continue to execute \<body-stmts\>.
Since \<cond-expr\> is tested at the end of the loop body, the loop body
is executed at least once.

#### foreachelem

syntax:
```
    foreach <elem-var> <collection-var> {
      <body-stmts> }
```

This is an abstract loop form where \<collection-var\> is an array-like
variable representing a collection of uniform elements, and \<elem-var\>
specifies a variable whose type is the element type of
\<collection-var\>. The loop body is represented by \<body-stmts\>,
which specifies the list of statements to be repeated for each element
of \<collection-var\> expressed via \<elem-var\>. This statement will be
lowered to a more concrete loop form based on the type of
\<collection-var\>.

#### if

syntax:
```
    if (<cond-expr>) {
      <then-part> }
    else {
      <else-part> }
```
If \<cond-expr\> evaluates to non-zero, control flow passes to the
\<then-part\> statements. Otherwise, control flow passes to the
\<else-part\> statements. If there is no else part, \"else {
\<else-part\> }\" can be omitted.

#### while

syntax:
```
    while (<cond-expr>) {
      <body-stmts> }
```
This implements the while loop. While \<cond-expr\> evaluates to
non-zero, the list of statements represented by \<body-stmts\> are
repeatedly executed. Since \<cond-expr\> is tested before the first
execution, the loop body may execute zero times.

### Flat control flow statements

#### brfalse

syntax: brfalse \<label\> (\<opnd0\>)

If \<opnd0\> evaluates to 0, branch to \<label\>. Otherwise, fall
through.

#### brtrue

syntax: brtrue \<label\> (\<opnd0\>)

If \<opnd0\> evaluates to non-0, branch to \<label\>. Otherwise, fall
through.

#### goto

syntax: goto \<label\>

Transfer control unconditionally to \<label\>.

#### multiway

syntax:
```
    multiway (<opnd0>) <default-label> {
      (<expr0>): goto <label0>
      (<expr1>): goto <label1>
      ...
      (<exprn>): goto <labeln> }
```

\<opnd0\> must be of type convertible to an integer or a string.
Following \<default-label\> is a table of pairs of expression tags and
labels. When executed, it evaluates \<opnd0\> and then searches the
table for a match of the evaluated value of \<opnd0\> with the evaluated
value of each expression tag \<expri\> in the listed order. On a match,
control is transferred to the corresponding label. If no match is found,
control is transferred to \<default- label\>. The evaluation of the
expression tags must not incur side effect. Depending on the resolved
type of \<opnd0\>, this statement will be lowered to either the switch
statement or a cascade of if statements.

#### return

syntax: return (\<opnd0\>, . . ., \<opndn\>)

Return from the current PU with the multiple return values given by the
operands. The list of operands can be empty, which corresponds to no
return value. The types of \<opndi\> must be compatible with the list of
return types according to the declaration or prototype of the PU.

#### switch

syntax:
```
    switch (<opnd0>) <default-label> {
      <intconst0>: goto <label0>
      <intconst1>: goto <label1>
      ...
      <intconstn>: goto <labeln> }
```

\<opnd0\> must be of integer type. After \<default-label\>, it specifies
a table of pairs of constant integer values (tags) and labels. When
executed, it searches the table for a match with the evaluated value of
\<opnd0\> and transfers control to the corresponding label. If no match
is found, control is transferred to \<default-label\>. There must not be
duplicate entries for the constant integer values. It is up to the
compiler backend to decide how to actually generate code for this
statement after analyzing the tag distribution in the table.

#### rangegoto

syntax:
```
    rangegoto (<opnd0> <tag-offset> {
      <intconst0>: goto <label0>
      <intconst1>: goto <label1>
      ...
      <intconstn>: goto <labeln> }
```

This is the lowered form of switch that explicitly designates its
execution to be handled by the jump table mechanism. \<opnd0\> must be
of integer type. After \<tag-offset\> follows a table of pairs of
constant integer values and labels. In searching the table for a match
during execution, the evaluated value of \<opnd0\> minus \<tag-offset\>
is used during execution so as to cause transfer of control to the
corresponding label. There must be no gap in the constant integer values
specified, and a match is guaranteed within the range of specified
constant integer values, which means the code generator can omit
generation of out-of-range checks. There must be no duplicated entries
for the constant integer values.

#### indexgoto

syntax: indexgoto (\<opnd0\> \<var-name\>

This is only generated by the compiler as a result of lowering the
switch statement. \<var-name\> is the name of a compiler-generated
symbol designating a static array, or jump table, which is statically
initialized to store labels.

Each stored label marks the code corresponding to a switch case.
Execution of this instruction uses the evaluated value of \<opnd0\> to
index into this array and then transfers control to the label stored at
that array element. If the evaluated value of \<opnd0\> is less than 0
or exceeds the number of entries in the jump table, the behavior is
undefined.

Call Statements
---------------

There are various flavors of procedure invocation. Intrinsics are
library functions known to the compiler.

### call

syntax: call \<func-name\> (\<opnd0\>, \..., \<opndn\>)

Invoke the function while passing the parameters given by the operands.

### callinstant

syntax: callinstant \<generic-func-name\> \<instant-vector\> (\<opnd0\>,
\..., \<opndn\>)

Instantiate the given generic function according to instantiation vector
\<instant-vector\> and then invoke the function while passing the
parameters given by the operands.

### icall

syntax: icall (\<func-ptr\>, \<opnd0\>, \..., \<opndn\>)

Invoke the function specified indirectly by \<func-ptr\>, passing the
parameters given by \<opnd0\> onwards.

### intrinsiccall

syntax: intrinsiccall \<intrinsic\> (\<opnd0\>, \..., \<opndn\>)

Invoke the specified intrinsic defined by the compiler while passing the
parameters given by the operands.

### intrinsiccallwithtype

syntax: intrinsiccallwithtype \<type\> \<intrinsic\> (\<opnd0\>, \...,
\<opndn\>)

This is the same as intrinsiccall except that it takes on an additional
high level type argument specified by \<type\>.

### xintrinsiccall

syntax: xintrinsiccall \<user-intrinsic-index\> (\<opnd0\>, \...,
\<opndn\>)

Invoke the intrinsic specified as an index into a user-defined intrinsic
table while passing the parameters given by the operands.

Java Call Statements
--------------------

The following statements are used to represent Java member function
calls that are not yet resolved.

### virtualcall

syntax: virtualcall \<method-name\> (\<object-ptr\>, \<opnd0\>, \...,
\<opndn\>)

\<object-ptr\> is a pointer to an instance of a class. The class
hierarchy is searched using the specified \<method-name\> to find the
appropriate virtual method to invoke. The invocation will pass the
remaining operands as parameters.

### superclasscall

syntax: superclasscall \<method-name\> (\<object-ptr\>, \<opnd0\>, \...,
\<opndn\>)

This is the same as virtualcall except it will not use the class\'s own
virtual method, but the one in its closest superclass that defines the
virtual method.

### interfacecall

syntax: interfacecall \<method-name\> (\<object-ptr\>, \<opnd0\>, \...,
\<opndn\>)

\<method-name\> is a method defined in an interface. \<object-ptr\> is a
pointer to an instance of a class the implements the interface. The
class is searched using the \<method-name\> to find the corresponding
method to invoke. The invocation will pass the remaining operands as
parameters.

There are also virtualcallinstant, superclasscallinstant and
interfacecallinstant for calling generic versions of the methods after
instantiating with the specified instantiation vector. The instantiation
vector is specified after \<method-name\>, as in the callinstant
instruction.

Calls with Return Values Assigned
---------------------------------

MAPLE IR supports calls with any number of return values. All the
various call operations have a corresponding higher-level abstracted
variant such that a single call operation also specify how the multiple
function return values are assigned, without relying on separate
statements to read the %%retval registers. Only assignments to local
scalar variables, fields in local aggregates or pseudo-registers are
allowed. These operations have the same names with the suffix
\"assigned\" added. They are callassigned, callinstantassigned,
icallassigned, intrinsiccallassigned, intrinsiccallwithtypeassigned,
xintrinsiccallassigned, virtualcallassigned, virtualcallinstantassigned,
superclasscallassigned, superclasscallinstantassigned,
interfacecallassigned and interfacecallinstantassigned. Only
callassigned is defined here. The same extension applies to the
definitions of the rest of these call operations.

### callassigned

syntax:
```
    callassigned <func-name> (<opnd0>, ..., <opndn>) {
      dassign <var-name0> <field-id0>
      dassign <var-name1> <field-id1>
      ...
      dassign <var-namen> <field-idn> }
```

Invoke the function passing the parameters given by the operands. After
returning from the call, the multiple return values are assigned to the
scalar variables listed in order. If a specified field-id is not 0, then
the corresponding variable must be an aggregate, and the assignment is
to the field corresponding to the field-id. If a field-id is absent, 0
is implied. If \<var-name\> is absent, it means the corresponding return
value is ignored by the caller. If a call has no return value, no
dassign should be listed.

In the course of compilation, these call instructions may be lowered to
use the special registers %%retval0, retval1, %%retval2, etc. to
indicate how their return values are fetched and used. These special
registers are overwritten by each call. The same special register can
assume whatever is the type of the return value. Each special register
can be read only once after each call.

Exceptions Handling
-------------------

Described in this section are the various exception handling constructs
and operations. The try statement marks the entrance to a try block. The
catch statement marks the entrance to a catch block. The finally
statement marks the entrance to a finally block. The endtry statement
marks the end of the composite exception handling constructs that began
with the try. In addition, there are two special types of labels.
Handler labels are placed before catch statements, and finally labels
are placed before finally statements. Handler labels are distinguished
from ordinary labels via the prefix \"\@h@\", while finally labels use
the prefix \"\@f@\". These special labels explicitly shows the
correspondence of try, catch and finally to each other in each
try-catch-finally composite, without relying on block nesting. The
special register %%thrownval contains the value being thrown, which is
the operand of the throw operation that raised the current exception.

Since different languages have exhibit different semantics or behavior
related to the exception handling constructs, the try and catch opcodes
have different variants distinguished by their language prefices.

### jstry

syntax: jstry \<handler-label\> \<finally-label\>

Executing this statement indicates entry into a Javascript try block.
\<handler-label\> is 0 when there is no catch block associated with the
try. \<finally-label\> is 0 when there is no finally block associated
with the try. Any exception thrown inside this try block will transfer
control to these labels, unless another nested try block is entered. A
finally block if present must be executed to conclude the execution of
the try composite constructs regardless of whether exception is thrown
or not.

There are three possible scenarios based on the way the
jstry-jscatch-finally composite is written:

1.  jstry-jscatch

2.  jstry-finally

3.  jstry-jscatch-finally

For case 1, if an exception is thrown inside the try block, control is
transferred to the handler label that marks the catch statement and the
exception is regarded as having been handled. Program flow eventually
exits the try block with a goto statement to the label that marks the
endtry statement. If no exception is thrown, the try block is eventually
exited via a goto statement to the label that marks the endtry
statement.

For case 2, if an exception is thrown inside the try block, control is
transferred to the finally label that marks the finally statement. But
the exception is regarded as not having been handled yet, and the search
for the throw\'s upper level handler starts. If no exception is thrown
in try block, program flow eventually exits the try block with a gosub
statement to the finally block. Execution in the finally block ends with
a retsub, which returns to the try block and the try block is then
exited via a goto statement to the label that marks the endtry
statement.

For case 3, if an exception is thrown inside the try block, control is
transferred to the handler label that marks the catch statement as in
case 1. Execution in the catch block ends with a gosub statement to the
finally block. Execution in the finally block ends with a retsub, which
returns to the catch block and the catch block is then exited via a goto
statement to the label that marks the endtry statement. If no exception
is thrown in the try block, program flow eventually exits the try block
with a gosub statement to the finally block, and execution will continue
in the finally block until it executes a retsub, at which time it
returns to the try block and the try block is then exited via a goto
statement to the label that marks the endtry statement.

### javatry

syntax: javatry {\<handler-label1\> \<handler-label2\> . . .
\<handler-labeln\>}

This is the Java flavor of try, which can have more than one catch block
but no finally block. The entry labels for the catch blocks are listed
in order and enclosed in braces.

### cpptry

syntax: cpptry {\<handler-label1\> \<handler-label2\> . . .
\<handler-labeln\>}

This is the C++ flavor of try, which can have more than one catch block
but no finally block. The entry labels for the catch blocks are listed
in order and enclosed in braces.

### throw

syntax: throw (\<opnd0\>)

Raise a user-defined exception with the given exception value. If this
statement is nested within a try block, control is then transferred to
the label associated with the try, which is either a catch statement or
a finally statement. If this throw statement is nested within a catch
block, control is first transferred to the finally associated with the
catch if any, in which case the finally block will be executed. After it
finishes executing the finally block, the search for the throw\'s upper
level handler starts. If this throw statement is nested within a finally
block, the search for the throw\'s upper level handler starts right
away. If the throw is not nested inside any try block within a function,
the system will look for the first enclosing try block by unwinding the
call stack. If no try block is found, the program will terminate. Inside
the catch block, the thrown exception value can be accessed using the
special register %%thrownval.

### jscatch

syntax: \<handler-label\> jscatch

This marks the start of the catch block associated with a try in
Javascript. The try block associated with this catch block is regarded
to have been exited and the exception is regarded as being handled. If
no exception is thrown inside the catch block, exit from the catch block
is effected by either a gosub statement to a finally label, if there is
a finally block, or a goto statement to endtry.

### javacatch

syntax: \<handler-label\> javacatch {\<type1\> \<type2\> \... \<typen\>}

This marks the start of a catch block in Java. The possible types of
thrown value that match this catch block are listed. If none of the
specified types corresponds to the type of the thrown value, control
will pass to the next javacatch.

### cppcatch

syntax: \<handler-label\> javacatch \<type1\>

This marks the start of a catch block in C++, in which each catch block
can only be matched by one type of thrown value. If specified type does
not correspond to the type of the thrown value, control will pass to the
next cppcatch.

### finally

syntax: \<finally-label\> finally

This marks the start of the finally block in Javascript. The finally
block can be entered either via the execution of a gosub statement, or a
throw statement the finally\'s corresponding try block that has no catch
block, or a throw statement in the finally\'s corresponding catch block.
The exit from the finally block can be effected by the execution of
either a retsub or a throw statement in the finally block. If the exit
is via a retsub and if there is outstanding throw yet to be handled, the
search for the throw\'s upper level handler continues.

### cleanuptry

syntax: cleanuptry

This statement is generated in situations where the control is going to
leave the try-catch-finally composite prematurely via jumps unrelated to
exception handling. This statement effects the cleanup work related to
exception handling for the current try-catch-finally composite in
Javascript.

### endtry

syntax: \<label\> endtry

This marks either the end of each try-catch-finally composite or the end
of each javatry block.

### gosub

syntax: gosub \<finally-label\>

Transfer control to the finally block marked by \<finally-label\>. This
also has the effect of exiting the try block or catch block which this
statement belongs. It is like a goto, except that the next instruction
is saved. Execution will transfer back to the next instruction when a
retsub statement is executed. This can also be thought of as a call,
except it uses label name instead of function name, and no passing of
parameter or return value is implied. This opcode is only generated from
Javascript.

### retsub

syntax: retsub

This must only occur as the last instruction inside a finally block. If
there is no outstanding throw, control is transferred back to the
instruction following the last gosub executed. Otherwise the search for
the upper level exception handler continues. This opcode is only
generated from Javascript.

Memory Allocation and Deallocation
----------------------------------

The following instructions are related to the allocation and
de-allocation of dynamic memory during program execution. The
instructions with \"gc\" as prefix are associated with languages with
managed runtime environments.

### alloca

syntax: alloca \<prim-type\> (\<opnd0\>)

This returns a pointer to the block of uninitialized memory allocated by
adjusting the function stack pointer %%SP, with size in bytes given by
\<opnd0\>. This instruction must only appear as the right hand side of
an assignment operation.

### decref

syntax: decref (\<opnd0\>)

\<opnd0\> must be a dread or iread of a pointer that refers to an object
allocated in the run-time-managed part of the heap. It decrements the
reference count of the pointed-to object by 1. \<opnd0\> must be of
primitive type ref.

### decrefreset

syntax: decrefreset(\<opnd0\>)

\<opnd0\> must be the address of a pointer that refers to an object
allocated in the run-time-managed part of the heap. It decrements the
reference count of the pointed-to object by 1, and then reset the value
of the pointer to null by zeroing its memory location.

### free

syntax: free (\<opnd0\>)

The block of memory pointed to by \<opnd0\> is released so it can be
re-allocated by the system for other uses.

### gcmalloc

syntax: gcmalloc \<pointer prim-type\> \<type\>

This requests the memory manager to allocate an object of type \<type\>
with associated meta-data according to the requirements of the managed
runtime. The size of the object must be fixed at compilation time. As
this returns the pointer to the allocated block, this instruction must
only appear as the right hand side of an assignment operation.

The managed runtime is responsible for its eventual deallocation.

### gcmallocjarray

syntax: gcmallocjarray \<pointer prim-type\> \<java-array-type\>
(\<opnd0\>)

This requests the memory manager to allocate a java array object as
given by \<java-array-type\>. The allocated storage must be large enough
to store the number of array elements as given by \<opnd0\>. As this
returns the pointer to the allocated block, this instruction must only
appear as the right hand side of an assignment operation.

The managed runtime is responsible for its eventual deallocation, and
the block size must remain fixed during its life time.

### gcpermalloc

syntax: gcpermalloc \<pointer prim-type\> \<type\>

This requests the memory manager to allocate an object of type \<type\>
in the permanent area of the heap which is not subject to deallocation.
The size of the object must be fixed at compilation time. As this
returns the pointer to the allocated block, this instruction must only
appear as the right hand side of an assignment operation

### incref

syntax: incref (\<opnd0\>)

\<opnd0\> must be a dread or iread of a pointer that refers to an object
allocated in the run-time managed part of the heap. It increments the
reference count of the pointed-to object\'s by 1. \<opnd0\> must be of
primitive type ref.

### malloc

syntax: malloc \<pointer prim-type\> (\<opnd0\>)

This requests the system to allocate a block of uninitialized memory
with size in bytes given by \<opnd0\>. As this returns the pointer to
the allocated block, this instruction must only appear as the right hand
side of an assignment operation. The block of memory remains unavailable
for re-use until it is explicitly freed via the free instruction.

### stackmalloc

syntax: stackmalloc \<pointer prim-type\> \<type\>

This allocates an object of type \<type\> on the function stack frame by
decrementing %%SP. The size of the object must be fixed at compilation
time. As this returns the pointer to the allocated block, this
instruction must only appear as the right hand side of an assignment
operation.

### stackmallocjarray

syntax: stackmallocjarray \<pointer prim-type\> \<java-array-type\>
(\<opnd0\>)

This allocates a java array object as given by \<java-array-type\> on
the function stack frame by decrementing %%SP. The allocated storage
must be large enough to store the number of array elements as given by
\<opnd0\>. As this returns the pointer to the allocated block, this
instruction must only appear as the right hand side of an assignment
operation.

Other Statements
----------------

### assertge

syntax: assertge (\<opnd0\>, \<opnd1\>)

Raise an exception if \<opnd0\> is not greater than or equal to
\<opnd1\>. This is used for checking if an array index is within range
during execution. \<opnd0\> and \<opnd1\> must be of the same type.

### assertlt

syntax: assertlt (\<opnd0\>, \<opnd1\>)

Raise an exception if \<opnd0\> is not less than \<opnd1\>. This is used
for checking if an array index is within range during execution.
\<opnd0\> and \<opnd1\> must be of the same type.

### assertnonnull

syntax: assertnonnull (\<opnd0\>)

Raise an exception if \<opnd0\> is a null pointer, corresponding to the
value 0.

### eval

syntax: eval (\<opnd0\>)

\<opnd0\> is evaluated but the result is thrown away. If \<opnd0\>
contains volatile references, this statement cannot be optimized away.

### checkpoint

syntax: checkpoint \<action\>

This instruction serves as a check point, such that when execution
reaches this instruction, it will trigger the indicated action.

### membaracquire

syntax: membaracquire

This instruction acts as both a Load-to-Load barrier and a Load-to-Store
barrier: the order between any load instruction before it and any load
instruction after it must be strictly followed, and the order between
any load instruction before it and any store instruction after it must
be strictly followed.

### membarrelease

syntax: membarrelease

This instruction acts as both a Load-to-Store barrier and a
Store-to-Store barrier: the order between any load instruction before it
and any store instruction after it must be strictly followed, and the
order between any store instruction before it and any store instruction
after it must be strictly followed.

### membarfull

syntax: membarfull

This instruction acts as a barrier to any load or store instruction
before it and any load or store instruction after it.

### syncenter

syntax: syncenter (\<opnd0\>)

This instruction indicates entry to a region where the object pointed to
by the pointer \<opnd0\> needs to be synchronized for Java
multi-threading. This means at any time, there cannot be more than one
thread executing in a synchronized region of the same object. Any other
thread attempting to enter a synchronized region of the same object will
be blocked. For the compiler, it implies a barrier to the backward
movement (against the flow of control) of any operation that accesses
the object.

### syncexit

syntax: syncexit (\<opnd0\>)

This instruction indicates exit from a region where the object pointed
to by the pointer \<opnd0\> needs to be synchronized for Java
multi-threading. For the compiler, it implies a barrier to the forward
movement (along the flow of control) of any operation that accesses the
object.

Declaration Specification
=========================

In this section, we describes the various kinds of statements in the
declaration part of Maple IR. Internally, they are represented by data
structures organized into different kinds of tables.

Type declarations can be huge and they are often shared by different
modules, MAPLE IR provides the *import* facility to avoid duplicating
type declarations in each MAPLE IR file. MAPLE IR files that store only
type information have .mplt as file suffix. They can then be imported
into each MAPLE IR files that need them via the **import** statement.

Module Declaration
------------------

Each Maple IR file represents a program module, also called compilation
unit, that consists of various declarations at the global scope. The
following directives appear at the start of the Maple IR file and
provide information about the module:

### entryfunc

syntax: entryfunc \<func-name\>

This gives the name of the function defined in the module that will
serve as the single entry point for the module.

### flavor

syntax: flavor \<IR-flavor\>

The IR flavor gives information as to how the IR was produced, which in
turn indicates the state of the compilation process.

### globalmemmap

syntax: globalmemmap = \[ \<initialization-values\> \]

This specifies the static initialization values of the global memory
block as a list of space-separated 32-bit integer constants. The amount
of initializations should correspond to the memory size given by
globalmemsize.

### globalmemsize

syntax: globalmemsize \<size-in-bytes\>

This gives the size of the global memory block for storing all global
static variables.

### globalwordstypetagged

syntax: globalwordstypetagged = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the Nth word in globalmemmap has a type tag, in
which case the type tag is at the (N+1)th word.

### globalwordsrefcounted

syntax: globalwordsrefcounted = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the Nth word in globalmemmap is a pointer to a
reference-counted dynamically allocated memory block.

### id

syntax: id \<id-number\>

This gives the unique module id assigned to the module. This id enables
the Maple virtual machine to handle the execution of program code
originating from multiple Maple IR modules.

### import

syntax: import \"\<filename\>\"

\<filename\> is the path name for a MAPLE IR file with suffix .mplt that
only stores type declarations. The contents of this file are imported.
This allows the same type declarations to be shared across multiple
files, and large volumes of type declarations to be organized by files.
Only one level of import is allowed, as .mplt files are not allowed to
have import statements.

### importpath

syntax: importpath \"\<path-name\>\"

This specifies a directory path for the compiler to look for the
imported MAPLE IR files required to complete the compilation. This is
used only in the early compilation phases, before all types and symbol
names have been fully resolved. Each appearance only specifies one
specific path.

### numfuncs

syntax: numfuncs \<integer\>

This gives the number of function definitions in the module, excluding
function prototypes.

### srclang

syntax: srclang \<language\>

This gives the source language that produces the Maple IR module.

Variable Declaration
--------------------

syntax: var \<id-name\> \<storage-class\> \<type\> \<type-attributes\>

The keyword \'var\' designates a declaration statement for a variable.
\<id-name\> specifies its name, prefixed by \'\$\' or \'%\' based on
whether its scope is global or local respectively. \<storage-class\> is
optional and can be extern, fstatic or pstatic. \<type\> is the type
specification. \<type-attributes\> is optional and specifies additional
attributes like volatile, const, alignment, etc. Examples:

var \$x extern f32 volatile static

syntax: tempvar \<id-name\> \<storage-class\> \<type\>
\<type-attributes\>

This is the same as **var** except that it conveys the additional
information that it is a compiler-generated temporary.

Pseudo-register Declarations
----------------------------

syntax: reg \<preg-name\> \<type\>

The keyword \'reg\' designates a declaration statement for a
pseudo-register. \<preg-name\> specifies the pseudo-register prefixed by
\'%\'. \<type\> is the high level type information. If a pseudo-register
is only of primitive type, its declaration is optional. This statement
is only allowed inside functions as pseudo-registers are of local scope.

Type Specification
------------------

Types are either primitive or derived. Derived types are specified using
C-like tokens, except that the specification is always
right-associative, following a strictly left to right order. Derived
types are distinguished from primitive types by being enclosed in
angular brackets. Derived types can also be thought of as high-level
types. Examples:
```
    var %p <* i32>      # pointer to a 32-bit integer

    var %a <[10] i32>   # an array of 10 32-bit integers
```
var %foo \<func(i32) i32\> \# a pointer to a function that takes one i32
parameter and returns an i32 value (func is a keyword)

Additional nested angular brackets are not required, as there is no
ambiguity due to the right-associative rule. But the nested angular
brackets can be optionally inserted to aid readability. Thus, the
following two examples are equivalent:
```
    var %q <* <[10] i32>> # pointer to an array of 10 32-bit integers

    var %q <* [10] i32>   # pointer to an array of 10 32-bit integers
```
Inside a struct declaration, field names are prefixed by @. Though label
names also use @ as prefix, there is no ambiguity due to the usage
context between struct field declaration and label usage being distinct.
Example:
```
    var %s <struct{@f1 i32,
                   @f2 f64,
                   @f3:3 i32}> # a bitfield of 3 bits
```
A union declaration has the same syntax as struct. In a union, all the
fields overlap with each other.

The last field of a struct can be a flexible array member along the line
of the C99 standard, which is an array with variable number of elements.
It is specified with empty square brackets, as in:
```
    var %p <* struct{@f1 i32,
                     @f2 <[] u16>}> # a flexible array member with unsigned 16-bit integers as elements
```
Structs with flexible array member as its last field can only be
dynamically allocated. Its actual size is fixed only at its allocation
during execution time, and cannot change during its life time. A struct
with flexible array member cannot be nested inside another aggregate.
During compilation, the flexible array member is regarded as having size
zero.

Because its use is usually associated with managed runtime, a language
processor may introduce additional meta-data associated with the array.
In particular, there must be some language-dependent scheme to keep
track of the size of the array during execution time.

When a type needs to be qualified by additional attributes for const,
volatile, restrict and various alignments, they follow the type that
they qualify. These attributes are not regarded as part of the type. If
these attributes are applied to a derived type, they must follow outside
the type angular brackets. Examples:
```
    var %x f64 volatile align(16) # %s is a f64 value that is volatile and
                                  # aligned on 16 byte boundary
    var %p <* f32> const volatile # %p is a pointer to a f32 value, and
                                  # %p itself is const and volatile
```
Alignments are specified in units of bytes and must be power of 2.
Alignment attributes must only be used to increase the natural
alignments of the types, to make the alignments more stringent. For
decreasing alignments, the generator of MAPLE IR must use smaller types
to achieve the effect of packing instead of relying on the align
attribute.

### Incomplete Type Specification

Languages like Java allow contents of any object to be referenced
without full definition of the object being available. Their full
contents are to be resolved from additional input files in later
compilation phases. MAPLE IR allows structs, classes and interfaces to
be declared incompletely so their specific contents can be referenced.
Instead of the struct, class and interface keywords, structincomplete,
classincomplete and interfaceincomplete should be used instead.

Type Declaration
----------------

The purpose of type declaration is to associate type names with types so
they can be referred to via their names, thus avoiding repeated
specification of the details of the types.

syntax: type \<id-name\> \<derived-type\>

Type names are also prefixed with either \'\$\' or \'%\' based on
whether the scope is global or local. Example:
```
    type $i32ptr <* i32>        # the type $i32ptr is a pointer to i32
```
Primitive types are not allowed to be given a different type name.

Attributes are not allowed in type declaration.

Once a type name is defined, specifying the type name is equivalent to
specifying the derived type that it stands for. Thus, the use of a type
name should always be enclosed in angular brackets.

Java Class and Interface Declaration
------------------------------------

A Java class designates more than a type, because the class name also
carries the attributes declared for the class. Thus, we support
declaration of Java classes via:

syntax: javaclass \<id-name\> \<class-type\> \<attributes\>

\<id-name\> must have \'\$\' as prefix as class names always have global
scope. For example:
```
    # a java class named "Puppy" with a single field "color" and attributes public and final
    javaclass $Puppy <class {@color i32}> public final
```
A **javaclass** name should not be regarded as a type name as it
contains additional attribute information. It cannot be enclosed in
angular brackets as it cannot be referred to as a type.

A java interface has the same form as the class type, being able to
extend another interface, but unlike class, an interface can extend
multiple interfaces. Another difference from class is that an interface
cannot be instantiated. Without instantiation, the data fields in
interfaces are always allocated statically. For example,
```
    interface <$interfaceA> { # this interface extends interfaceA
        @s1 int32,            # data fields inside interfaces always statically allocated
        &method1(int32) f32 } # a method declaration
```
MAPLE IR handles an interface declaration as a type declaration. Thus,
the above can be specified after the type keyword to be associated with
a type name. Separately, the **javainterface** keyword declares the
symbol associated with the interface:

syntax: javainterface \<id-name\> \<interface-type\> \<attributes\>

\<id-name\> must have \'\$\' as prefix as interface names always have
global scope. For example:
```
    # $IFA is an interface with a single method &amethod
    javainterface $IFA <interface {&amethod(void) int32}> public static
```
Again, a **javainterface** name should not be regarded as a type name,
as it is a symbol name. When a class implements an interface, it
specifies the **javainterface** name as part of its comma-separated
contents, as in:
```
    class <$PP> { &amethod(void) int32, # this class extends the $PP class, and
                                # &amethod is a member function of this class
                  $IFA }        # this class implements the $IFA interface
```
Function Declaration
--------------------

syntax:
```
    func <func-name> <attributes> (
      var <parm0> <type>,
      ...
      var <parmn> <type>) <ret-type0>, . . . <ret_typen> {
      <stmt0>
      ...
      <stmtn> }

```
\<attributes\> provides various attributes about the function, like
static, extern, etc. The opening parentheses starts the parameter
declarations, which can be empty. Each \<parm~i~\> is of the form of a
**var** or **reg** declaration declaring each incoming parameter. If the
last parameter is specified as \"\...\", it indicates the start of the
variable part of the arguments as in C. Following the parameter
declarations is a list of the multiple return types separated by commas.
If there is no return value, \<ret-type0\> should specify void. Each
\<ret-type~i~\> can be either primitive or derived. If no statement
follows the parentheses, then it is just a prototype declaration.

### funcsize

syntax: funcsize \<size-in-bytes\>

This directive appears inside the function block to give the Maple IR
code size of the function.

### framesize

syntax: framesize \<size-in-bytes\>

This directive appears inside the function block to give the stack frame
size of the function.

### moduleid

syntax: moduleid \<id\>

This directive appears inside the function to give the unique id of the
module which the function belongs to.

### upformalsize

syntax: upformalsize \<size-in-bytes\>

This directive appears inside the function block to give the size of
upformal segment that stores the formal parameters being passed above
the frame pointer %%FP.

### formalwordstypetagged

syntax: formalwordstypetagged = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the Nth word in the upformal segment has a type
tag, in which case the type tag is at the (N+1)th word.

### formalwordsrefcounted

syntax: formalwordsrefcounted = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the Nth word in the upformal segment is a
pointer to a reference-counted dynamically allocated memory block.

### localwordstypetagged

syntax: localwordstypetagged = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the -Nth word in the local stack frame has a
type tag, in which case the type tag is at the (-N+1)th word.

### localwordsrefcounted

syntax: localwordsrefcounted = \[ \<word-values\> \]

This specifies a bitvector initialized to the value specified by the
list of space-separated 32-bit integer constants. The Nth bit in this
bitvector is set to 1 if the -Nth word in the local stack frame is a
pointer to a reference-counted dynamically allocated memory block.

Initializations
---------------

When there are initializations associated with a **var** declaration,
there is \'=\' after the **var** declaration, followed by the
initialization value. For aggregates, the list of initialization values
are enclosed by brackets \'\[\' and \'\]\', with the values separated by
commas. In arrays, the initialization values for the array elements are
listed one by one, and nested brackets must be used to correspond to
elements in each lower-order dimension.

In specifying the initializations for a struct, inside the brackets,
field-ID followed by \'=\' must be used to specify the value for each
field explicitly. The fields\' initialization values can be listed in
arbitrary order. For nested structs, the usage of nested brackets is
optional, because field-IDs at the top level can uniquely specify fields
in nested structs. But if nested brackets are used, then field-ID usage
within the nested brackets is relative to that corresponding level of
sub-struct. Example:
```
    type %SS <struct { @g1 f64, @g2 f64 }>
    var %s struct{@f1 i32,
                  @f2 <%SS>,
                  @f3:4 i32} = [
        1 = 99,
        2 = [1= 10.0], # field f2.g1 has field ID 1 in struct %SS and
                       # is initialized to 10.0
        4 = 22.2,      # field f2.g2 has field ID 4 in struct %s and
                       # is initialized to 22.2
        5 = 15 ]       # field f3 (4 bits in size) has field ID 5 in struct %s
                       # and is initialized to 15
```
 Type Parameters
---------------

Also called generics or templates, type parameters allow derived types
and functions to be written without specifying the exact types in parts
of their contents. The type parameters can be instantiated to different
specific types later, thus enabling the code to be more widely
applicable, promoting software re-use.

Type parameters and their instantiation can be handled completely by the
language front-ends. MAPLE IR provides representation for generic types
and generic functions and their instantiation so as to reduce the amount
of work in the language front-ends. A MAPLE IR file with type parameters
requires a front-end lowering phase to de-genericize the IR before the
rest of the MAPLE IR components can process the code.

Type parameters are symbol names prefixed with \"!\", and can appear
anywhere that a type can appear. Each type or function definition can
have multiple type parameters, and each type parameter can appear more
than one time. Since type parameters are also types, they can only
appear inside the angular brackets \"\<\" and \"\>\", e.g. \<!T\>. When
the definition of a derived type contains any type parameter, the type
becomes a generic type. When the definition of a function contains any
type parameter, the function becomes a generic function. A function
prototype cannot contain generic type.

A generic type or generic function is marked with the generic attribute
to make them easier to be identified. A generic type or function is
instantiated by assigning specific non-generic types to each of its type
parameters. The instantiation is specified by a list of such assignments
separated by commas. We refer to this as an instantiation vector, which
is specified inside braces \"{\" and \"}\". In the case of the
instantiation of a generic type, the type parameter is immediately
followed by the instantiation vector. Example:
```
    type $apair <struct {@f1 <!T>, @f2 <!T>}> # $apair is a generic type

    var $x <$apair{!T=f32}>     # the type of $x is $apair instantiated with
                                # f32 being assigned to the type parameter !T
```
A generic function is instantiated by invoking it with an instantiation
vector. The instantiation vector immediately follows the name of the
generic function. Since the instantiation vector is regarded as type
information, it is further enclosed inside the angular brackets \"\<\"
and \"\>\". Invocation of generic functions must be via the opcodes
callinstant and callinstantassigned, which correspond to call and
callassigned respectively. Example:
```
    # &swap is a generic function to swap two parameters
    func &swap (var %x \<!UU\>, var %y \<!UU\>) void {
      var %z \<!UU\>
      dassign %z (dread agg %x)
      dassiign %x (dread agg %y)
      dassign %y (dread agg %z)
      return
    }

    ...

      # &swap is instantiated with type argument <$apair{!T=i32}>,
      # itself an instantiated type
      callinstant &swap<{!UU=<$apair{!T=i32}>}> (
      dread agg %a,
      dread agg %b)
```
