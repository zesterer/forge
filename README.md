# Forge

Forge is a dynamically-typed language written in Rust. It is inspired by JavaScript, Rust, Python and [Rhai](https://github.com/jonathandturner/rhai).
In the future, you'll be able to use Forge as a general-purpose lightweight scripting language in your applications.

[You can try out Forge v1 in your browser here!](https://forge.jsbarretto.com)

## Example

```js
var n = input "Enter a number: ";

var c = 1;
while c <= n {
	print "square(" + c + ") = " + c * c;
	c = c + 1;
}
```

## Goals

- Simple, familiar syntax
- Lightweight, quick to parse
- Moderately fast execution speeds
- Well-considered, 'common-sense' design
- Useful, informative error messages
- Easy to build into an existing codebase
- Python-like REPL prompt

## Usage

Using Forge is similar in principle to using Python.
Once compiled, running the shell or executing scripts with the interpreter is trivial.

To access the REPL shell, run:

```
$ forge
```

To execute a script, run:

```
$ forge my_script.fg
```

## Design

### Types

Forge has several distinct types:

- Number *64-bit, floating-point*
- String *unicode-compliant*
- Boolean
- Function *Currently only in Forge v1*
- List *Currently unimplemented*
- Map *Currently unimplemented*
- Object *Currently unimplemented*
- Custom *Used to call to and from Rust*
- Null

### Interpreter

Currently, Forge is only implemented as an AST-walking interpreter.
In the future, I aim to generate more efficient low-level bytecode for the language.
I also aim to implement many a variety of optimisations throughout the compilation process.

### Error Messages

Forge aims to produce the most useful, informative and intelligence error messages it can.
Errors can be emitted at compile-time or run-time. Below are a few examples.

```
>> var x = 1; if x > 2 { print "Hello, world!" oops; }
[ERROR] Parsing error at 1:45...
   ...while parsing if-else statement...
   ...while parsing print statement...
   |1:45| var x = 1; if x > 2 { print "Hello, world!" oops; }
                                                      ^^^^
   Expected ';' (did you forget to add one on the previous line?), found identifier 'oops'.
```

```
>> var p = true; while p { print "On the next iteration, p will be null"; p = null; }
On the next iteration, p will be null
[ERROR] Runtime error at 1:21...
   |1:21| var p = true; while p { print "On the next iteration, p will be null"; p = null; }
                              ^
   Cannot determine the truthiness of value of type 'null'. Did you mean for this to be a bool?
```
