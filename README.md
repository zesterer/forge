# Forge

Forge is a dynamically-typed language written in Rust. It is inspired by JavaScript, Rust, Python and [Rhai](https://github.com/jonathandturner/rhai).
In the future, you'll be able to use Forge as a general-purpose lightweight scripting language in your applications.

[You can try out Forge v1 in your browser here!](https://forge.jsbarretto.com)

## Example

```js
var a = 1;
var b = 1;
var n = 0;

while n < 10 {
	print "Fibonacci(" + (n + 1) + ") = " + a;

	var tmp = b;
	b = a;
	a = tmp + b;

	n = n + 1;
}
```

## Goals

- Simple, familiar syntax
- Lightweight, quick to parse
- Moderately fast execution speeds
- Well-considered, 'common-sense' design
- Easy to build into an existing codebase
- Python-like REPL prompt

## Design

### Types

Forge has several distinct types:

- Numbers *64-bit, floating-point*
- Strings *unicode-compliant*
- Booleans
- Functions *Currently only in Forge v1*
- Lists *Currently unimplemented*
- Maps *Currently unimplemented*
- Objects *Currently unimplemented*
- Null

### Interpreter

Currently, Forge is only implemented as an AST-walking interpreter.
In the future, I aim to generate more efficient low-level bytecode for the language.
I also aim to implement many a variety of optimisations throughout the compilation process.
