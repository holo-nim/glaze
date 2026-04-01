# glaze

Serialization/deserialization between typed `NimNode`s and compile time values. Use cases include:

* storing values to/loading values from [macrocaches](https://nim-lang.org/docs/macrocache.html)
* reading `static`/`const` values from custom pragma argument nodes or other typed AST

Unlike `macros.newLit` (which only does serialization), AST is generated to be "reversible" via the deserialization procs. Like `macros.newLit`, the serialized AST also works as literals for the serialized values. Deserialization also supports the AST of typed static values/constants, which is not always valid AST in normal code.

Serialization/deserialization of typed `NimNode`s themselves are also supported, however this is accomplished via a cache that does not shrink, so it is not meant to be overused. Untyped `NimNode`s can use something like `macros.astGenRepr` or the [nuance](https://github.com/metagn/nuance) library which also works at runtime.
