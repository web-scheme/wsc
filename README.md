
**S-lang** is a [Scheme][1] compiler and interpretter.
It aims to:

- Faithfully conform to the latest RⁿRS specification
  (currently [R⁷RS][2], including R⁷RS-large once ratified).
- Prioritize [WebAssembly][3] as a compilation target and FFI.

[1]: https://schemers.org
[2]: https://small.r7rs.org/attachment/r7rs.pdf
[3]: https://webassembly.org

## WebAssembly Target

S-lang compiles Scheme code into WebAssembly modules.
This allows run-time or compile-time linkage to other WebAssembly modules,
including those implemented in foreign languages
with appropriate [interface types][4].

This goal may sometimes conflict with faithful conformity to the Scheme spec.
In such cases, minimal out-of-spec features may be added,
but no in-spec feature removed or altered.
Any Scheme program that does *not* make use of WebAssembly-specific functionality
must be expressable using only in-spec features

[4]: https://github.com/WebAssembly/interface-types/blob/main/proposals/interface-types/Explainer.md

### Type ABI

All values in s-lang are represented in wasm as `i64`.
Values are either primitives, or garbage-collected non-primitives.
The most significant bits indicate information about the type
according to this table:

| bits       | meaning                                        |
| :--------- | :--------------------------------------------- |
| `01`       | float (62 bit)                                 |
| `10`       | integer (62 bit)                               |
| `11`       | rational (62 bit)                              |
| `000`      | non-primitive (8-byte-aligned pointer) or null |
| `00100000` | boolean                                        |
| `00100001` | character (UTF-32)                             |
| `00100010` | file descriptor (32-bit integer)               |
| `00100011` | EOF object                                     |

If the value's 3 most significant bits are zero,
it's either a non-primitive or null (`()`).
It is multiplied by 8 and interpretted as a pointer into the s-lang heap.
S-lang objects are always 8-byte-aligned.
If zero, it's null.

There are 3 pseudo-64-bit primitives (62-bit floats, integers, and rationals)
and anything else is at most 56 bits (7 bytes),
leaving the 5 least significant bits of the most significant byte
to differentiate between the various "small" (sub-8-byte) primitives.

#### But what if I need 64 bits!?

Having 62-bit number primitives means losing the 2 least significant bits
of the 52-bit fractional part of an IEEE 754 [*binary64*][5].
It means shrinking the range of integers by 75%
compared to what a "true 64-bit" system would be able to handle.

62 bits is enough for 99% of anticipated use-cases,
and it's well within Scheme's loose [number implementation restrictions][6].
The alternative would be to implement all numbers as non-primitives,
requiring indirection and garbage collection.
When needed,
true 64-bit numbers — including complex numbers — or compact buffers thereof
are indeed implemented as non-primitive objects.
<!-- TODO: Provide examples when they exist. -->

[5]: https://en.wikipedia.org/wiki/Double-precision_floating-point_format
[6]: https://small.r7rs.org/attachment/r7rs.pdf#subsection.6.2.3

#### Non-primitives

|         ID | meaning    |
| ---------: | :--------- |
|          0 | procedure  |
|          1 | pair       |
|          2 | symbol     |
|          3 | string     |
|          4 | vector     |
|          5 | bytevector |
|          6 | promise    |

<!-- OLD STUFF:

#### Example

```scheme
(define null '())
(define symbol 'simble)
(define boolean #t)
(define char #\x3F)
(define integer 8)
(define real 4.2)
(define bytevector #u8(1 2 3))
(define string "A Unicode string ☺")
(define vector #(1 2 3))
(define pair (cons 1 (cons 2 3)))
(define eof-object TODO)  ;;  <- Is this necessary?
(define port TODO)        ;;  <- Is this necessary?
(define (foo x y) (+ x y))
(define-record-type recordt TODO)

(export null symbol boolean
        char integer real
        bytevector string vector pair
        eof-object port foo recordt)

(display "This is top-level code.\n")
```

This produces a WASM module that looks like this:

```
(module
  (func $foo (param $x i64) (param $y i64) (result i64)
    local.get $x
    local.get $y
    i32.add)
  (export "foo" (func $foo)))
```

### Resolving Dissonance

> Scheme is dynamically typed whereas WebAssembly is statically typed.

This affects s-lang's ability to import and export WebAssembly functions and globals.
Basic imports can be handled without deviating from the WASM spec.

> Scheme enables dynamic function *definition*
> whereas WebAssembly functions are strictly static.

In order to export a Scheme function so that foreign modules may invoke it,
it must be statically compiled.
Meanwhile, to support `eval`,
the s-lang runtime must be able to compile or interpret dynamic data as code.
It would be impractical to satisfy both use-cases with a unified concept of function,
so s-lang distinguishes between static and dynamic functions.

Use of `eval` and `read` in a compiled module
requires expensive scanning, parsing, and interpretting machinery.
These are the only built-in Scheme functions that are "dynamically linked"
via WebAssembly imports in the compiled output module.
This happens automatically and does not require an explicit `import`.

| Scheme type | WASM type | Note                                              |
| ----------: | :-------- | :------------------------------------------------ |
| Boolean     | `bytes`   | An array of integers between 0 and 255 inclusive. |
| Bytevector  | `bytes`   | An array of integers between 0 and 255 inclusive. |

-->
