# Libraries

Implementations for WSC's built-in libraries.

Each library is defined by a Meson subproject.
The hypothetical library `(a b c)` would exist in the subdirectory `a/b/c/`,
with its main build file at `a/b/c/meson.build`
and its build directory at `a/b/c/build`.
When built, it would produce a standalone WASM module at `a/b/c/build/c.wasm`.
