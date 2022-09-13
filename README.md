# S-lang

The [WebScheme] compiler.

Since most of the Wasm proposals on which this will depend
are still under development,
this project is still very much in the imaginative phase.
Most of the implementation
is currently focused on parsing and macro expansion.

GitHub stars appreciated!

[WebScheme]: https://web-scheme.org

## Contributing

Join the [mailing list] to get updates and ask questions.
Frankly, it'll probably be pretty quiet for a while,
but hoping to pick up the pace as WASM matures.

The source code is mostly written using [sweet-expressions].
Only standard R⁷RS-small
or basic R⁷RS-large assumptions (e.g. [intermediate hash tables]).
Some wrappers are used for bootstrapping with Guile.

The implementation for the standard library lives in [`lib/scheme/base`].

[mailing list]: https://groups.google.com/a/web-scheme.org/g/dev
[sweet-expressions]: https://srfi.schemers.org/srfi-110/srfi-110.html
[intermediate hash tables]: https://srfi.schemers.org/srfi-125/srfi-125.html
[`lib/scheme/base`]: lib/scheme/base
