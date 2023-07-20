# Bootstrapping WebScheme

The idea is to implement the whole thing in the `src` folder in Guile,
fork the bootstrap-ready Guile code into a temporary folder,
tweak the existing `src` tree to better fit R7RS (module system),
and use the forked Guile code to compile the refactored `src` code.

## Get Started

[Install Guile](https://www.gnu.org/software/guile/download/)
version 3.0.5 or later.
