# Bootstrapping WebScheme

The idea is to implement the whole thing in the `src` folder in Guile,
fork the bootstrap-ready Guile code into a temporary folder,
tweak the existing `src` tree to better fit R7RS (module system),
and use the forked Guile code to compile the refactored `src` code.

## Get Started

[Install Guix],
then set up your development environment:

```
guix environment --manifest=bootstrap/manifest.scm
```

[Install Guix]: https://guix.gnu.org/manual/en/html_node/Installation.html
