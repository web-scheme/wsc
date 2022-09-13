# Bootstrapping WebScheme

The idea is to implement the whole thing in this `bootstrap` folder,
fork the bootstrap code into the top-level `src` folder with needed adjustments,
and use Guile to run the `bootstrap` code to compile the `src` code.

## Get Started

[Install Guix],
then set up your development environment:

```
guix environment --manifest=bootstrap/manifest.scm
```

[Install Guix]: https://guix.gnu.org/manual/en/html_node/Installation.html
