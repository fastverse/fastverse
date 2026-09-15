# Detach (fastverse) packages

Detaches (*fastverse*) packages, removing them from the
[`search`](https://rdrr.io/r/base/search.html) path.

## Usage

``` r
fastverse_detach(
  ...,
  unload = FALSE,
  force = FALSE,
  include.self = TRUE,
  session = FALSE,
  permanent = FALSE
)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names. If left empty, all packages returned by
  [`fastverse_packages`](fastverse_packages.md) are detached.

- unload:

  logical. `TRUE` also unloads the packages using
  [`detach(name, unload = TRUE)`](https://rdrr.io/r/base/detach.html).

- force:

  logical. should a *fastverse* package be detached / unloaded even
  though other attached packages depend on it?

- include.self:

  logical. `TRUE` also includes the `fastverse` package - only
  applicable if `...` is left empty.

- session:

  logical. `TRUE` also removes the packages from
  `options("fastverse.extend")`, so they will not be attached again with
  [`library(fastverse)`](https://fastverse.github.io/fastverse/) in the
  current session. If `...` is left empty and `include.self = TRUE`,
  this will clear **all** *fastverse* options set for the session.

- permanent:

  logical. if `...` are used to detach certain packages,
  `permament = TRUE` will disable them being loaded the next time the
  *fastverse* is loaded. This is implemented via a config file saved to
  the package directory. Core *fastverse* packages can also be detached
  in this way. To add a package again use
  `extend_fastverse(..., permanent = TRUE)`. The config file can be
  removed with [`fastverse_reset`](fastverse_reset.md).

## Value

`fastverse_detach` returns `NULL` invisibly.

## See also

[`fastverse_extend`](fastverse_extend.md),
[`fastverse`](fastverse-package.md)
