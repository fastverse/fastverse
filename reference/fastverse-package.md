# The fastverse

The *fastverse* is an extensible suite of R packages, developed
independently by various people, that jointly contribute to the
objectives of:

1.  Speeding up R through heavy use of compiled code (C, C++, Fortran)

2.  Enabling more complex statistical and data manipulation operations
    in R

3.  Reducing the number of dependencies required for advanced computing
    in R

Inspired by the `tidyverse` package, the `fastverse` package is a
flexible package loader and manager that allows users to put together
their own 'verses' of packages and load them with
[`library(fastverse)`](https://fastverse.github.io/fastverse/).

`fastverse` installs 4 core packages (`data.table`, `collapse`, `kit`
and `magrittr`) that provide native C/C++ code of proven quality, work
well together, and enable complex statistical computing and data
manipulation - with only `Rcpp` as an additional dependency.

`fastverse` also allows users to freely (and permanently) extend or
reduce the number of packages in the *fastverse*. An overview of
high-performing packages for various common tasks is provided in the
[README](https://github.com/fastverse/fastverse#suggested-extensions)
file. An overview of the package and the different ways to extend the
*fastverse* is provided in the
[vignette](https://fastverse.github.io/fastverse/articles/fastverse_intro.html).

## Functions in the `fastverse` Package

Functions to extend or reduce the number of packages in the
*fastverse* - either for the session or permanently - and to restore
defaults.

[`fastverse_extend()`](fastverse_extend.md)  
[`fastverse_detach()`](fastverse_detach.md)  
[`fastverse_reset()`](fastverse_reset.md)

Function to display conflicts for *fastverse* packages (or any other
attached packages)

[`fastverse_conflicts()`](fastverse_conflicts.md)

Function to update *fastverse* packages (and dependencies) and install
(missing) packages

[`fastverse_update()`](fastverse_update.md)  
[`fastverse_install()`](fastverse_install.md)

Utilities to retrieve the names of *fastverse* packages (and
dependencies), their update status, and produce a situation report

[`fastverse_packages()`](fastverse_packages.md)  
[`fastverse_deps()`](fastverse_deps.md)  
[`fastverse_sitrep()`](fastverse_sitrep.md)

Function to create a fully separate extensible meta-package/verse like
`fastverse`

[`fastverse_child()`](fastverse_child.md)

## *fastverse* Options

- `options(fastverse.quiet = TRUE)` will disable all automatic messages
  (including conflict reporting) when calling
  [`library(fastvsers)`](https://rdrr.io/r/base/library.html),
  [`fastverse_extend`](fastverse_extend.md),
  [`fastverse_update(install = TRUE)`](fastverse_update.md) and
  [`fastverse_install`](fastverse_install.md).

- `options(fastverse.styling = FALSE)` will disable all styling applied
  to text printed to the console.

- `options(fastverse.extend = c(...))` can be set before calling
  [`library(fastvsers)`](https://rdrr.io/r/base/library.html) to extend
  the fastverse with some packages for the session. The same can be done
  with the [`fastverse_extend`](fastverse_extend.md) function after
  [`library(fastvsers)`](https://rdrr.io/r/base/library.html), which
  will also populate `options("fastverse.extend")`.

- `options(fastverse.install = TRUE)` can be set before
  [`library(fastverse)`](https://fastverse.github.io/fastverse/) to
  install any missing packages beforehand. See also
  [`fastverse_install`](fastverse_install.md).

## *fastverse* Harmonizations

There are 3 internal clashes between
[`collapse::funique`](https://fastverse.org/collapse/reference/funique.html)
and [`kit::funique`](https://fastverse.org/kit/reference/funique.html),
[`collapse::fduplicated`](https://fastverse.org/collapse/reference/funique.html)
and
[`kit::fduplicated`](https://fastverse.org/kit/reference/funique.html),
and
[`collapse::fdroplevels`](https://fastverse.org/collapse/reference/fdroplevels.html)
and
[`data.table::fdroplevels`](https://rdrr.io/pkg/data.table/man/fdroplevels.html).
The *collapse* versions take precedence in all cases as they provide
greater performance.
