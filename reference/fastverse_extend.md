# Extend the fastverse

Loads additional packages as part of the *fastverse*. By default only
for the session, but extensions can be saved up to
reinstallation/updating of the *fastverse* package.

## Usage

``` r
fastverse_extend(
  ...,
  install = FALSE,
  permanent = FALSE,
  check.conflicts = !isTRUE(getOption("fastverse.quiet")),
  topics = NULL,
  repos = getOption("repos")
)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names.

- install:

  logical. Install packages not available?

- permanent:

  logical. Should packages be saved and included when
  [`library(fastverse)`](https://fastverse.github.io/fastverse/) is
  called next time? Implemented via a config file saved to the package
  directory. The file will be removed if the *fastverse* is reinstalled,
  and can be removed without reinstallation using
  [`fastverse_reset`](fastverse_reset.md). Packages can be removed from
  the config file using
  [`fastverse_detach(..., permanent = TRUE)`](fastverse_detach.md).

- check.conflicts:

  logical. Should conflicts between extension packages and attached
  packages be checked?

- topics:

  depreciated argument used to bulk-attach or install suggested
  extension packages prior to v0.3.0.

- repos:

  character vector. Base URL(s) of the repositories to use, e.g., the
  URL of a CRAN mirror such as `"https://cloud.r-project.org"`. The
  macro `.fastverse_repos` contains the URL of the [fastverse r-universe
  server](https://fastverse.r-universe.dev) to check/install the
  development version of packages.

## Value

`fastverse_extend` returns `NULL` invisibly.

## Details

The *fastverse* can be extended using a free choice of packages. An
overview of high-performing packages for various tasks is provided in
the
[README](https://github.com/fastverse/fastverse#suggested-extensions)
file.

When the *fastverse* is extended calling `fastverse_extend(...)`, the
packages that are not attached are attached, but conflicts are checked
for all specified packages. If `permanent = FALSE`, an
`options("fastverse.extend")` is set which stores these extension
packages, regardless of whether they were already attached or not. When
calling [`fastverse_packages`](fastverse_packages.md),
[`fastverse_deps`](fastverse_deps.md),
[`fastverse_conflicts`](fastverse_conflicts.md),
[`fastverse_update`](fastverse_update.md),
[`fastverse_sitrep`](fastverse_sitrep.md) or
[`fastverse_detach`](fastverse_detach.md), these packages are included
as part of the *fastverse*. This is also the case if `permanent = TRUE`,
with the only difference that instead of populating the option, a file
is saved to the package directory such that the packages are also loaded
(as part of the core *fastverse*) when calling
[`library(fastverse)`](https://fastverse.github.io/fastverse/) in the
next session. To extend the *fastverse* for the current session when it
is not yet loaded, users can also set
`options(fastverse.extend = c(...))`, where `c(...)` is a character
vector of package names, before calling
[`library(fastverse)`](https://fastverse.github.io/fastverse/).

## See also

[`fastverse_detach`](fastverse_detach.md),
[`fastverse`](fastverse-package.md)

## Examples

``` r
# fastverse_extend(xts, stringi, fasttime)
```
