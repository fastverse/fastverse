# Install (missing) fastverse packages

This function (by default) checks if any *fastverse* package is missing
and installs the missing package(s). The development versions of
*fastverse* packages can also be installed from
[r-universe](https://fastverse.r-universe.dev). The link to the
repository is contained in the `.fastverse_repos` macro.

## Usage

``` r
fastverse_install(
  ...,
  only.missing = TRUE,
  install = TRUE,
  repos = getOption("repos")
)
```

## Arguments

- ...:

  comma-separated package names, quoted or unquoted, or vectors of
  package names. If left empty, all packages returned by
  [`fastverse_packages`](fastverse_packages.md) are checked.

- only.missing:

  logical. `TRUE` only installs packages that are unavailable. `FALSE`
  installs all packages, even if they are available.

- install:

  logical. `TRUE` will proceed to install packages, whereas `FALSE`
  (recommended) will print the installation command asking you to run it
  in a clean R session.

- repos:

  character vector. Base URL(s) of the repositories to use, e.g., the
  URL of a CRAN mirror such as `"https://cloud.r-project.org"`. The
  macro `.fastverse_repos` contains the URL of the [fastverse r-universe
  server](https://fastverse.r-universe.dev) to check/install the
  development version of packages.

## Value

`fastverse_install` returns `NULL` invisibly.

## Note

There is also the possibility to set `options(fastverse.install = TRUE)`
before [`library(fastverse)`](https://fastverse.github.io/fastverse/),
which will call `fastverse_install()` before loading any packages to
make sure all packages are available. If you are using a `.fastverse`
configuration file inside a project (see vignette), you can also place
`_opt_fastverse.install = TRUE` before the list of packages in that
file.

## See also

[`fastverse_update`](fastverse_update.md),
[`fastverse`](fastverse-package.md)
