# Update fastverse packages

This will check all *fastverse* packages (and their dependencies) for
updates and (optionally) install those updates.

## Usage

``` r
fastverse_update(..., install = FALSE, repos = getOption("repos"))
```

## Arguments

- ...:

  arguments passed to [`fastverse_deps`](fastverse_deps.md).

- install:

  logical. `TRUE` will proceed to install outdated packages, whereas
  `FALSE` (recommended) will print the installation command asking you
  to run it in a clean R session.

- repos:

  character vector. Base URL(s) of the repositories to use, e.g., the
  URL of a CRAN mirror such as `"https://cloud.r-project.org"`. The
  macro `.fastverse_repos` contains the URL of the [fastverse r-universe
  server](https://fastverse.r-universe.dev) to check/install the
  development version of packages.

## Value

`fastverse_update` returns `NULL` invisibly.

## See also

[`fastverse_deps`](fastverse_deps.md),
[`fastverse`](fastverse-package.md)

## Examples

``` r
if (FALSE) { # \dontrun{
## Update from CRAN
fastverse_update()

## Update from R-Universe (development versions)
fastverse_update(repos = .fastverse_repos)
} # }
```
