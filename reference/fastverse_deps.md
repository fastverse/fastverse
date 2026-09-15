# List all fastverse dependencies

Lists all *fastverse* dependencies and the local and repository (e.g.
CRAN) versions of packages and dependencies.

## Usage

``` r
fastverse_deps(
  pkg = fastverse_packages(),
  recursive = FALSE,
  repos = getOption("repos"),
  include.self = FALSE,
  check.deps = TRUE
)
```

## Arguments

- pkg:

  character vector of packages to check dependencies and versions of.
  The default is all *fastverse* packages.

- recursive:

  logical. `TRUE` recursively determines all packages required to
  operate these packages. `FALSE` will only list the packages and their
  direct dependencies.

- repos:

  character vector. Base URL(s) of the repositories to use, e.g., the
  URL of a CRAN mirror such as `"https://cloud.r-project.org"`. The
  macro `.fastverse_repos` contains the URL of the [fastverse r-universe
  server](https://fastverse.r-universe.dev) to check/install the
  development version of packages.

- include.self:

  logical. `TRUE` also includes the *fastverse* package and checks
  against the repository version.

- check.deps:

  logical. `FALSE` will not determine dependencies but only display the
  update status of packages in `pkg`.

## Value

A data frame giving the package names, the repository and local version,
and a logical variable stating whether the local version is behind the
repository version.

## See also

[`fastverse_sitrep`](fastverse_sitrep.md),
[`fastverse`](fastverse-package.md)
