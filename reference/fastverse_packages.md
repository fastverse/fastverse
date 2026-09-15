# List all packages in the fastverse

Core packages are first fetched from a project-level configuration file
(if found), else from a global configuration file (if found), otherwise
the standard set of core packages is returned. In addition, if
`extensions = TRUE`, any packages used to extend the *fastverse* for the
current session (fetched from `getOption("fastverse.extend")`) are also
returned.

## Usage

``` r
fastverse_packages(extensions = TRUE, include.self = TRUE)
```

## Arguments

- extensions:

  logical. `TRUE` appends the set of core packages with all packages
  found in `options("fastverse.extend")`.

- include.self:

  logical. Include the *fastverse* package in the list?

## Value

A character vector of package names.

## See also

[`fastverse_extend`](fastverse_extend.md),
[`fastverse`](fastverse-package.md)

## Examples

``` r
fastverse_packages()
#> [1] "data.table" "magrittr"   "kit"        "collapse"   "fastverse" 
```
