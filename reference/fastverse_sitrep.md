# Get a situation report on the fastverse

This function gives a quick overview of the version of R and all
*fastverse* packages (including availability updates for packages) and
indicates whether any global or project-level configuration files are
used (as described in more detail the vignette).

## Usage

``` r
fastverse_sitrep(...)
```

## Arguments

- ...:

  arguments other than `pkg` passed to
  [`fastverse_deps`](fastverse_deps.md).

## Value

`fastverse_sitrep` returns `NULL` invisibly.

## See also

[`fastverse_deps`](fastverse_deps.md),
[`fastverse`](fastverse-package.md)
