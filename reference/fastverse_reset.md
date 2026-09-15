# Reset the fastverse to defaults

Calling this function will remove global configuration files and
(default) clear all package options. Attached packages will not be
detached, and configuration files for projects (as discussed in the
vignette) will not be removed.

## Usage

``` r
fastverse_reset(options = TRUE)
```

## Arguments

- options:

  logical. `TRUE` also clears all *fastverse* options.

## Value

`fastverse_reset` returns `NULL` invisibly.

## See also

[`fastverse_extend`](fastverse_extend.md),
[`fastverse`](fastverse-package.md)
