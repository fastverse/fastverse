# Conflicts between the fastverse and other packages

This function lists all the conflicts among *fastverse* packages and
between *fastverse* packages and other attached packages. It can also be
used to check conflicts for any other attached packages.

## Usage

``` r
fastverse_conflicts(pkg = fastverse_packages())
```

## Arguments

- pkg:

  character. A vector of packages to check conflicts for. The default is
  all *fastverse* packages.

## Value

An object of class 'fastverse_conflicts': A named list of character
vectors where the names are the conflicted objects, and the content are
the names of the package namespaces containing the object, in the order
they appear on the [`search`](https://rdrr.io/r/base/search.html) path.

## Details

There are 3 internal conflicts in the core *fastverse* which are not
displayed by `fastverse_conflicts()`:

- [`collapse::funique`](https://fastverse.org/collapse/reference/funique.html)
  and `collapse::fdupliacted` mask
  [`kit::funique`](https://fastverse.org/kit/reference/funique.html) and
  [`kit::fduplicated`](https://fastverse.org/kit/reference/funique.html).
  If both packages are detached, *collapse* is attached after *kit*. In
  general, the *collapse* versions are faster and a bit more versatile.
  The *kit* versions are also very fast and additionally supports
  matrices!

- collapse::fdroplevels masks
  [`data.table::fdroplevels`](https://rdrr.io/pkg/data.table/man/fdroplevels.html).
  The former is faster and supports arbitrary data structures, whereas
  the latter has options to exclude certain levels from being dropped.

## See also

[`fastverse`](fastverse-package.md)

## Examples

``` r
# Check conflicts between fastverse packages and all attached packages
fastverse_conflicts()
#> -- Conflicts ------------------------------------------ fastverse_conflicts() --
#> x data.table::%notin%() masks base::%notin%()

# Check conflicts among all attached packages
fastverse_conflicts(sub("package:", "", search()[-1], fixed = TRUE))
#> -- Conflicts ------------------------------------------ fastverse_conflicts() --
#> x data.table::%notin%() masks base::%notin%()
#> x methods::body<-()     masks base::body<-()
#> x methods::kronecker()  masks base::kronecker()
```
