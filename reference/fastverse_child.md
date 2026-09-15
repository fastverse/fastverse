# Create a new (descendant) verse of packages

Creates and installs a fully customizable descendant verse of packages
that is itself extensible and inherits 90% of the functionality of the
`fastverse` package.

## Usage

``` r
fastverse_child(
  name,
  title,
  pkg,
  maintainer,
  version = "0.1.0",
  dir = ".",
  theme = c("fastverse", "tidyverse"),
  install = TRUE,
  keep.dir = TRUE,
  ...
)
```

## Arguments

- name:

  character. The name of the child-verse e.g. 'myMLverse'.

- title:

  character. The title of the child-verse e.g. 'My Machine Learning
  Verse'.

- pkg:

  character. A vector of core packages for the new verse.

- maintainer:

  character. A quoted [`person`](https://rdrr.io/r/utils/person.html)
  statement giving the package maintainer. If you do not intend to
  substantially modify the code of the resulting source package,
  `role = "cre"` is appropriate. See Examples.

- version:

  character. A quoted version number for the new package.

- dir:

  character. The directory in which a source directory dir/name for the
  package will be created.

- theme:

  character. Set the colour-theme for text printed to the console. The
  options are `"fastverse"` or `"tidyverse"`.

- install:

  logical. `TRUE` installs the package using
  `install.packages(dir/name, repos = NULL, type = "source", ...)`.

- keep.dir:

  logical. `FALSE` will remove the source directory (dir/name) again
  after installation.

- ...:

  further arguments to
  [`install.packages`](https://rdrr.io/r/utils/install.packages.html).

## Value

`fastverse_child` returns `NULL` invisibly.

## Details

This function creates and installs a source package according to the
users specification. For that it downloads the 'child' branch of the
[GitHub repository](https://github.com/fastverse/fastverse), which was
specifically set up to produce a new verse, unzips it into a source
directory, and substitutes the user inputs into the files. The package
is then installed from source, and (optionally) the source directory is
removed again.

*fastverse* children inherit 90% of the functionality of the *fastverse*
package: they are not permanently globally extensible and can not bear
children themselves, but can be configured for projects (using a `.name`
config file) and extended in the session. Use of this function requires
an internet connection but no additional R packages (like `devtools`,
`remotes` or `roxygen2`).

## See also

[`fastverse_extend`](fastverse_extend.md),
[`fastverse`](fastverse-package.md)

## Examples

``` r
if (FALSE) { # \dontrun{

fastverse_child(
  name = "tsverse", 
  title = "Time Series Package Verse", 
  pkg = c("xts", "roll", "zoo", "tsbox", "urca", "tseries", "tsutils", "forecast"), 
  maintainer = 'person("GivenName", "FamilyName", role = "cre", email = "your@email.com")',
  dir = "C:/Users/.../Documents", 
  theme = "tidyverse")
} # }
```
