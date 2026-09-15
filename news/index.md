# Changelog

## fastverse 0.3.4

CRAN release: 2024-10-08

- Fixed `"width"` issue during print if `getOption("width")` is too
  narrow. Thanks [@realzhang](https://github.com/realzhang)
  ([\#115](https://github.com/fastverse/fastverse/issues/115)).

## fastverse 0.3.3

CRAN release: 2024-05-30

- All unnecessary suggested packages are removed to avoid unneeded
  installations on servers.

## fastverse 0.3.2

CRAN release: 2023-09-20

- following CRAN suggestion, the `fastverse.Rd` page is renamed to
  `fastverse-package.Rd`, with a `'fastverse'` alias.

- [`fastverse_deps()`](../reference/fastverse_deps.md) (and
  [`fastverse_update()`](../reference/fastverse_update.md),
  [`fastverse_sitrep()`](../reference/fastverse_sitrep.md)) ignore
  dependencies not available on CRAN (issuing a warning).

## fastverse 0.3.1

CRAN release: 2023-01-29

- Hides another core *fastverse* conflict between a new function
  [`collapse::fduplicated()`](https://fastverse.org/collapse/reference/funique.html)
  and
  [`kit::fduplicated()`](https://fastverse.org/kit/reference/funique.html).
  The conflict is solved in favor of the new *collapse* version, which
  is faster and can also indicate all occurrences of duplicated values.
  [`kit::fduplicated()`](https://fastverse.org/kit/reference/funique.html)
  on the other hand can determine duplicates starting from the last
  value, and also supports matrices.

## fastverse 0.3.0

CRAN release: 2022-11-15

- Packages *matrixStats* and *fst* are removed from the core set of
  packages following a poll on Twitter in which more than 50% of
  respondents voted in favor. This makes the *fastverse* package lighter
  and focused on core data manipulation tasks. Users have many options
  to keep the original extent of the fastverse, such as (1) calling
  `fastverse_extend(matrixStats, fst, permanent = TRUE)` once following
  the update, (2) calling `fastverse_extend(matrixStats, fst)` right
  after
  [`library(fastverse)`](https://fastverse.github.io/fastverse/), (3)
  placing a `.fastverse` configuration file with `matrixStats, fst` in
  the project directory, or (4) setting
  `options(fastverse.extend = c("matrixStats", "fst"))` in an
  `.Rprofile` file. See the
  [vignette](https://fastverse.github.io/fastverse/articles/fastverse_intro.html)
  for details. The GitHub and R-universe versions of *fastverse* retain
  their capability to correct some functions in *matrixStats* to make
  them preserve dimension names by default, whenever *matrixStats* is
  added as an extension. The CRAN version does not allow this feature.

- A global variable `.fastverse_repos` was added containing the
  repository URLs of the [fastverse
  r-universe](https://fastverse.r-universe.dev/), alongside additional
  `repos` arguments to functions
  [`fastverse_deps()`](../reference/fastverse_deps.md),
  [`fastverse_extend()`](../reference/fastverse_extend.md),
  [`fastverse_install()`](../reference/fastverse_install.md),
  [`fastverse_update()`](../reference/fastverse_update.md) and
  [`fastverse_sitrep()`](../reference/fastverse_sitrep.md). This allows
  users to easily install binaries of the development versions (or early
  GitHub releases) of *fastverse* packages (both core and suggested
  extensions) using e.g. `fastverse_update(repos = .fastverse_repos)`.

- An internal clash arising between a new `fdroplevels()` function
  introduced in *data.table* 1.14.5 and the existing function
  `fdroplevels()` introduced in *collapse* 1.4.0 (November 2020) is
  resolved in favor of the latter, which was tested to be faster, and
  supports both factors and arbitrary list-based objects (including
  data.table’s).
  [`data.table::fdroplevels()`](https://rdrr.io/pkg/data.table/man/fdroplevels.html)
  is only for factors, as *data.table* introduced a method
  `droplevels.data.table` to handle data.table’s. Both the function and
  the method however have additional arguments allowing the user to
  exclude certain unused levels from being dropped (in line with base
  R), which are missing in the *collapse* versions. Thus nothing changes
  for *fastverse* users which have been using
  [`collapse::fdroplevels()`](https://fastverse.org/collapse/reference/fdroplevels.html)
  thus far, and `droplevels.data.table` will continue to work.

- Function `fastvverse_install()` prints a message
  `"All fastverse packages installed"` if all *fastverse* packages are
  installed, unless `options(fastverse.quiet = TRUE)` is set. There is
  also the option to set `_opt_fastverse.install = TRUE` in a
  `.fastverse` configuration file, or
  `options(fastverse.install = TRUE)` in an `.Rprofile` file, such that
  calling [`library(fastverse)`](https://fastverse.github.io/fastverse/)
  will install any missing packages by calling `fastvverse_install()`
  before attempting to load packages. This however also displayed the
  message `"All fastverse packages installed"`, which is undesirable. So
  whenever `options(fastverse.install = TRUE)` is set, a call to
  `fastvverse_install()` will not print anything if all packages are
  available.

- New packages and 2 new topics: ‘Compiling R’ and ‘R Bindings to Faster
  Languages’ were added to [Suggested
  Extensions](https://github.com/fastverse/fastverse#suggested-extensions)
  list. In lieu of this the `topics` argument to
  [`fastverse_extend()`](../reference/fastverse_extend.md) was
  depreciated, as hard coding many suggested packages in the package
  source code itself is impractical. Please feel free to suggest further
  packages for the list, so that it becomes a valuable resource for
  everyone interested in high-performance in R.

## fastverse 0.2.4

CRAN release: 2022-05-31

- Binaries of core fastverse packages (and a few others, including the
  *fastverse* package itself) can now be installed from a newly created
  [fastverse r-universe](https://fastverse.r-universe.dev/). This
  ensures that Windows and Mac binaries of the development versions of
  these packages are always available, independent of their CRAN status.
  These binaries can be installed using
  `install.packages("fastverse", repos = "https://fastverse.r-universe.dev/")`.

- Adding *geos* and *dqrng*, *fastmap* and *fastmatch* to suggested list
  (thanks also to Grant McDermott and Alexander Fisher for suggesting).

## fastverse 0.2.3

CRAN release: 2022-02-17

- Added `options(fastverse.install = TRUE)` which can be set before
  [`library(fastverse)`](https://fastverse.github.io/fastverse/),
  triggering an internal call to
  [`fastverse_install()`](../reference/fastverse_install.md) - to make
  sure any missing packages are installed before loading them. In a
  `.fastverse` configuration file placed inside a project directory, you
  can also place `_opt_fastverse.install = TRUE` before the packages
  list.

## fastverse 0.2.2

CRAN release: 2022-02-16

- Added possibility to set global options and environment variables in
  project configuration files. See Vignette for details.

- Added new packages to [suggested packages
  list](https://fastverse.github.io/fastverse/#suggested-extensions):
  *rrapply*, *MatrixExtra* and *rsparse*.

- Package does not import any helper functions from *collapse* anymore,
  so that hard detaching with `fastverse_detach(unload = TRUE)` is
  possible for *collapse* as well.

## fastverse 0.2.1

CRAN release: 2021-09-02

- Remove invalid URL and lifecycle badge (package is now considered
  stable).

## fastverse 0.2.0

- Making sure no files are written to disc on CRAN servers.

- [`fastverse_update()`](../reference/fastverse_update.md) has an
  additional argument `install`. The default is `install = FALSE`, which
  will just print the installation command as before, asking you to run
  it in a clean R session, whereas `install = TRUE` will instead execute
  the command.

- Added a function
  [`fastverse_install()`](../reference/fastverse_install.md) to check
  package availability and install any missing *fastverse* packages
  before calling
  [`library(fastverse)`](https://fastverse.github.io/fastverse/). Useful
  especially for custom *fastverse* configurations (inside projects) to
  safeguard the availability of packages.

- Added a function
  [`fastverse_child()`](../reference/fastverse_child.md) (following a
  request), which allows users to create a wholly different
  package-verse like the *fastverse* - under a different name and with
  different core packages. Such *fastverse* children inherit 90% of the
  functionality of the *fastverse* package (e.g. they are not
  permanently globally extensible, but can be configured for projects
  and extended in the session). Childbearing requires an internet
  connection but no additional packages (like `devtools`).

- Export utility functions `is_installed` and `is_attached` to check if
  packages are installed / attached.

- Several additions to suggested extensions list: `qs`, `arrow`,
  `stringdist`, `stringfish`, `nanotime`.

## fastverse 0.1.8

CRAN release: 2021-08-12

CRAN required me to place return values on
[`fastverse_extend()`](../reference/fastverse_extend.md) and
[`fastverse_detach()`](../reference/fastverse_detach.md) - which do not
return anything.

## fastverse 0.1.7

Implementing various CRAN comments on 0.1.6 (small things), and setting
up synchronous development of CRAN and GitHub version through a
‘development’ branch which can be merged into both. Pull requests should
be sent to the ‘development’ branch.

## fastverse 0.1.6

CRAN rejected 0.1.5, which made sure *matrixStats* handles attributes
consistently whenever the *fastverse* is attached, because CRAN packages
are not allowed to modify the namespace of other packages. Thus 0.1.6
comes in two versions: A CRAN version which takes *matrixStats* as it
is, and a GitHub version which is the original 0.1.5. User who value a
consistent *matrixStats* that preserves dimension names in all functions
are recommended the GitHub version.

## fastverse 0.1.5

First *fastverse* CRAN submission on 7th August 2021. Development
started in Spring 2021.
