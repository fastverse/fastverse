# Introduction to the \*fastverse\*

The *fastverse* is an extensible suite of R packages, developed
independently by various people, that contribute to the objectives of:

- Speeding up R through heavy use of compiled code (C, C++, Fortran)
- Enabling more complex statistical and data manipulation operations in
  R
- Reducing the number of dependencies required for advanced computing in
  R

The *fastverse* installs 4 core packages (*data.table*, *collapse*,
*kit* and *magrittr*)[^1] that are (by default) attached with
[`library(fastverse)`](https://fastverse.github.io/fastverse/). These
packages were selected because they provide high quality compiled code
for most common statistical and data manipulation tasks, have carefully
managed APIs, jointly depend only on base R and *Rcpp*, and work
remarkably well together.

``` r

library(fastverse)
# -- Attaching packages ------------------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v data.table 1.15.4     v kit        0.0.17
# v magrittr   2.0.3      v collapse   2.0.15
```

The *fastverse* package then provides functionality familiar from the
*tidyverse* package, such as checking and reporting namespace clashes,
and utilities for updating packages, listing dependencies etc…

``` r

# Checking for any updates
fastverse_update()
# All fastverse packages up-to-date
```

A key feature of the *fastverse* is that it can be extended with other
packages that can easily be loaded into the current session and then
managed using the tools this package provides. Users are also encouraged
to make of use the features described in the remainder of this vignette
to permanently extend the *fastverse* or even create ‘verses’ of
packages that suit their personal analysis needs. A selection of
[suggested
packages](https://fastverse.github.io/fastverse/#suggested-extensions)
is provided on the website[^2].

## Extending the *fastverse* for the Session

After the core packages have been attached with
[`library(fastverse)`](https://fastverse.github.io/fastverse/), it is
possible to extend the *fastverse* for the current session by adding any
number of additional packages with
[`fastverse_extend()`](../reference/fastverse_extend.md). This will
attach the packages, displaying the package versions (useful for
replicability), and (by default) check for namespace clashes with
attached packages, as well as among the added packages.

``` r

# Extend the fastverse for the session
fastverse_extend(xts, roll, fasttime)
# -- Attaching extension packages --------------------------------------------------------------------- fastverse 0.3.2 --
# v xts      0.13.1     v fasttime 1.1.0 
# v roll     1.1.6
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x xts::first() masks data.table::first()
# x xts::last()  masks data.table::last()

# See that these are now part of the fastverse
fastverse_packages()
# [1] "data.table" "magrittr"   "kit"        "collapse"   "xts"        "roll"       "fasttime"   "fastverse"

# They are also saved in a like-named option 
options("fastverse.extend")
# $fastverse.extend
# [1] "xts"      "roll"     "fasttime"
```

All *fastverse* packages (or particular packages) can be detached using
`fastverse_detach`.

``` r

# Detaches all packages (including the fastverse) but does not (default) unload them
fastverse_detach()
```

For programming purposes it is also possible to pass vectors of packages
to both `fastverse_extend` and `fastverse_detach`[^3]. The defaults of
`fastverse_detach` are set such that detaching is very ‘light’. Packages
are not unloaded and all *fastverse* options set for the session are
kept.

``` r

# Extensions are still here ...
options("fastverse.extend")
# $fastverse.extend
# [1] "xts"      "roll"     "fasttime"

# Thus attaching the fastverse again will include them
library(fastverse)
# -- Attaching packages ------------------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v data.table 1.15.4     v xts        0.13.1
# v magrittr   2.0.3      v roll       1.1.6 
# v kit        0.0.17     v fasttime   1.1.0 
# v collapse   2.0.15
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x xts::first()          masks data.table::first()
# x xts::last()           masks data.table::last()
# x data.table::yearmon() masks zoo::yearmon()
# x data.table::yearqtr() masks zoo::yearqtr()
```

‘Harder’ modes of detaching can be achieved using arguments
`unload = TRUE` (and `force = TRUE`) to (forcefully) detach and unload
*fastverse* packages, and/or `session = TRUE` which will clear all
*fastverse* options set[^4].

``` r

# Detaching and unloading all packages and clearing options
fastverse_detach(session = TRUE, unload = TRUE)
```

`fastverse_detach` can also be used to detach any other attached
packages not part of the *fastverse*. It is recommended to load all
packages used in the current session through
[`fastverse_extend()`](../reference/fastverse_extend.md), to display any
conflicts when they are loaded and be able to keep track of the search
path with
[`fastverse_conflicts()`](../reference/fastverse_conflicts.md).

Since `options("fastverse.extend")` keeps track of which packages were
added to the *fastverse* for the current session, it is also possible to
set it before loading the *fastverse* e.g. 

``` r

options(fastverse.extend = c("qs", "fst"))
library(fastverse)
# -- Attaching packages ------------------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v data.table 1.15.4     v collapse   2.0.15
# v magrittr   2.0.3      v qs         0.25.5
# v kit        0.0.17     v fst        0.9.8
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x data.table::yearmon() masks zoo::yearmon()
# x data.table::yearqtr() masks zoo::yearqtr()

fastverse_detach(session = TRUE)
```

## Permanent Extensions

`fasvtverse_extend` and `fastverse_detach` both have an argument
`permanent = TRUE` which can be used to make these changes persist
across R sessions. This is implemented using a global configuration file
saved to the package directory[^5].

For example, suppose most of my work involves time series analysis, and
I would like to add *xts*, *zoo*, *anytime* and *roll* to my
*fastverse*. Let’s say I also don’t really need the switches and
parallel statistics provided by the *kit* package and I am completely
happy with the base pipe so I don’t need *magrittr*. Let’s finally say
that I don’t want `xts::first` and `xts::last` to mask
[`data.table::first`](https://rdrr.io/pkg/data.table/man/last.html) and
[`data.table::last`](https://rdrr.io/pkg/data.table/man/last.html).

Then I could permanently modify my *fastverse* as follows[^6]:

``` r

library(fastverse)
# -- Attaching packages ------------------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v data.table 1.15.4     v kit        0.0.17
# v magrittr   2.0.3      v collapse   2.0.15
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x data.table::yearmon() masks zoo::yearmon()
# x data.table::yearqtr() masks zoo::yearqtr()

# Adding extensions
fastverse_extend(xts, zoo, roll, anytime, permanent = TRUE)
# -- Attaching extension packages --------------------------------------------------------------------- fastverse 0.3.2 --
# v xts     0.13.1     v anytime 0.3.9 
# v roll    1.1.6
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x zoo::as.Date()         masks base::as.Date()
# x zoo::as.Date.numeric() masks base::as.Date.numeric()
# x xts::first()           masks data.table::first()
# x xts::last()            masks data.table::last()
# x data.table::yearmon()  masks zoo::yearmon()
# x data.table::yearqtr()  masks zoo::yearqtr()

# Removing some core packages
fastverse_detach(data.table, kit, magrittr, permanent = TRUE)

# Adding data.table again, so it is attached last
fastverse_extend(data.table, permanent = TRUE)
# -- Attaching extension packages --------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v data.table 1.15.4
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x data.table::first()   masks xts::first()
# x data.table::last()    masks xts::last()
# x data.table::yearmon() masks zoo::yearmon()
# x data.table::yearqtr() masks zoo::yearqtr()
```

To verify our modification, we can see the order in which the packages
are attached, and do a conflict check:

``` r

# This will be the order in which packages are attached
fastverse_packages(include.self = FALSE)
# [1] "collapse"   "xts"        "zoo"        "roll"       "anytime"    "data.table"

# Check conflicts to make sure data.table functions take precedence
fastverse_conflicts()
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x zoo::as.Date()         masks base::as.Date()
# x zoo::as.Date.numeric() masks base::as.Date.numeric()
# x data.table::first()    masks xts::first()
# x data.table::last()     masks xts::last()
# x data.table::yearmon()  masks zoo::yearmon()
# x data.table::yearqtr()  masks zoo::yearqtr()
```

Note that `options("fastverse.extend")` is still empty, because we have
written those changes to a config file[^7]. Now lets see if our
permanent modification worked:

``` r

# Detach all packages and clear all options
fastverse_detach(session = TRUE)
```

``` r

library(fastverse) 
# -- Attaching packages ------------------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'data.table' was built under R version 4.3.1
# v collapse   2.0.15     v roll       1.1.6 
# v xts        0.13.1     v anytime    0.3.9 
# v zoo        1.8.12     v data.table 1.15.4
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x zoo::as.Date()         masks base::as.Date()
# x zoo::as.Date.numeric() masks base::as.Date.numeric()
# x data.table::first()    masks xts::first()
# x data.table::last()     masks xts::last()
# x data.table::yearmon()  masks zoo::yearmon()
# x data.table::yearqtr()  masks zoo::yearqtr()
```

After this permanent modification, the *fastverse* can still be extend
for the session:

``` r

# Extension for the session
fastverse_extend(Rfast, coop)
# -- Attaching extension packages --------------------------------------------------------------------- fastverse 0.3.2 --
# Warning: package 'Rcpp' was built under R version 4.3.1
# v Rfast 2.1.0     v coop  0.6.3
# -- Conflicts ---------------------------------------------------------------------------------- fastverse_conflicts() --
# x Rfast::group()     masks collapse::group()
# x Rfast::transpose() masks data.table::transpose()

# These packages go here
options("fastverse.extend")
# $fastverse.extend
# [1] "Rfast" "coop"

# This fetches packages from both the file and the option
fastverse_packages()
# [1] "collapse"   "xts"        "zoo"        "roll"       "anytime"    "data.table" "Rfast"      "coop"       "fastverse"
```

As long as the current installation of the *fastverse* is kept, these
modifications will persist across R sessions. Needless to say this is
not ideal as reinstallation of the *fastverse* will remove the config
file. Therefore, the *fastverse* also offers a persistent and more
flexible mechanism to configure it inside projects.

## Custom *fastverse* Configurations for Projects

You can put together a custom collection of packages for a project, and
load / manage them with
[`library(fastverse)`](https://fastverse.github.io/fastverse/).

For this you need to include a configuration file named `.fastverse` (no
file extension) inside a project directory, and place inside that file
the names of packages to be loaded when calling
[`library(fastverse)`](https://fastverse.github.io/fastverse/)[^8]. Note
that **all** packages to be loaded as core *fastverse* for your project
need to be included in that file, in the order they should be attached.

In addition, you can set global options and environment variables,
either before or after the list of packages. Options must be prefixed
with `_opt_` and environment variables with `_env_`, and either must be
placed on separate lines. For example, including a `.fastverse` script
like this

    _opt_collapse_mask = c("manip", "helper")
    _opt_fastverse.install = TRUE

    data.table, kit, magrittr, collapse, qs, fixest, 
    ranger, robustbase, decompr

    _opt_max.print = 100
    _opt_kit.nThread = 4
    _env_NCRAN = TRUE

in a project directory, and placing
[`library(fastverse)`](https://fastverse.github.io/fastverse/) at the
top of an R script in the project will first set
`options(collapse_mask = c("manip", "helper), fastverse.install = TRUE)`[^9],
then attach all the packages in the order provided, and then set
`options(max.print = 100, kit.nThread = 4)` and
`Sys.setenv(NCRAN = TRUE)`. Note that packages can be spread across
multiple lines, but need to be together i.e. they cannot be separated by
options. Alternatively you can also use an `.Rprofile` file and set
*fastverse* options such as
`options(fastverse.extend = c(...packages...))` there. The advantage of
`.fastverse` files is that you can specify options to be loaded before
or after the loading of packages through
[`library(fastverse)`](https://fastverse.github.io/fastverse/)[^10], and
you can also prevent core packages from being loaded by excluding them
from the `.fastverse` file.

Using the *fastverse* to jointly load important packages and set
important options can facilitate package management inside projects and
serve as a bridge between loading packages individually and using more
rigorous package and namespace management solutions such as
[renv](https://github.com/rstudio/renv),
[conflicted](https://github.com/r-lib/conflicted),
[box](https://github.com/klmr/box) or
[import](https://github.com/rticulate/import).

At the most basic level, loading packages with the *fastverse* displays
the package versions and checks namespace conflicts, helping you spot
issues that might arise as packages are updated. It also allows you to
easily check the dependencies and update status of packages used in your
project with [`fastverse_sitrep()`](../reference/fastverse_sitrep.md),
and update if necessary with
[`fastverse_update()`](../reference/fastverse_update.md).

Using a config file in a project will ignore any global configuration as
discussed in the previous section. You can still extend the *fastverse*
inside a project session using `fastverse_extend` (or
`options(fastvers.extend = c(...packages...))` before
[`library(fastverse)`](https://fastverse.github.io/fastverse/)).

## Creating Separate Package-Verses

At last, you can also create wholly separate and fully customizable
verses - with the [`fastverse_child()`](../reference/fastverse_child.md)
function. Let’s say I would like to create a verse for time series
analysis that I want to keep separate from the *fastverse*. This is
easily done using e.g.

``` r

fastverse_child(
  name = "tsverse", 
  title = "Time Series Package Verse", 
  pkg = c("xts", "roll", "zoo", "tsbox", "urca", "tseries", "tsutils", "forecast"), 
  maintainer = 'person("GivenName", "FamilyName", role = "cre", email = "your@email.com")',
  dir = "C:/Users/.../Documents", 
  theme = "tidyverse")
```

By default (`install = TRUE`, `keep.dir = TRUE`) the package is
installed and a source directory is created under dir/name, allowing
further edits to the package. Such *fastverse* children inherit 90% of
the functionality of the *fastverse* package: they are not permanently
globally extensible and can not bear children themselves, but can be
configured for projects (using, in this case, a `.tsverse` config file
and options starting with ‘tsverse’) and extended in the session. The
function uses a prepared ‘child’ branch of the [GitHub
repository](https://github.com/fastverse/fastverse), and thus does not
require any further packages such as `devtools`.

## Dependencies, Situational Reports and Updating

Just like it’s *tidyverse* equivalent,
[`fastverse_deps()`](../reference/fastverse_deps.md) (recursively)
determines the joint dependencies of *fastverse* packages and also
checks local versions against CRAN versions.

``` r

# Recursively determine the joint dependencies of the current fastverse configuration
fastverse_deps(recursive = TRUE) # Returns a data frame
#          package      repos      local behind
# 1       collapse     2.0.14     2.0.15  FALSE
# 2            xts     0.13.2     0.13.1   TRUE
# 3            zoo     1.8.12     1.8.12  FALSE
# 4           roll      1.1.7      1.1.6   TRUE
# 5        anytime      0.3.9      0.3.9  FALSE
# 6     data.table     1.15.4     1.15.4  FALSE
# 7          Rfast      2.1.0      2.1.0  FALSE
# 8           coop      0.6.3      0.6.3  FALSE
# 9             BH   1.84.0.0   1.81.0.1   TRUE
# 10       lattice     0.22.6     0.21.8   TRUE
# 11          Rcpp     1.0.12     1.0.12  FALSE
# 12 RcppArmadillo 0.12.8.3.0 0.12.8.1.0   TRUE
# 13       RcppGSL     0.3.13     0.3.13  FALSE
# 14  RcppParallel      5.1.7      5.1.7  FALSE
# 15  RcppZiggurat      0.1.6      0.1.6  FALSE
```

Additional flexibility is offered by the `pkg` argument allowing
dependency and update status checks for any other packages.
[`fastverse_sitrep()`](../reference/fastverse_sitrep.md) displays the
same information in a more elegant printout, also showing the version of
R, and whether any global or project-level configuration files - as
discussed in previous sections - are used.

``` r

# Check versions and update status of packages and dependencies
fastverse_sitrep() # default is recursive = FALSE
# -- fastverse 0.3.2: Situation Report ------------------------------------------------------------------------ R 4.3.0 --
#  * Global config file: TRUE
#  * Project config file: FALSE
# -- Core packages ------------------------------------------------------------------------------------------------------- 
#  * collapse      (2.0.15)
#  * xts           (0.13.1 < 0.13.2)
#  * zoo           (1.8.12)
#  * roll          (1.1.6 < 1.1.7)
#  * anytime       (0.3.9)
#  * data.table    (1.15.4)
# -- Extension packages -------------------------------------------------------------------------------------------------- 
#  * Rfast         (2.1.0)
#  * coop          (0.6.3)
# -- Dependencies -------------------------------------------------------------------------------------------------------- 
#  * BH            (1.81.0.1 < 1.84.0.0)
#  * lattice       (0.21.8 < 0.22.6)
#  * Rcpp          (1.0.12)
#  * RcppArmadillo (0.12.8.1.0 < 0.12.8.3.0)
#  * RcppParallel  (5.1.7)
#  * RcppZiggurat  (0.1.6)
```

[`fastverse_update()`](../reference/fastverse_update.md) can be used to
(default) print an
[`install.packages()`](https://rdrr.io/r/utils/install.packages.html)
statement to update *fastverse* packages and dependencies, or to install
updates straight away (`install = TRUE`). In all three functions,
`check.deps = FALSE` can be specified to exclude dependencies of
*fastverse* packages, `recursive = TRUE` can be used to check all
dependencies, and `include.self = TRUE` can be used to also check for
updates of the `fastverse` package itself.

The development versions of all core and suggested *fastverse* packages
are also collected on an [r-universe
server](https://fastverse.r-universe.dev) at
<https://fastverse.r-universe.dev>. Mac/Windows binaries are built on
this server within 24h after a change to the main branch of the GitHub
repository. Since v0.3.0 the package exports a global macro
`.fastverse_repos` which contains the URL of this repository alongside
the CRAN repository URL (needed for non-*fastverse* dependencies).
Functions [`fastverse_deps()`](../reference/fastverse_deps.md),
[`fastverse_update()`](../reference/fastverse_update.md),
[`fastverse_install()`](../reference/fastverse_install.md) and
`fastverse_siteep()` have a `repos` argument which defaults to
`getOption("repos")` (CRAN), but users can set
`repos = .fastverse_repos` to check against / update / install
development versions of *fastverse* packages from r-universe.

``` r

# Check development versions on GitHub / r-universe, and install them if desired. 
fastverse_update(repos = .fastverse_repos) 
# The following packages are out of date:
# 
#  * xts           (0.13.1 -> 0.13.2.2)
#  * roll          (1.1.6 -> 1.1.8)
#  * anytime       (0.3.9 -> 0.3.9.5)
#  * data.table    (1.15.4 -> 1.15.99)
#  * BH            (1.81.0.1 -> 1.84.0.0)
#  * lattice       (0.21.8 -> 0.22.6)
#  * Rcpp          (1.0.12 -> 1.0.12.3)
#  * RcppArmadillo (0.12.8.1.0 -> 0.12.8.3.0)
# 
# Start a clean R session then run:
# install.packages(c("xts", "roll", "anytime", "data.table", "BH", "lattice", "Rcpp", 
# "RcppArmadillo"), repos = c(fastverse = "https://fastverse.r-universe.dev", CRAN = "https://cloud.r-project.org"))
```

## Other *fastverse* Options

Apart from `"fastverse.extend"`, the *fastverse* also has options
`"fastverse.install"`, `"fastverse.styling"` and `"fastverse.quiet"`.
Setting `options(fastverse.install = TRUE)` before
[`library(fastverse)`](https://fastverse.github.io/fastverse/) will make
sure any packages missing on your system will be installed beforehand.
This can also be done ex-post using the
[`fastverse_install()`](../reference/fastverse_install.md) function.
Setting `options(fastverse.styling = FALSE)` will disable coloured text
printed to the R console (as done for this vignette).
`options(fastverse.quiet = TRUE)` will omit any messages printed from
[`library(fastverse)`](https://fastverse.github.io/fastverse/) and
[`fastverse_extend()`](../reference/fastverse_extend.md) or
[`fastverse_install()`](../reference/fastverse_install.md):

``` r

fastverse_detach()
options(fastverse.quiet = TRUE)
library(fastverse) # Nothing to see here
# Warning: package 'data.table' was built under R version 4.3.1

# This gives lots of function clashes with data.table, but they are not displayed in quiet mode
fastverse_extend(lubridate)
# Warning: package 'lubridate' was built under R version 4.3.1
```

If you only want to omit a function clash check when calling
`fastverse_extend`, you can also use
`fastverse_extend(..., check.conflicts = FALSE)`.

## Conclusion

The *fastverse* was developed principally for 2 reasons: to promote
quality high-performance software development for R, and to provide a
flexible approach to package loading and management in R, particularly
for users wishing to combine various high-performance packages in
statistical workflows. To the extent that high-performance software
development in R continues to prioritize low-dependency and stable APIs,
complex statistical and project workflows can be developed without
sophisticated package management solutions.

``` r

# Resetting the fastverse to defaults (clearing all permanent extensions and options)
fastverse_reset()
# Detaching 
fastverse_detach()
```

## Appendix: A note on OpenMP Multithreading

*data.table*, *collapse* and *kit* support OpenMP multithreading on all
platforms. Global defaults can be set using
[`data.table::setDTthreads()`](https://rdrr.io/pkg/data.table/man/openmp-utils.html),
`collapse::set_collapse(nthreads = ...)` and
`options(kit.nThread = ...)`. Users should be careful with running
multithreaded *collapse* and *kit* functions inside *data.table*. I have
created a small [video
tutorial](https://www.youtube.com/watch?v=ne4Es2_9r5A) for *collapse*
where I talk more about this amongst other things.

Note also that on macOS, OpenMP is disabled in binaries installed from
CRAN. To enable OpenMP on macOS, users first need to install OpenMP and
do some configuration. See [here](https://mac.r-project.org/openmp/) for
instructions and the latest OpenMP releases. I have also compactly
written the terminal commands you need to run in the description of the
[tutorial](https://www.youtube.com/watch?v=ne4Es2_9r5A). After
installing OpenMP and configuring your R-session, install the packages
from source or use r-universe binaries. An easy way would be to run
`fastverse_install(data.table, collapse, kit, only.missing = FALSE, repos = .fastverse_repos)`.

[^1]: Before v0.3.0 *matrixStats* and *fst* were past of the core
    *fastverse*, but were removed following a poll on Twitter in
    November 2022 where more than 50% voted in favor of removing them
    from the core *fastverse*.

[^2]: Let me know about other packages you think should be featured
    there.

[^3]: In particular, the `...` expression is first captured using
    `substitute(c(...))`, and then evaluated inside `tryCatch`. If this
    evaluation fails or did not result in a character vector, the
    expression is coerced to character.

[^4]: `options("fastverse.quiet")` and `options("fastverse.styling")`
    will only be cleared if all packages are detached. If selected
    packages are detached, they are removed from
    `options("fastverse.extend")`.

[^5]: Thus it will be removed when the *fastverse* is reinstalled.

[^6]: I note that namespace conflicts can also be detected and handled
    with the [conflicted](https://cran.r-project.org/package=conflicted)
    package on CRAN.

[^7]: When fetching the names of *fastverse* packages,
    `fastverse_packages` first checks any config file and then checks
    `options("fastverse.extend")`.

[^8]: You can place package names in that file any manner you deem
    suitable: separated using spaces or commas, on one or multiple
    lines. Note that the file will be read from left to right and from
    top to bottom. Packages are attached in the order found in the file.

[^9]: Setting `options(fastverse.install = TRUE)` before loading the
    packages will make sure any packages missing on your system will be
    installed beforehand. `options(collapse_mask = ...)` can be used to
    make base R and dplyr functions with faster versions provided in the
    *collapse* package. See `help("collapse-options")`.

[^10]: This may be useful if packages set certain options when being
    loaded, overwriting any prior settings, or if options affect the way
    packages are loaded and should better be set after they are loaded.
