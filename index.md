# fastverse

The *fastverse* is a suite of complementary high-performance packages
for statistical computing and data manipulation in R. Developed
independently by various people, *fastverse* packages jointly contribute
to the objectives of:

- Speeding up R through heavy use of compiled (C/C++) code
- Enabling more complex statistical and data manipulation operations in
  R
- Reducing the number of dependencies required for advanced computing in
  R

The `fastverse` package is a meta-package providing utilities for easy
installation, loading and management of these packages. It is an
extensible framework that allows users to create a ‘verse’ of packages
suiting their general needs - see the
[**vignette**](https://fastverse.github.io/fastverse/articles/fastverse_intro.html)
for a concise overview of the package.

## Core Packages

The *fastverse* installs with 4 core packages (5 dependencies in total)
which provide broad C/C++ based statistical and data manipulation
functionality and have carefully managed APIs.

- **[data.table](https://r-datatable.com)**: Enhanced data frame class
  with concise data manipulation framework offering powerful
  aggregation, update, reshaping, (rolling) joins, rolling statistics,
  set operations on tables, fast csv read/write, and various utilities
  such as data transposition/stringsplit-transpose.

- **[collapse](https://fastverse.org/collapse)**: Fast grouped and
  weighted statistical computations, time series and panel data
  transformations, list-processing, data manipulation functions
  (incl. fast joins and pivots), summary statistics, and various
  utilities for efficient programming. Class-agnostic framework designed
  to work with vectors, matrices, data frames, lists and related classes
  including *xts*, *data.table*, *tibble*, and *sf*.

- **[kit](https://fastverse.org/kit)**: Parallel (row-wise) statistical
  functions, vectorized and nested switches, and some utilities such as
  efficient partial sorting.

- **[magrittr](https://magrittr.tidyverse.org)**: Efficient pipe
  operators and aliases for enhanced R programming and code un-nesting.

## Installation

``` r

# Install the CRAN version
install.packages("fastverse")

# Install (Windows/Mac binaries) from R-universe
install.packages("fastverse", repos = "https://fastverse.r-universe.dev")

# Install from GitHub (requires compilation)
remotes::install_github("fastverse/fastverse")
```

## Extending the *fastverse*

Users can, via the [`fastverse_extend()`](reference/fastverse_extend.md)
function, freely add packages. Setting `permanent = TRUE` adds them to
the core *fastverse*. Another option is placing a `.fastverse` config
file with packages in a project directory. Separate verses can be
created with [`fastverse_child()`](reference/fastverse_child.md). See
the
[**vignette**](https://fastverse.github.io/fastverse/articles/fastverse_intro.html)
for details.

### Suggested Extensions

High-performing packages for different data manipulation and statistical
computing topics are suggested below. The total (recursive) dependency
count is indicated for each package.

------------------------------------------------------------------------

#### Time Series

- **[xts](https://github.com/joshuaulrich/xts)** and
  **[zoo](https://github.com/cran/zoo)**: Fast and reliable matrix-based
  time series classes providing fully identified ordered observations
  and various utilities for plotting and computations (1 dependency).

- **[roll](https://github.com/jasonjfoster/roll)**: Fast rolling and
  expanding window functions for vectors and matrices (3 dependencies).

  *Notes*: *xts*/*zoo* objects are preserved by *roll* functions and by
  *collapse*’s time series and data transformation functions[^1]. As
  *xts*/*zoo* objects are matrices, all *matrixStats* functions apply to
  them as well. *xts* objects can also easily be converted to and from
  *data.table*, which also has some fast rolling functions like
  `frollmean` and `frollapply`.

#### Dates and Times

- **[anytime](https://github.com/eddelbuettel/anytime)**: Anything to
  ‘POSIXct’ or ‘Date’ converter (2 dependencies).

- **[fasttime](https://github.com/s-u/fasttime)**: Fast parsing of
  strings to ‘POSIXct’ (0 dependencies).

- **[nanotime](https://github.com/eddelbuettel/nanotime)**: Provides a
  coherent set of temporal types and functions with nanosecond precision
  -  
  based on the ‘integer64’ class (7 dependencies).

- **[clock](https://github.com/r-lib/clock)**: Comprehensive library for
  date-time manipulations using a new family of orthogonal date-time
  classes (durations, time points, zoned-times, and calendars) (6
  dependencies).

- **[timechange](https://github.com/vspinu/timechange)**: Efficient
  manipulation of date-times accounting for time zones and daylight
  saving times (1 dependency).

  *Notes*: Date and time variables are preserved in many *data.table*
  and *collapse* operations. *data.table* additionally offers an
  efficient integer based date class ‘IDate’ with some supporting
  functionality. *xts* and *zoo* also provide various functions to
  transform dates, and *zoo* provides classes ‘yearmon’ and ‘yearqtr’
  for convenient computation with monthly and quarterly data. Package
  *mondate* also provides a class ‘mondate’ for monthly data. Many users
  also find **[lubridate](https://github.com/tidyverse/lubridate)**
  convenient for ‘POSIX-’ and ‘Date’ based computations.

#### Strings

- **[stringi](https://github.com/gagolews/stringi)**: Main R package for
  fast, correct, consistent, and convenient string/text manipulation
  (backend to *stringr* and *snakecase*) (0 dependencies).

- **[stringfish](https://github.com/traversc/stringfish)**: Fast
  computation of common (base R) string operations using the ALTREP
  system (2 dependencies).

- **[stringdist](https://github.com/markvanderloo/stringdist)**: Fast
  computation of string distance metrics, matrices, and fuzzy matching
  (0 dependencies).

- **[fuzzystring](https://github.com/PaulESantos/fuzzystring)**: Fast,
  flexible fuzzy string joins for data frames using approximate string
  matching. Built on top of *data.table* and *stringdist* (3
  dependencies).

  *Notes*: At least two packages offer convenient wrappers around the
  rather rich *stringi* API:
  **[stringr](https://github.com/tidyverse/stringr)** provides simple,
  consistent wrappers for common string operations, based on *stringi*
  (3 dependencies), and
  **[snakecase](https://github.com/Tazinho/snakecase)** converts strings
  into any case, based on *stringi* and *stringr* (4 dependencies).

#### Statistics and Computing

- **[matrixStats](https://github.com/HenrikBengtsson/matrixStats)**:
  Efficient row-and column-wise (weighted) statistics on matrices and
  vectors, including computations on subsets of rows and columns (0
  dependencies).
  **[matrixTests](https://github.com/karoliskoncevicius/matrixTests)**
  uses *matrixStats* for efficient multiple hypothesis testing on
  matrix/data.frame rows/columns (1 dependency).

- **[Rfast](https://github.com/RfastOfficial/Rfast)** and
  **[Rfast2](https://github.com/RfastOfficial/Rfast2)**: Heterogeneous
  sets of fast functions for statistics, data manipulation, estimation,
  and hypothesis testing on vectors, matrix row/columns, and sometimes
  data frame columns (4-5 dependencies).

- **[broadcast](https://github.com/tony-aw/broadcast)**: provides
  ‘Numpy’-like broadcasted array operations and array binding (1
  dependency).

- **[vctrs](https://github.com/r-lib/vctrs/)**: Computational backend of
  the [*tidyverse*](https://github.com/tidyverse) that provides many
  basic programming functions for R vectors (including lists and data
  frames) implemented in C (such as sorting, matching, replicating,
  unique values, concatenating, splitting etc. of vectors). These are
  often significantly faster than base R equivalents, but not as
  aggressively optimized as some equivalents in *collapse* or
  *data.table* (4 dependencies).

- **[parallelDist](https://github.com/alexeckert/parallelDist)**:
  Multi-threaded distance matrix computation (3 dependencies). See also
  [`collapse::fdist()`](https://fastverse.org/collapse/reference/fdist.html)
  for multithreaded/SIMD euclidean distances.

- **[coop](https://github.com/wrathematics/coop)**: Fast implementations
  of the covariance, correlation, and cosine similarity (0
  dependencies).

- **[rsparse](https://CRAN.R-project.org/package=rsparse)**: Implements
  many algorithms for statistical learning on sparse matrices - matrix
  factorizations, matrix completion, elastic net regressions,
  factorization machines (8 dependencies). See also package
  **[MatrixExtra](https://CRAN.R-project.org/package=MatrixExtra)**.

- **[SLmetrics](https://github.com/serkor1/SLmetrics)**: Fast and
  memory-efficient evaluation of statistical learning algorithms,
  categorical, cross-sectional and time series data (3 dependencies).

- **[fastmatrix](https://github.com/faosorios/fastmatrix)** provides a
  small set of functions written in C or Fortran providing fast
  computation of some matrices and operations useful in statistics (0
  dependencies).

- **[rrapply](https://CRAN.R-project.org/package=rrapply)**: The
  `rrapply()` function extends base
  [`rapply()`](https://rdrr.io/r/base/rapply.html) by including a
  condition or predicate function for the application of functions and
  diverse options to prune or aggregate the result (0 dependencies).

- **[dqrng](https://github.com/daqana/dqrng)**: Fast uniform, normal or
  exponential random numbers and random sampling (i.e. faster `runif`,
  `rnorm`, `rexp`, `sample` and `sample.int` functions) (3
  dependencies).

- **[RcppAlgos](https://github.com/jwood000/RcppAlgos)**: Optimized
  functions and flexible iterators for combinatorics (permutations,
  combinations, partitions, etc.) and computational mathematics (prime
  sieving, factorization). Features multi-threading and a low memory
  footprint (2 dependencies).

- **[fastmap](https://github.com/r-lib/fastmap)**: Fast implementation
  of data structures based on C++, including a key-value store
  (`fastmap`), stack (`faststack`), and queue (`fastqueque`) (0
  dependencies).

- **[fastmatch](https://github.com/s-u/fastmatch)**: A faster
  [`match()`](https://rdrr.io/r/base/match.html) function (drop-in
  replacement for [`base::match`](https://rdrr.io/r/base/match.html),
  and `base::%in%`), that keeps the hash table in memory for much faster
  repeated lookups (0 dependencies).

- **[collections](https://github.com/randy3k/collections)**:
  High-performance container data types (queues, stacks, deques, ordered
  dictionaries) backed by C++ for efficient algorithmic programming (0
  dependencies).

- **[stdvectors](https://github.com/digEmAll/stdvectors)**: Allows the
  creation and manipulation of C++ `std::vector`’s in R. Unlike R
  vectors, `std::vector`’s are dynamically allocated (growable) arrays
  (1 dependency).

- **[cheapr](https://github.com/NicChr/cheapr)**: Provides fast and
  memory-efficient functions for common data manipulation and
  transformation tasks, focusing on minimal dependencies and high
  performance (3 dependencies).

- **[hutilscpp](https://github.com/hughparsonage/hutilscpp)** provides
  C++ implementations of some frequently used utility functions in R (4
  dependencies).

  *Notes*: *Rfast* has a number of like-named functions to
  *matrixStats*. These are simpler but typically faster and support
  multi-threading. Some highly efficient statistical functions can also
  be found scattered across various other packages, notable to mention
  here are *Hmisc* (60 dependencies) and *DescTools* (17 dependencies).

#### Spatial

- **[sf](https://github.com/r-spatial/sf/)**: Leading framework for
  geospatial computing and manipulation in R, offering a simple and
  flexible spatial data frame and supporting functionality (12
  dependencies).

- **[s2](https://github.com/r-spatial/s2)**: Provides R bindings for
  [Google’s s2 C++ library](https://github.com/google/s2geometry) for
  high-performance geometric calculations on the sphere (3D,
  geographic/geodetic CRS). Used as a backend to *sf* for calculations
  on geometries with geographic/geodetic CRS, but using *s2* directly
  can provide substantial performance gains (2 dependencies).

- **[geos](https://github.com/paleolimbot/geos/)**: Provides an R API to
  the [Open Source Geometry Engine (GEOS)](https://trac.osgeo.org/geos/)
  C-library, which can be used to very efficiently manipulate planar
  (2D/flat/projected CRS) geometries, and a vector format with which to
  efficiently store ‘GEOS’ geometries. Used as a backend to *sf* for
  calculations on geometries with projected CRS, but using *geos*
  directly can provide substantial performance gains (2 dependencies).

- **[stars](https://github.com/r-spatial/stars)**: Spatiotemporal data
  (raster and vector) in the form of dense arrays, with space and time
  being array dimensions (16 dependencies).

- **[terra](https://github.com/rspatial/terra)**: Methods for spatial
  data analysis with raster and vector data. Processing of very large
  (out of memory) files is supported (1 dependency).

- **[exactextractr](https://github.com/isciences/exactextractr)**:
  Provides fast extraction from raster datasets using polygons. Notably,
  it is much faster than *terra* for computing summary statistics of
  raster layers within polygons (17 dependencies).

- **[geodist](https://github.com/hypertidy/geodist)**: Provides very
  fast calculation of geodesic distances (0 dependencies).

- **[dggridR](https://github.com/r-barnes/dggridR)**: Provides discrete
  global grids for R: allowing accurate partitioning of the earths
  surface into equally sized grid cells of different shapes and sizes
  (11 dependencies).

- **[cppRouting](https://github.com/vlarmet/cppRouting)**: Algorithms
  for routing and solving the traffic assignment problem, including
  calculation of distances, shortest paths and isochrones on weighted
  graphs using several (optimized) variants of Dijkstra’s algorithm (4
  dependencies).

- **[igraph](https://github.com/igraph)**: Provides and R port to the
  *igraph* C library for complex network analysis and graph theory (11
  dependencies).

- **[dbscan](https://github.com/mhahsler/dbscan)**: Fast C++-based
  implementation of density-based clustering algorithms including
  DBSCAN, HDBSCAN, OPTICS, and related methods. Also provides k-nearest
  neighbor search and shared nearest neighbor clustering (4
  dependencies).

- **[leaderCluster](https://cran.r-project.org/package=leaderCluster)**:
  Implements the leader clustering algorithm for efficiently clustering
  large datasets, particularly suited for geospatial point data (0
  dependencies).

  *Notes*: *collapse* can be used for efficient manipulation and
  computations on *sf* data frames. *sf* also offers tight integration
  with *dplyr*. Another efficient routing package is
  [*dodgr*](https://github.com/UrbanAnalyst/dodgr) (45 dependencies).
  [*sfnetworks*](https://github.com/luukvdmeer/sfnetworks) allows
  network analysis combining *sf* and *igraph* (42 dependencies) and
  functions for network cleaning (partly taken from
  [tidygraph](https://github.com/thomasp85/tidygraph) which also wraps
  *igraph*). [*stplanr*](https://github.com/ropensci/stplanr)
  facilitates sustainable transport planning with R, including very
  useful helpers such as `overline()` to turn a set of linestrings
  (routes) into a network (45 dependencies).

#### Visualization

- **[lattice](https://github.com/deepayan/lattice)**: Trellis graphics
  for R (0 dependencies).

- **[grid](https://github.com/cran/grid)**: The grid graphics package (0
  dependencies). \<!–

- **[vegabrite](https://github.com/vegawidget/vegabrite)** provides an
  interface to the [vega-lite](https://vega.github.io/vega-lite/)
  high-level grammar of interactive graphics. –\>

- **[tinyplot](https://github.com/grantmcdermott/tinyplot)** provides a
  lightweight extension of the base R graphics system, with support for
  automatic grouping, legends, facets, and various other enhancements (0
  dependencies).

&nbsp;

- **[ggplot2](https://github.com/tidyverse/ggplot2)**: Create elegant
  data visualizations using the Grammar of Graphics (15 dependencies).

- **[scales](https://github.com/r-lib/scales)**: Scale functions for
  visualizations (10 dependencies).

- **[scattermore](https://github.com/exaexa/scattermore)**: Extremely
  fast scatterplot rasterization for large datasets, enabling
  visualization of millions of points (16 dependencies).

- **[ggrastr](https://github.com/VPetukhov/ggrastr)**: Rasterizes layers
  in ggplot2 for faster plotting of large datasets (37 dependencies).

  *Notes:* *latticeExtra* provides extra graphical utilities base on
  *lattice*. *gridExtra* provides miscellaneous functions for *grid*
  graphics (and consequently for *ggplot2* which is based on *grid*).
  *gridtext* provides improved text rendering support for *grid*
  graphics. Many packages offer *ggplot2* extensions, (typically
  starting with ‘gg’) such as *ggExtra*, *ggalt*, *ggforce*, *ggh4x*,
  *ggmap*, *ggtext*, *ggthemes*, *ggrepel*, *ggridges*, *ggfortify*,
  *ggstatsplot*, *ggeffects*, *ggsignif*, *GGally*, *ggcorrplot*,
  *ggdendro*, etc.. Users in desperate need for greater performance may
  also find the (unmaintained)
  [lwplot](https://github.com/eddelbuettel/lwplot) package useful that
  provides a faster and lighter version of *ggplot2* with *data.table*
  backend.

#### Data Manipulation in R Based on Faster Languages

- **[r-polars](https://github.com/pola-rs/r-polars)** provides an R-port
  to the impressively fast [polars DataFrame’s
  library](https://github.com/pola-rs/polars/) written in Rust (1
  dependencies).

- **[duckplyr](https://github.com/tidyverse/duckplyr)** provides a
  drop-in replacement for dplyr verbs using DuckDB as the backend,
  enabling fast and memory-efficient data manipulation for large
  datasets (22 dependencies).

  *Notes*: Package
  **[tidypolars](https://github.com/etiennebacher/tidypolars)** provides
  a *tidyverse*-style wrapper around *r-polars*.

#### Data Input-Output, Serialization, and Larger-Than-Memory Processing (IO)

- **[iotools](https://cran.r-project.org/package=iotools)**:
  High-performance I/O tools for streaming data, including ultra-fast
  chunk-wise processing and string splitting operations (0
  dependencies).

- **[fst](https://github.com/fstpackage/fst)**: A compressed data file
  format that is very fast to read and write. Full random access in both
  rows and columns allows reading subsets from a ‘.fst’ file (2
  dependencies).

- **[qs](https://github.com/qsbase/qs)** provides a lightning-fast and
  complete replacement for the `saveRDS` and `readRDS` functions in R.
  It supports general R objects with attributes and references - at
  similar speeds to *fst* - but does not provide on-disk random access
  to data subsets like *fst* (4 dependencies).

- **[arrow](https://github.com/apache/arrow/tree/master/r)** provides
  both a low-level interface to the Apache Arrow C++ library (a
  multi-language toolbox for accelerated data interchange and in-memory
  processing) including fast reading / writing delimited files,
  efficient storage of data as `.parquet` or `.feather` files, efficient
  (lazy) queries and computations, and sharing data between R and Python
  (14 dependencies). It provides methods for several *dplyr* functions
  allowing highly efficient data manipulation on arrow datasets. Check
  out the [useR2022 workshop](https://arrow-user2022.netlify.app/) on
  working with larger than memory data with apache arrow in R, and the
  [apache arrow R
  cookbook](https://arrow.apache.org/cookbook/r/index.html) as well as
  the [awesome-arrow-r](https://github.com/thisisnic/awesome-arrow-r)
  repository.

- **[duckdb](https://github.com/duckdb/duckdb)**: DuckDB is a
  high-performance analytical database system that can be used on
  in-memory or out-of memory data (including csv, `.parquet` files,
  arrow datasets, and it’s own `.duckdb` format), and that provides a
  rich SQL dialect and optimized query execution for data analysis (1
  dependency). It can also be used with the *dbplyr* package that
  translates *dplyr* code to SQL.
  [This](https://www.christophenicault.com/post/large_dataframe_arrow_duckdb/)
  Article by Christophe Nicault (October 2022) demonstrates the
  integration of *duckdb* with R and *arrow*. Also see the [official
  docs](https://duckdb.org/docs/api/r.html).

- **[vroom](https://github.com/tidyverse/vroom)** provides fast reading
  of delimited files (23 dependencies).

  *Notes*: *data.table* provides `fread` and `fwrite` for fast reading
  of delimited files.

#### Parallelization, High-Performance Computing and Out-Of-Memory Data

- **[mirai](https://github.com/r-lib/mirai)**: Minimalist async
  evaluation framework for R: a ‘mirai’ evaluates an expression in a
  parallel process, on the local machine or over the network, returning
  the result automatically upon completion. Also provides a parallel map
  function (1 dependency).

- See also the [High-Performance and Parallel
  Computing](https://CRAN.R-project.org/view=HighPerformanceComputing)
  Task View and the [futureverse](https://www.futureverse.org/).

#### Compiling R

- **[nCompiler](https://github.com/nimble-dev/nCompiler)**: Compiles R
  functions to C++, and covers basic math, distributions, vectorized
  math and linear algebra, as well as basic control flow. R and Compiled
  C++ functions can also be jointly utilized in the a class ‘nClass’
  that inherits from R6. An in-progress
  [user-manual](https://htmlpreview.github.io/?https://raw.githubusercontent.com/nimble-dev/nCompiler/master/UserManual/_site/index.html)
  provides an overview of the package.

- **[ast2ast](https://github.com/Konrad1991/ast2ast)**: Also compiles R
  functions to C++, and is very straightforward to use (it has a single
  function `translate()` to compile R functions). Supports, linear
  algebra (matrix product, solve, backsolve/forwardsolve, Cholesky
  decomposition) and automatic differentiation. [Available on
  CRAN](https://CRAN.R-project.org/package=ast2ast) (3 dependencies).

- **[odin](https://github.com/mrc-ide/odin)**: Implements R to C
  translation and compilation, but specialized for differential equation
  solving problems. [Available on
  CRAN](https://CRAN.R-project.org/package=odin) (8 dependencies).

- **[armacmp](https://github.com/dirkschumacher/armacmp)** translates
  linear algebra code written in R to C++ using the Armadillo Template
  Library. The package can also be used to write mathematical
  optimization routines that are translated and optimized in C++ using
  *RcppEnsmallen*.

- **[r2c](https://github.com/brodieG/r2c)** provides compilation of R
  functions to be applied over many groups (e.g. grouped bivariate
  linear regression etc.).

- **[FastR](https://github.com/oracle/fastr)** is a high-performance
  implementation of the entire R programming language, that can JIT
  compile R code to run on the [Graal VM](https://www.graalvm.org/).

- **[inline](https://github.com/eddelbuettel/inline)** allows users to
  write C, C++ or Fortran functions and compile them directly to an R
  function for use within the R session. [Available on
  CRAN](https://CRAN.R-project.org/package=inline) (0 dependencies).

- **[quickr](https://github.com/t-kalinowski/quickr)** is an R to
  Fortran transpiler with data type and shape annotations for high
  performance. [Available on
  CRAN](https://CRAN.R-project.org/package=quickr) (3 dependencies).

  *Notes*: Many of these projects are experimental and not available as
  CRAN packages.

#### R-like Data Manipulation in Faster Languages

- **[tidypolars](https://github.com/markfairbanks/tidypolars)** is a
  python library built on top of
  [polars](https://github.com/pola-rs/polars/) that gives access to
  methods and functions familiar to R tidyverse users.

- **[Tidier.jl](https://github.com/TidierOrg/Tidier.jl)** provides a
  Julia implementation of the tidyverse mini-language in Julia. Powered
  by the [DataFrames.jl](https://github.com/JuliaData/DataFrames.jl)
  library.

#### R Bindings to Faster Languages

- **[R’s C API](http://adv-r.had.co.nz/C-interface.md)** is the most
  natural way to extend R and does not require additional packages. It
  is further documented in the [Writing R Extensions
  Manual](https://cran.r-project.org/doc/manuals/R-exts.html#System-and-foreign-language-interfaces),
  the [R Internals
  Manual](https://cran.r-project.org/doc/manuals/r-release/R-ints.html),
  the **[r-internals](https://github.com/hadley/r-internals)**
  repository and sometimes referred to in the [R
  Blog](https://developer.r-project.org/Blog/public/) (and some other
  Blogs on the web). Users willing to extend R in this way should
  familiarize themselves with R’s garbage collection and [PROTECT
  Errors](https://github.com/kalibera/cran-checks/blob/master/rchk/PROTECT.md).

- **[Rcpp](https://github.com/RcppCore/Rcpp)** provides seamless R and
  C++ integration, and is widely used to extend R with C++. Compared to
  the C API compile time is slower and object files are larger, but
  users don’t need to worry about garbage collection and can use modern
  C++ as well as a rich set of R-flavored functions and classes (0
  dependencies).

- **[cpp11](https://github.com/r-lib/cpp11)** provides a simpler,
  header-only R binding to C++ that allows faster compile times and
  [several other
  enhancements](https://cpp11.r-lib.org/articles/motivations.html) (0
  dependencies).

- **[tidyCpp](https://github.com/eddelbuettel/tidycpp)** provides a tidy
  C++ wrapping of the C API of R - to make the C API more amenable to
  C++ programmers (0 dependencies).

- **[JuliaCall](https://github.com/Non-Contradiction/JuliaCall)**
  Provides an R interface to the Julia programming language (11
  dependencies). Other interfaces are provided by
  [XRJulia](https://github.com/johnmchambers/XRJulia) (2 dependencies)
  and [JuliaConnectoR](https://github.com/stefan-m-lenz/JuliaConnectoR)
  (0 dependencies).

- **[rextendr](https://github.com/extendr/rextendr)** provides an R
  interface to the Rust programming language (29 dependencies).

- **[rJava](https://github.com/s-u/rJava)** provides an R interface to
  Java (0 dependencies).

  *Notes*: There are many Rcpp extension packages binding R to powerful
  C++ libraries, such as linear algebra through *RcppArmadillo* and
  *RcppEigen*, thread-safe parallelism through *RcppParallel* etc.

#### Tidyverse-like Data Manipulation built on *data.table*

- **[tidytable](https://github.com/markfairbanks/tidytable)**: A tidy
  interface to *data.table* that is *rlang* compatible. Quite
  comprehensive implementation of *dplyr*, *tidyr* and *purr* functions.
  Package uses a class *tidytable* that inherits from *data.table*. The
  [`dt()`](https://rdrr.io/r/stats/TDist.html) function makes
  *data.table* syntax pipeable (12 total dependencies).

- **[dtplyr](https://github.com/tidyverse/dtplyr)**: A tidy interface to
  *data.table* built around lazy evaluation i.e. users need to call
  `as.data.table()`,
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or
  `as_tibble()` to access the results. Lazy evaluation holds the
  potential of generating more performant *data.table* code (20
  dependencies).

- **[tidyfst](https://github.com/hope-data-science/tidyfst)**: Tidy
  verbs for fast data manipulation. Covers *dplyr* and some *tidyr*
  functionality. Functions have `_dt` suffix and preserve *data.table*
  object. A
  [cheatsheet](https://raw.githubusercontent.com/hope-data-science/tidyfst/master/docs/tidyfst_cheatsheet.pdf)
  is provided (7 dependencies).

- **[tidyft](https://github.com/hope-data-science/tidyft)**: Tidy verbs
  for fast data operations by reference. Best for big data manipulation
  on out of memory data using facilities provided by *fst* (7
  dependencies).

- **[tidyfast](https://github.com/TysonStanley/tidyfast)**: Fast tidying
  of data. Covers *tidyr* functionality, `dt_` prefix, preserves
  *data.table* object (2 dependencies).

- **[maditr](https://github.com/gdemin/maditr)**: Fast data aggregation,
  modification, and filtering with pipes and *data.table*. Minimal
  implementation with functions `let()` and `take()` for most common
  data manipulation tasks. Also provides Excel-like lookup functions (2
  dependencies).

- **[table.express](https://github.com/asardaes/table.express)** also o
  builds *data.table* expressions from *dplyr* verbs, without executing
  them eagerly. Similar to *dtplyr* but less mature (17 dependencies).

  *Notes*: These packages are wrappers around *data.table* and do not
  introduce own compiled code.

#### Tidyverse-like Data Manipulation built on collapse

- **[fastplyr](https://github.com/NicChr/fastplyr)**: Implements a set
  of fast, pipe-friendly data manipulation verbs (such as `mutate`,
  `summarise`, `arrange`, `filter`, `select`, and `group_by`) built on
  top of *collapse* for efficient grouped and column-wise operations (22
  dependencies).

- **[timeplyr](https://github.com/NicChr/timeplyr)**: Provides fast and
  flexible time-based data manipulation and aggregation, including
  rolling, cumulative, and grouped operations, with a focus on efficient
  time series workflows (41 dependencies).

  *Notes*: These packages are wrappers around *collapse* and do not
  introduce own compiled code.

------------------------------------------------------------------------

#### Adding to this list

Please notify me of any other packages you think should be included
here. Such packages should be well designed, top-performing,
low-dependency, and, with few exceptions, provide own compiled code.
Please note that the *fastverse* focuses on general purpose statistical
computing and data manipulation, thus I won’t include fast packages to
estimate specific kinds of models here (of which R also has a great
many).

[^1]: *collapse* functions can also handle irregular time series.
