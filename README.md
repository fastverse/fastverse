# fastverse <img src='misc/fastverse_logo.png' width="350px" align="right" />

<!-- badges: start 
[![CRAN status](https://www.r-pkg.org/badges/version/fastverse)](https://cran.r-project.org/package=fastverse) 
[![cran checks](https://cranchecks.info/badges/worst/fastverse)](https://cran.r-project.org/web/checks/check_results_fastverse.html)
[![Travis build status](https://travis-ci.com/SebKrantz/fastverse.svg?branch=master)](https://travis-ci.com/SebKrantz/fastverse) -->
[![R build status](https://github.com/SebKrantz/fastverse/workflows/R-CMD-check/badge.svg)](https://github.com/SebKrantz/fastverse/actions)
<!--
[![Codecov test coverage](https://codecov.io/gh/SebKrantz/fastverse/branch/master/graph/badge.svg)](https://codecov.io/gh/SebKrantz/fastverse?branch=master)
[![minimal R version](https://img.shields.io/badge/R%3E%3D-2.10-6666ff.svg)](https://cran.r-project.org/)
[![status](https://tinyverse.netlify.com/badge/fastverse)](https://CRAN.R-project.org/package=fastverse)
[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.fastverse.org/lifecycle/#maturing)
![downloads per month](http://cranlogs.r-pkg.org/badges/fastverse?color=blue)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/fastverse?color=blue)
 badges: end -->


The *fastverse* package integrates and provides utilities for easy installation, loading and management of a complimentary set of high-performance R packages for statistical computing and data manipulation. *fastverse* packages work well together and provide:

- Fast R code - especially for limited personal computing resources. All critical codes in *fastverse* packages are written in a compiled language such as C, C++ or Fortran. Many packages additionally offer multi-threading. 

- High code quality and above average levels of maintenance, thorough documentation and seamless application to the designated R objects, minding all the typical properties of real world data such as missing values. 

- A minimal set of dependencies. Most *fastverse* packages only depend on packages providing C++ API's in R, if any. 

## Packages

The *fastverse* consists of 6 core packages (7 dependencies in total) that provide broad based data manipulation functionality and have a carefully managed API. These packages are installed and attached along with the `fastverse` package. In addition the user has the option (via the `fastverse_entend()` function) to freely and flexibly install and attach extension packages offering more specific functionality. 

### Core fastverse

- **data.table**: Enhanced data frame class with concise data manipulation framework offering powerful aggregation, extremely flexible split-apply-combine computing, reshaping, joins, rolling statistics, set operations on tables, fast csv read/write, and various utilities such as transposition of data. 

- **collapse**: Fast grouped & weighted statistical computations, time series and panel data transformations, list-processing, data manipulation functions, summary statistics and various utilities such as support for variable labels. Class-agnostic framework designed to work with vectors, matrices, data frames, lists and related classes including *xts*, *data.table*, *tibble*, *pdata.frame*, *sf*.  <!-- *tsibble*, *tibbletime* -->

- **matrixStats**: Efficient row-and column-wise statistics on matrices (and vectors), including computations on subsets of rows and columns. 

- **kit**: Fast vectorized and nested switches, some parallel (row-wise) statistics, and some utilities such as efficient partial sorting and unique values. 

- **magrittr**: Efficient pipe operators for enhanced programming and code unnesting.

- **fst**: A compressed data file format that is very fast to read and write. 

  *Additional dependency*: Package *Rcpp* is imported by *collapse* and *fst*.

### Suggested Extensions


#### Time Series (TS)
- **xts** and **zoo**: A fast and reliable matrix-based time series class providing fully identified ordered observations and various utilities for plotting and computations (1 dependency).

- **roll**: Really fast rolling and expanding window functions that preserve *xts* (3 dependencies).

  *Integration Notes*: *xts* objects are preserved by *roll* functions and by *collapse*'s time series and data transformation functions^[These functions can also handle irregular time series, but this requires passing an integer time variable to the `t` argument which has consecutive integer steps for regular parts of the time series and non-consecutive integers for the irregular parts.]. As *xts* objects are matrices, all *matrixStats* functions apply to them as well. Finally, *xts* objects can be converted quickly and easily to and from *data.table*. 
  
<!-- Passing the `xts::index()` coerced to integer to the `t` argument of *collapse*'s `flag`, `fdiff` and `fgrowth` further allows exact time-based computations on irregularly spaced time series, which is not supported by *xts*'s built-in functions. -->  

#### Dates and Times (DT)
- **lubridate**: Comprehensive and fast library to deal with dates and times (2 dependencies).

- **clock**: Comprehensive and fast library to deal with dates and times (6 dependencies).

- **timechange**: (1 dependency).

- **fasttime**: Fast parsing of strings to 'POSIXct' (0 dependencies).

- **anytime**: Anything to 'POSIXct' or 'Date' Converter

- **nanotime**: (7 dependencies).

  *Integration Notes*: Date and time variables are preserved in many *data.table* and *collapse* operations. *data.table* additionally offers an efficient integer based date class 'IDate' with some supporting functionality.

#### Strings (ST)
- **stringi**: Main R package for fast, correct, consistent, and convenient string/text manipulation (backend to *stringr* and *snakecase*) (0 dependencies).

- **stringr**: Simple, Consistent Wrappers for Common String Operations, based on *stringi* (3 dependencies).

- **snakecase**: Convert Strings into any Case, based on *stringi* and *stringr* (4 dependencies).

#### Statistics and Computing (SC)
- **Rfast** and **Rfast2**: Heterogenous sets of fast functions for statistics, estimation and data manipulation operating on vectors and matrices. Missing values are not supported (4-5 dependencies).

- **parallelDist**: Multi-Threaded Distance Matrix Computation (3 dependencies).

- **coop**: Fast implementations of the covariance, correlation, and cosine similarity (0 dependencies).

  *Notes*: *Rfast* has a number of like-named functions to matrixstats. These are simpler but typically faster and support multithreading. Some highly efficient statistical functions can also be found scattered across various other packages, notable to mention here are *Hmisc* (60 dependencies) and *DescTools* (17 dependencies). *fastDummies* (16 dependencies) implements creation of dummy (binary) variables. 

<!-- 
- **fastmatch**: Fast match function.
- **fastmap**: Fast Implementation of a Key-Value Store.
- **fastDummies**: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables. (16 dependencies)
-->  

#### Spatial (SP)
- **sf**: Leading fast framework for geospatial computing and manipulation in R, offering a simple and flexible spatial data frame and supporting functionality (13 dependencies). 

- **stars**: (17 dependencies) 

- **terra**: (4 dependencies) 

  *Integration Notes*: *collapse* can be used for efficient manipulation and computations on *sf* data frames. *sf* also offers tight integration with *dplyr*.


#### Visualization (VI)
- **dygraphs**: Interface to 'Dygraphs' Interactive Time Series Charting Library (11 dependencies). 

- **lattice**: Trellis Graphics for R (0 dependencies). 

- **grid**: The Grid Graphics Package (0 dependencies). 

- **ggplot2**: Create Elegant Data Visualisations Using the Grammar of Graphics (30 dependencies). 

- **scales**: Scale Functions for Visualization (10 dependencies). 

Notes: *latticeExtra* provides extra graphical utilities base on *lattice*. *gridExtra* provides miscellaneous functions for *grid* graphics (and consequently for *ggplot2* which is based on *grid*). *gridtext* provides improved text rendering support for *grid* graphics. Many packages offer *ggplot2* extensions, (typically starting with 'gg') such as *ggExtra*, *ggalt*, *ggforce*, *ggmap*, *ggtext*, *ggthemes*, *ggrepel*, *ggridges*, *ggfortify*, *ggstatsplot*, *ggeffects*, *ggsignif*, *GGally*, *ggcorrplot*, *ggdendro*, etc...


#### Tidyverse-like data manipulation built on *data.table* (TV)

- **tidytable**: Quite comprehensive implementation of *dplyr*, *tidyr* and *purr* functions based on *data.table* backend. Function names appended with a `.` e.g. `mutate.()`. The `dt()` helper further makes *data.table* syntax pipable. Imports *rlang* and others, thus a borderline case (14 total dependencies). 

- **tidyfast**: Fast Tidying of Data. Covers `tidyr` functionality, `dt_` prefix (2 dependencies). 

- **tidyfst**: Tidy Verbs for Fast Data Manipulation. Covers `dplyr` functionality, `_dt` suffix, cheatsheet (7 dependencies). 

- **tidyft**: Tidy Verbs for Fast Data Operations by Reference. This toolkit is designed for big data analysis in high-performance desktop or laptop computers (7 dependencies).

- **maditr**: Minimal implementation with functions `let()` and `take()` (2 dependencies). 

(One could also mention Rstudio's **dtplyr** and the **table.express** package here, but these packages import **dplyr** and thus have a around 20 dependencies.)

## Usage

``` r
# Loads and attaches the core fastverse packages
library(fastverse)

# Extend the fastverse by all installed extension packages
fastverse_extend()

# Extends the core fastverse by packages in certain topics
fastverse_extend(topics = c("ts", "sp"))

# Extends the core fastverse by certain packages
fastverse_extend(xts, roll, sf, Rfast)
```
