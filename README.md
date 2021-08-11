# fastverse <img src='logo.png' width="350px" align="right" />

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


The *fastverse* is a suite of complementary high-performance packages for statistical computing and data manipulation in R. Developed independently by various people, *fastverse* packages jointly contribute to the objectives of:

- Speeding up R through heavy use of compiled code (C, C++, Fortran)
- Enabling more complex statistical and data manipulation operations in R
- Reducing the number of dependencies required for advanced computing in R

The `fastverse` package integrates, and provides utilities for easy installation, loading and management 
of these packages. It is an extensible framework that allows users to (permanently) add or remove packages to create a 'verse' of packages suiting their general needs. Separate 'verses' can be created for different projects. 

*fastverse* packages are jointly attached with `library(fastverse)`, and several functions starting with `fastverse_` help manage dependencies, detect namespace conflicts, add/remove packages from the *fastverse* and update packages.

## Core Packages

The *fastverse* consists of 6 core packages (7 dependencies in total) which provide broad C/C++ based statistical and data manipulation functionality and have carefully managed APIs. These packages are installed and attached along with the `fastverse` package. 

- **data.table**: Enhanced data frame class with concise data manipulation framework offering powerful aggregation, extremely flexible split-apply-combine computing, reshaping, joins, rolling statistics, set operations on tables, fast csv read/write, and various utilities such as transposition of data. 

- **collapse**: Fast grouped & weighted statistical computations, time series and panel data transformations, list-processing, data manipulation functions, summary statistics and various utilities such as support for variable labels. Class-agnostic framework designed to work with vectors, matrices, data frames, lists and related classes including *xts*, *data.table*, *tibble*, *pdata.frame*, *sf*.  <!-- *tsibble*, *tibbletime* -->

- **matrixStats**: Efficient row-and column-wise (weighted) statistics on matrices and vectors, including computations on subsets of rows and columns. 

- **kit**: Fast vectorized and nested switches, some parallel (row-wise) statistics, and some utilities such as efficient partial sorting and unique values. 

- **magrittr**: Efficient pipe operators for enhanced programming and code unnesting.

- **fst**: A compressed data file format that is very fast to read and write. Full random access in both rows and columns allows reading subsets from a '.fst' file. 

  *Additional dependency*: Package *Rcpp* is imported by *collapse* and *fst*.

## Installation

Currently, there are 2 different versions of the *fastverse* on CRAN and GitHub. The GitHub version is recommended if you want to have *matrixStats* consistently preserve attributes of your matrices: it modifies functions in the *matrixStats* namespace making them preserve attributes consistently (and by default) whenever the *fastverse* is attached. This version was rejected by CRAN because it requires a call to `unlockBinding`. The CRAN version takes *matrixStats* as it is, which means most functions do not preserve attributes such as dimension names in computations. 

``` r
# Install the CRAN version
install.packages("fastverse")

# Install the GitHub version (Requires Rtools)
remotes::install_github("SebKrantz/fastverse")
```
*Note* that the GitHub version is not a development version, development takes place in the 'development' branch. 
*matrixStats* is slowly evolving towards greater consistency, but it might take more than half a year until dimension names are handled consistently by default - due to the large number of reverse dependencies. Until then CRAN and GitHub versions of the *fastverse* are released together. 

## Extending the *fastverse*
In addition, users have the option (via the `fastverse_entend()` function) to freely attach extension packages offering more specific functionality. The *fastverse* can by extended by any R package, either just for the current session or permanently: 

<details>
  <summary><b><a style="cursor: pointer;">Click here to expand </a></b> </summary>

<PRE class="fansi fansi-message"><code class="r"># Loads and attaches the core fastverse packages
library(fastverse)</code>
<CODE># -- <span style="font-weight: bold;">Attaching packages</span><span> --------------------------------------- </span><span style="color: #0087FF;">fastverse</span><span> 0.1.5 --
</span></CODE><CODE># <span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">data.table </span><span> 1.14.0     </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">collapse   </span><span> 1.6.5 
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">magrittr   </span><span> 2.0.1      </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">matrixStats</span><span> 0.59.0
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">kit        </span><span> 0.0.7      </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">fst        </span><span> 0.9.4
</span></CODE>
<code class="r"># Permanently extends the core fastverse by certain packages
fastverse_extend(xts, roll, dygraphs, permanent = TRUE)</code>
<CODE># -- <span style="font-weight: bold;">Attaching extension packages</span><span> ----------------------------- </span><span style="color: #0087FF;">fastverse</span><span> 0.1.5 --
</span></CODE><CODE># <span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">xts     </span><span> 0.12.1      </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">dygraphs</span><span> 1.1.1.6
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">roll    </span><span> 1.1.6
</span></CODE><CODE># -- <span style="font-weight: bold;">Conflicts</span><span> ------------------------------------------ fastverse_conflicts() --
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">xts</span><span>::</span><span style="color: #00BB00;">first()</span><span> masks </span><span style="color: #0000BB;">data.table</span><span>::first()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">xts</span><span>::</span><span style="color: #00BB00;">last()</span><span>  masks </span><span style="color: #0000BB;">data.table</span><span>::last()
</span></CODE>
<code class="r"># If the fastverse is now loaded in a new session, these packages are added 
fastverse_detach(session = TRUE)
library(fastverse)
</code><CODE># -- <span style="font-weight: bold;">Attaching packages</span><span> --------------------------------------- </span><span style="color: #0087FF;">fastverse</span><span> 0.1.5 --
</span></CODE><CODE># <span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">data.table </span><span> 1.14.0      </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">fst        </span><span> 0.9.4  
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">magrittr   </span><span> 2.0.1       </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">xts        </span><span> 0.12.1 
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">kit        </span><span> 0.0.7       </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">roll       </span><span> 1.1.6  
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">collapse   </span><span> 1.6.5       </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">dygraphs   </span><span> 1.1.1.6
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">matrixStats</span><span> 0.59.0
</span></CODE><CODE># -- <span style="font-weight: bold;">Conflicts</span><span> ------------------------------------------ fastverse_conflicts() --
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">xts</span><span>::</span><span style="color: #00BB00;">first()</span><span>           masks </span><span style="color: #0000BB;">data.table</span><span>::first()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">collapse</span><span>::</span><span style="color: #00BB00;">is.regular()</span><span> masks </span><span style="color: #0000BB;">zoo</span><span>::is.regular()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">xts</span><span>::</span><span style="color: #00BB00;">last()</span><span>            masks </span><span style="color: #0000BB;">data.table</span><span>::last()
</span></CODE>
<code class="r"># We can also extend only the fastverse for the session, here adding Rfast2
# and any installed suggested packages for date-time manipulation (see following README section)
fastverse_extend(Rfast2, topics = &quot;DT&quot;) </code>
<CODE># -- <span style="font-weight: bold;">Attaching extension packages</span><span> ----------------------------- </span><span style="color: #0087FF;">fastverse</span><span> 0.1.5 --
</span></CODE><CODE># <span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">Rfast2   </span><span> 0.0.9      </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">clock    </span><span> 0.3.1 
# </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">lubridate</span><span> 1.7.10     </span><span style="color: #0087FF;">v</span><span> </span><span style="color: #FF0087;">fasttime </span><span> 1.0.2
</span></CODE><CODE># -- <span style="font-weight: bold;">Conflicts</span><span> ------------------------------------------ fastverse_conflicts() --
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">as.difftime()</span><span> masks </span><span style="color: #0000BB;">base</span><span>::as.difftime()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">clock</span><span>::</span><span style="color: #00BB00;">as_date()</span><span>         masks </span><span style="color: #0000BB;">lubridate</span><span>::as_date()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">date()</span><span>        masks </span><span style="color: #0000BB;">base</span><span>::date()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">hour()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::hour()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">intersect()</span><span>   masks </span><span style="color: #0000BB;">base</span><span>::intersect()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">is.Date()</span><span>     masks </span><span style="color: #0000BB;">collapse</span><span>::is.Date()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">isoweek()</span><span>     masks </span><span style="color: #0000BB;">data.table</span><span>::isoweek()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">mday()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::mday()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">minute()</span><span>      masks </span><span style="color: #0000BB;">data.table</span><span>::minute()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">month()</span><span>       masks </span><span style="color: #0000BB;">data.table</span><span>::month()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">quarter()</span><span>     masks </span><span style="color: #0000BB;">data.table</span><span>::quarter()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">second()</span><span>      masks </span><span style="color: #0000BB;">data.table</span><span>::second()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">setdiff()</span><span>     masks </span><span style="color: #0000BB;">base</span><span>::setdiff()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">union()</span><span>       masks </span><span style="color: #0000BB;">base</span><span>::union()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">wday()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::wday()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">week()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::week()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">yday()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::yday()
# </span><span style="color: #BB0000;">x</span><span> </span><span style="color: #0000BB;">lubridate</span><span>::</span><span style="color: #00BB00;">year()</span><span>        masks </span><span style="color: #0000BB;">data.table</span><span>::year()
</span></CODE>
<code class="r"># This shows a situation report of the fastverse, including all dependencies
fastverse_sitrep(recursive = TRUE)</code>
<CODE># -- <span style="color: #0087FF;">fastverse</span><span> 0.1.5: </span><span style="font-weight: bold;">Situation Report</span><span> -------------------------------- R 4.1.0 --
#  * Global config file: TRUE
#  * Project config file: FALSE
# -- Core packages --------------------------------------------------------------- 
#  * <span style="color: #FF0087;">data.table   </span><span> (1.14.0)
#  * </span><span style="color: #FF0087;">magrittr     </span><span> (2.0.1)
#  * </span><span style="color: #FF0087;">kit          </span><span> (0.0.7)
#  * </span><span style="color: #FF0087;">collapse     </span><span> (1.6.5)
#  * </span><span style="color: #FFAF00;">matrixStats  </span><span> (0.59.0 &lt; 0.60.0)
#  * </span><span style="color: #FF0087;">fst          </span><span> (0.9.4)
#  * </span><span style="color: #FF0087;">xts          </span><span> (0.12.1)
#  * </span><span style="color: #FF0087;">roll         </span><span> (1.1.6)
#  * </span><span style="color: #FF0087;">dygraphs     </span><span> (1.1.1.6)
# -- Extension packages ---------------------------------------------------------- 
#  * </span><span style="color: #FF0087;">Rfast2       </span><span> (0.0.9)
#  * </span><span style="color: #FF0087;">lubridate    </span><span> (1.7.10)
#  * </span><span style="color: #FFAF00;">clock        </span><span> (0.3.1 &lt; 0.4.0)
#  * </span><span style="color: #FF0087;">fasttime     </span><span> (1.0.2)
# -- Dependencies ---------------------------------------------------------------- 
#  * </span><span style="color: #FF0087;">base64enc    </span><span> (0.1.3)
#  * </span><span style="color: #FF0087;">cpp11        </span><span> (0.3.1)
#  * </span><span style="color: #FF0087;">digest       </span><span> (0.6.27)
#  * </span><span style="color: #FFAF00;">ellipsis     </span><span> (0.3.1 &lt; 0.3.2)
#  * </span><span style="color: #FF0087;">generics     </span><span> (0.1.0)
#  * </span><span style="color: #FF0087;">glue         </span><span> (1.4.2)
#  * </span><span style="color: #FF0087;">htmltools    </span><span> (0.5.1.1)
#  * </span><span style="color: #FF0087;">htmlwidgets  </span><span> (1.5.3)
#  * </span><span style="color: #FF0087;">jsonlite     </span><span> (1.7.2)
#  * </span><span style="color: #FF0087;">lattice      </span><span> (0.20.44)
#  * </span><span style="color: #FF0087;">RANN         </span><span> (2.6.1)
#  * </span><span style="color: #FF0087;">Rcpp         </span><span> (1.0.7)
#  * </span><span style="color: #FFAF00;">RcppArmadillo</span><span> (0.10.2.1.0 &lt; 0.10.6.0.0)
#  * </span><span style="color: #FFAF00;">RcppGSL      </span><span> (0.3.8 &lt; 0.3.9)
#  * </span><span style="color: #FFAF00;">RcppParallel </span><span> (5.0.2 &lt; 5.1.4)
#  * </span><span style="color: #FFAF00;">RcppZiggurat </span><span> (0.1.5 &lt; 0.1.6)
#  * </span><span style="color: #FFAF00;">Rfast        </span><span> (2.0.1 &lt; 2.0.3)
#  * </span><span style="color: #FF0087;">rlang        </span><span> (0.4.11)
#  * </span><span style="color: #FFAF00;">tzdb         </span><span> (0.1.1 &lt; 0.1.2)
#  * </span><span style="color: #FFAF00;">vctrs        </span><span> (0.3.7 &lt; 0.3.8)
#  * </span><span style="color: #FF0087;">yaml         </span><span> (2.2.1)
#  * </span><span style="color: #FF0087;">zoo          </span><span> (1.8.9)
</span></CODE>
<code class="r"># Resets the fastverse to defaults, removing any permanent modifications
fastverse_reset()</code>
</PRE>

</details>
<p> </p>

In addition to a global customization, separate *fastverse*'s can be created for projects by adding a `.fastverse` config file in the project directory and listing packages there. Only these packages will then be loaded and managed with `library(fastverse)` in the project. 

### Suggested Extensions

High-performing packages for different data manipulation and statistical computing topics are suggested below. Each topic has a 2-character topic-id, which can be used to quickly attach all available packages with `fastvere_extend(topcis = c(..id's..))`, and to install missing packages by adding argument `install = TRUE`. The majority of these packages provide compiled code and have few dependencies. The total (recursive) dependency count is indicated for each package. 

***

#### Time Series (TS)

- **xts** and **zoo**: Fast and reliable matrix-based time series classes providing fully identified ordered observations and various utilities for plotting and computations (1 dependency).

- **roll**: Very fast rolling and expanding window functions for vectors and matrices (3 dependencies).

  *Notes*: *xts*/*zoo* objects are preserved by *roll* functions and by *collapse*'s time series and data transformation functions^[*collapse* functions can also handle irregular time series, but this requires passing an integer time variable to the `t` argument which has consecutive integer steps for regular parts of the time series and non-consecutive integers for the irregular parts.]. As *xts*/*zoo* objects are matrices, all *matrixStats* functions apply to them as well. *xts* objects can also easily be converted to and from *data.table*. 
  
<!-- Passing the `xts::index()` coerced to integer to the `t` argument of *collapse*'s `flag`, `fdiff` and `fgrowth` further allows exact time-based computations on irregularly spaced time series, which is not supported by *xts*'s built-in functions. -->  

#### Dates and Times (DT)

- **lubridate**: Facilitates POSIX-based computations (2 dependencies).

- **clock**: Comprehensive library for date-time manipulations using a new family of orthogonal date-time classes (durations, time points, zoned-times, and calendars) (6 dependencies).

- **anytime**: Anything to 'POSIXct' or 'Date' Converter (2 dependencies).

- **fasttime**: Fast parsing of strings to 'POSIXct' (0 dependencies).

- **timechange**: Efficient manipulation of date-times accounting for time zones and daylight saving times (1 dependency).

  *Notes*: Date and time variables are preserved in many *data.table* and *collapse* operations. *data.table* additionally offers an efficient integer based date class 'IDate' with some supporting functionality. *xts* and *zoo* also provide various functions to transform dates. 
  
  <!-- - **nanotime**: (7 dependencies). -->

#### Strings (ST)

- **stringi**: Main R package for fast, correct, consistent, and convenient string/text manipulation (backend to *stringr* and *snakecase*) (0 dependencies).

- **stringr**: Simple, consistent wrappers for common string operations, based on *stringi* (3 dependencies).

- **snakecase**: Convert strings into any case, based on *stringi* and *stringr* (4 dependencies).

#### Statistics and Computing (SC)

- **Rfast** and **Rfast2**: Heterogeneous sets of fast functions for statistics, estimation and data manipulation operating on vectors and matrices. Missing values and object attributes are not (consistently) supported (4-5 dependencies).

- **parallelDist**: Multi-threaded distance matrix computation (3 dependencies).

- **coop**: Fast implementations of the covariance, correlation, and cosine similarity (0 dependencies).

  *Notes*: *Rfast* has a number of like-named functions to *matrixStats*. These are simpler but typically faster and support multi-threading. Some highly efficient statistical functions can also be found scattered across various other packages, notable to mention here are *Hmisc* (60 dependencies) and *DescTools* (17 dependencies). *fastDummies* (16 dependencies) implements creation of dummy (binary) variables. 

<!-- 
- **fastmatch**: Fast match function.
- **fastmap**: Fast Implementation of a Key-Value Store.
- **fastDummies**: Fast Creation of Dummy (Binary) Columns and Rows from Categorical Variables. (16 dependencies)
-->  

#### Spatial (SP)

- **sf**: Leading framework for geospatial computing and manipulation in R, offering a simple and flexible spatial data frame and supporting functionality (13 dependencies). 

- **stars**: Spatiotemporal data (raster and vector) in the form of dense arrays, with space and time being array dimensions (17 dependencies). 

- **terra**: Methods for spatial data analysis with raster and vector data. Processing of very large (out of memory) files is supported (4 dependencies).

  *Notes*: *collapse* can be used for efficient manipulation and computations on *sf* data frames. *sf* also offers tight integration with *dplyr*.


#### Visualization (VI)

- **dygraphs**: Interface to 'Dygraphs' interactive time series charting library (11 dependencies). 

- **lattice**: Trellis graphics for R (0 dependencies). 

- **grid**: The grid graphics package (0 dependencies). 

- **ggplot2**: Create elegant data visualizations using the Grammar of Graphics (30 dependencies). 

- **scales**: Scale functions for visualizations (10 dependencies). 

  *Notes:* *latticeExtra* provides extra graphical utilities base on *lattice*. *gridExtra* provides miscellaneous functions for *grid* graphics (and consequently for *ggplot2* which is based on *grid*). *gridtext* provides improved text rendering support for *grid* graphics. Many packages offer *ggplot2* extensions, (typically starting with 'gg') such as *ggExtra*, *ggalt*, *ggforce*, *ggmap*, *ggtext*, *ggthemes*, *ggrepel*, *ggridges*, *ggfortify*, *ggstatsplot*, *ggeffects*, *ggsignif*, *GGally*, *ggcorrplot*, *ggdendro*, etc...


#### Tidyverse-like data manipulation built on *data.table* (TV)

- **tidytable**: A tidy interface to *data.table* that is *rlang* compatible. Quite comprehensive implementation of *dplyr*, *tidyr* and *purr* functions. *tidyverse* function names are appended with a `.` e.g. `mutate.()`. Package uses a class *tidytable* that inherits from *data.table*. The `dt()` function makes *data.table* syntax pipeable (14 total dependencies). 

- **tidyfast**: Fast tidying of data. Covers *tidyr* functionality, `dt_` prefix, preserves *data.table* object. Some unnecessary deep copies (2 dependencies). 

- **tidyfst**: Tidy verbs for fast data manipulation. Covers *dplyr* and some *tidyr* functionality. Functions have `_dt` suffix and preserve *data.table* object. A [cheatsheet](<https://raw.githubusercontent.com/hope-data-science/tidyfst/master/docs/tidyfst_cheatsheet.pdf>) is provided (7 dependencies). 

- **tidyft**: Tidy verbs for fast data operations by reference. Best for big data manipulation on out of memory data using facilities provided by *fst* (7 dependencies).

- **maditr**: Fast data aggregation, modification, and filtering with pipes and *data.table*. Minimal implementation with functions `let()` and `take()` for most common data manipulation tasks. Also provides Excel-like lookup functions (2 dependencies). 

  *Notes*: One could also mention Rstudio's *dtplyr* and the *table.express* package here, but these packages import *dplyr* and thus have a around 20 dependencies.
  
  
***
  
#### Adding to this list  

Feel free to notify me of any other packages you think should be included here. Such packages should be well designed, top-performing, low-dependency, and, with few exceptions, provide own compiled code. Please note that the *fastverse* focuses on general purpose statistical computing and data manipulation, thus I won't include fast packages to estimate specific kinds of models here (of which R also has a great many). 
