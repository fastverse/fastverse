# fastverse 0.2.2

* Added possibility to set global options and environment variables in project configuration files. See Vignette for details. 

* Added new packages to [suggested packages list](https://sebkrantz.github.io/fastverse/#suggested-extensions): *rrapply*, *MatrixExtra* and *rsparse*.

* Package does not import any helper functions from *collapse* anymore, so that hard detaching with `fastverse_detach(unload = TRUE)` is possible for *collapse* as well.

# fastverse 0.2.1

* Remove invalid URL and lifecycle badge (package is now considered stable).

# fastverse 0.2.0

* Making sure no files are written to disc on CRAN servers. 

* `fastverse_update()` has an additional argument `install`. The default is `install = FALSE`, which will just print the installation command as before, asking you to run it in a clean R session, whereas `install = TRUE` will instead execute the command. 

* Added a function `fastverse_install()` to check package availability and install any missing *fastverse* packages before calling `library(fastverse)`. Useful especially for custom *fastverse* configurations (inside projects) to safeguard the availability of packages.

* Added a function `fastverse_child()` (following a request), which allows users to create a wholly different package-verse like the *fastverse* - under a different name and with different core packages. Such *fastverse* children inherit 90% of the functionality of the *fastverse* package (e.g. they are not permanently globally extensible, but can be configured for projects and extended in the session). Childbearing requires an internet connection but no additional packages (like `devtools`). 

* Export utility functions `is_installed` and `is_attached` to check if packages are installed / attached. 

* Several additions to suggested extensions list: `qs`, `arrow`, `stringdist`, `stringfish`, `nanotime`.

# fastverse 0.1.8
CRAN required me to place return values on `fastverse_extend()` and `fastverse_detach()` - which do not return anything.

# fastverse 0.1.7
Implementing various CRAN comments on 0.1.6 (small things), and setting up synchronous development of CRAN and GitHub version through a 'development' branch which can be merged into both. Pull requests should be sent to the 'development' branch. 

# fastverse 0.1.6
CRAN rejected 0.1.5, which made sure *matrixStats* handles attributes consistently whenever the *fastverse* is attached, because CRAN packages are not allowed to modify the namespace of other packages. Thus 0.1.6 comes in two versions: A CRAN version which takes *matrixStats* as it is, and a GitHub version which is the original 0.1.5. User who value a consistent *matrixStats* that preserves dimension names in all functions are recommended the GitHub version. 

# fastverse 0.1.5
First *fastverse* CRAN submission on 7th August 2021. Development started in Spring 2021. 
