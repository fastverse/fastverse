# Sumission comments: 

# Options set at the end of code, no possibility of it to fail...


# TODO: 
# function argument consistency? -> pkg or pck - > Ok
# Options: "." or "_" ? -> "." is used by tidyverse and data.table and kit: better !!
# Suggested extensions: Less is better: Only quality code !!
# Harmonize description, vignette, package page. -> Ok
# matrixStats new useNames argument can be passed down ??? Same behavior ? -> yes !!
# Spell check !! -> yes !
# Check local config file!! packages loaded in the order you say in the vignette ?? -> yes !!
# Check documentation links !! -> Ok
# MISSING GH LINKS (README !!) -> Ok
# Update DESCRIPTION !! (Links, URL) -> Ok

# Clearer use of terms 'loaded' and 'attached'. -> yes !

# Update Logo !!!! -> Ok




# library(RWsearch)
# 
# # How it works: library(fastverse) loads core fastverse. 
# # then function fastverse_extend(..., topics = NULL) by default loads all installed others,
# # alternatively specify topics...
# 
# 
# .core <- .c(data.table, collapse, magrittr, matrixStats, kit, fst)
# crandb_down()
# 
# # Core ext
# Rfast, Rfast2, fastmatch, 
# fastmap, fastDummies, parallelDist # -> very specialized...
# 
# # Time Series
# xts, zoo, roll, runner
# # specialized: rollRegres, RolWinMulCor, seismicRoll,
# # RcppRoll -> Does not preserve attribures... less functions than roll... and not really maintained..
# # caTools -> even slower rolling functions
# # accelerometry -> Interesting but very specialized
# 
# # runner: Improved lag function... and rolling stats -> Interesting... written in C it seems 
# # slider: too many dependencies...
# # runstats: Very simple, and not compiled code.. 
# # tbrf: fastverse - many dependencies...
# 
# # Dates and Times 
# lubridate, fasttime
# 
# # Strings
# stringr or stringi
# 
# # Linear Algebra
# # EigenR, - apparently not faster than base R
# # pracma - Take a more careful look. 
# 
# # Spatial
# sf
# 
# # Tidy like 
# # DTSg, 
# tidy_pck <- cnsc(maditr, table.express, tidyfast, tidyft, tidyfst, tidytable)
# 
# 
# 
# p_display7(tidy_pck)
# p_html(tidy_pck)
# p_page(tidy_pck)
# 
# fst_pck <- s_crandb(fast), high, C, performance, mode = "relax")
# fst_pck <- s_crandb(fast, matrix, operations, mode = "relax")
# p_display5(fst_pck)
# p_page("caTools")
# 
# corpcor
# 
# lubridate
# # ff, sparsepp, fastmap, fts, csvread, hashr
# # caTools, fastDummies, parallelDist, rollRegres, matrixTests, EigenR, pracma
# fastmatrix
# # funique package -> Not faster than kit or collapse ...
# # Also runstats -> check out, but no compilation suggests slow...
# p_display5(cnsc(chron, timeDate, tis, zoo))
# p_page("runstats")
# 
# # Covariance / correlation: 
# # coop  -> This is it !!
# # corpcor,  -> Nah, strange....
# # covglasso -> Nah
# 
# # Generlly interesting: dLagM, rtkore, dbx, pak, L0Learn, mlpack, tsdb, xyz, hashr, BMisc, descstatsr,FAOSTAT, ff, ForestTools, manhattanly
# 
# # Estimation functions:
# ranger, fixest, miceFast, missRanger
