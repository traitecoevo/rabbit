
<!-- README.md is generated from README.Rmd. Please edit that file 
To regenerate, run `quarto::quarto_render("Readme.Rmd", output_file = "README.md")`
-->

# rabbit

<!-- badges: start -->
<!-- badges: end -->

The goal of rabbit is to …

Use package `RcppRoll` to optimise claulcations of metrics clculated in
wolling windows, e.g. rolling mean, var, sd etc.

## Installation

You can install the development version of rabbit from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("traitecoevo/rabbit")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rabbit)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union

#Read file
data <- arrow::read_parquet("tests/testthat/raw_Pic2Jan_50000.parquet") |> slice(1:1000)

# Calcualte metrics
data_metrics <- moving_window_calcs_2(data)

data_metrics %>% slice(50:150)
#> # A tibble: 101 × 26
#>    time                meanX  meanY meanZ  maxx   maxy  maxz  minx   miny  minz    sdx     sdy    sdz   SMA minODBA maxODBA minVDBA maxVDBA sumODBA sumVDBA   corXY   corXZ  corYZ    skx       sky
#>    <dttm>              <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl>  <dbl> <dbl>  <dbl>   <dbl>  <dbl> <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>  <dbl>  <dbl>     <dbl>
#>  1 1970-01-01 00:00:00 0.592 -0.317 0.713  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0142 0.00826 0.0119  1.62    1.56    1.66   0.942    1.01    81.1    49.0 -0.0804 -0.0423 0.216  -0.237 -0.000909
#>  2 1970-01-01 00:00:00 0.592 -0.317 0.713  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0144 0.00861 0.0124  1.62    1.56    1.66   0.942    1.01    81.1    49.0 -0.0143 -0.0886 0.0836 -0.254  0.0923  
#>  3 1970-01-01 00:00:00 0.593 -0.317 0.712  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0138 0.00847 0.0124  1.62    1.56    1.66   0.942    1.01    81.1    49.0 -0.0732 -0.0708 0.0921 -0.228  0.0403  
#>  4 1970-01-01 00:00:00 0.594 -0.317 0.713  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0131 0.00858 0.0119  1.62    1.58    1.66   0.957    1.01    81.2    49.0 -0.0514 -0.172  0.128  -0.149  0.0971  
#>  5 1970-01-01 00:00:00 0.593 -0.317 0.713  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0136 0.00844 0.0119  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0361 -0.177  0.114  -0.195  0.0457  
#>  6 1970-01-01 00:00:00 0.592 -0.317 0.714  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0142 0.00843 0.0116  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0433 -0.235  0.141  -0.225  0.0585  
#>  7 1970-01-01 00:00:00 0.592 -0.317 0.714  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0144 0.00843 0.0116  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0253 -0.265  0.104  -0.240  0.0585  
#>  8 1970-01-01 00:00:00 0.592 -0.316 0.714  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0143 0.00834 0.0115  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0559 -0.260  0.115  -0.295  0.0697  
#>  9 1970-01-01 00:00:00 0.592 -0.316 0.714  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0143 0.00819 0.0115  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0523 -0.262  0.129  -0.295  0.0232  
#> 10 1970-01-01 00:00:00 0.592 -0.316 0.714  0.61 -0.297 0.735 0.563 -0.328 0.688 0.0141 0.00819 0.0119  1.62    1.58    1.66   0.957    1.01    81.1    49.0 -0.0622 -0.264  0.135  -0.283  0.0232  
#> # ℹ 91 more rows
#> # ℹ 1 more variable: skz <dbl>
```

Speed comparison on test file, comparing 3 techniques

``` r
source("tests/testthat/doAccloop.R")

microbenchmark::microbenchmark(
  doAccloop_all(data),
  moving_window_calcs(data),
  moving_window_calcs_2(data)
)
#> Warning in microbenchmark::microbenchmark(doAccloop_all(data), moving_window_calcs(data), : less accurate nanosecond times to avoid potential integer overflows
#> Unit: milliseconds
#>                         expr         min          lq        mean      median          uq        max neval cld
#>          doAccloop_all(data) 1874.630536 1924.027336 1957.852846 1944.725366 1975.835469 2156.93837   100  a 
#>    moving_window_calcs(data)    8.334029    8.542699    8.890808    8.678962    8.841568   12.56634   100   b
#>  moving_window_calcs_2(data)    7.946661    8.187188    8.807385    8.397169    8.768752   12.94694   100   b
```

Speed comparison on a big test file, comparing the 2 faster techniques

``` r
df <- arrow::read_parquet('/Users/dfalster/GitHub/projects/Adams-Accelerometer_2024/data/Pic2Jan_S1.parquet')
nrow(df)
#> [1] 23196001
```

version 1

``` r
system.time(moving_window_calcs(df))
#>    user  system elapsed 
#>  73.113   7.256  81.563
```

version 2

``` r
system.time(moving_window_calcs_2(df))
#>    user  system elapsed 
#>  41.972   4.096  46.985
```
