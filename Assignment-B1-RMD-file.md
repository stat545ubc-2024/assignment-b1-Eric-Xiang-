Assignment B1
================
Eric Xiang
2024-10-24

``` r
#import 
library(palmerpenguins)# use penguin dataset to test the function 
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(testthat)
```

    ## 
    ## Attaching package: 'testthat'
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     is_null
    ## 
    ## The following objects are masked from 'package:readr':
    ## 
    ##     edition_get, local_edition
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     matches
    ## 
    ## The following object is masked from 'package:dplyr':
    ## 
    ##     matches

``` r
library(usethis)
library(devtools)#for marking down the function 
```

    ## 
    ## Attaching package: 'devtools'
    ## 
    ## The following object is masked from 'package:testthat':
    ## 
    ##     test_file

``` r
penguins
```

    ## # A tibble: 344 × 8
    ##    species island    bill_length_mm bill_depth_mm flipper_length_mm body_mass_g
    ##    <fct>   <fct>              <dbl>         <dbl>             <int>       <int>
    ##  1 Adelie  Torgersen           39.1          18.7               181        3750
    ##  2 Adelie  Torgersen           39.5          17.4               186        3800
    ##  3 Adelie  Torgersen           40.3          18                 195        3250
    ##  4 Adelie  Torgersen           NA            NA                  NA          NA
    ##  5 Adelie  Torgersen           36.7          19.3               193        3450
    ##  6 Adelie  Torgersen           39.3          20.6               190        3650
    ##  7 Adelie  Torgersen           38.9          17.8               181        3625
    ##  8 Adelie  Torgersen           39.2          19.6               195        4675
    ##  9 Adelie  Torgersen           34.1          18.1               193        3475
    ## 10 Adelie  Torgersen           42            20.2               190        4250
    ## # ℹ 334 more rows
    ## # ℹ 2 more variables: sex <fct>, year <int>

``` r
summarize_data<- function(data,...,summ_vars,na.rm=FALSE){
    
  if (!is.numeric(data[[deparse(substitute(summ_vars))]])) {
    stop("Input summ_var is not in the data set or is not a numeric variable, double check")
  }
#extra variable is used in grouping if needed
  data%>%
    group_by(...)%>% 
    summarize(median=median({{summ_vars}},na.rm=na.rm),mean=mean({{summ_vars}},na.rm=na.rm),sd=sd({{summ_vars}},na.rm=na.rm)) 
    }
```

``` r
#Exercise 2 documentation with Roxygen 
#Roxygen: list(markdown = TRUE)
```

``` r
#exercise include examples 

#working example. We utilize the penguin data to provide example. This is to show the functional example. The function first groups the data by species and island. Then, it summarizes the bill_length_mm in term of mdian, mean and sd for each group. 
summarize_data(penguins,species,island,summ_vars=bill_length_mm
,na.rm=TRUE)
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 5
    ## # Groups:   species [3]
    ##   species   island    median  mean    sd
    ##   <fct>     <fct>      <dbl> <dbl> <dbl>
    ## 1 Adelie    Biscoe      38.7  39.0  2.48
    ## 2 Adelie    Dream       38.6  38.5  2.47
    ## 3 Adelie    Torgersen   38.9  39.0  3.03
    ## 4 Chinstrap Dream       49.6  48.8  3.34
    ## 5 Gentoo    Biscoe      47.3  47.5  3.08

``` r
#failed to show the result because the summ_vars supplied is not a numeric variable 

summarize_data(penguins,species,island,summ_vars=sex
,na.rm=TRUE)
```

    ## Error in summarize_data(penguins, species, island, summ_vars = sex, na.rm = TRUE): Input summ_var is not in the data set or is not a numeric variable, double check

``` r
summarize_data(penguins,island,summ_vars=bill_length_mm
,na.rm=FALSE)
```

    ## # A tibble: 3 × 4
    ##   island    median  mean    sd
    ##   <fct>      <dbl> <dbl> <dbl>
    ## 1 Biscoe      NA    NA   NA   
    ## 2 Dream       44.7  44.2  5.95
    ## 3 Torgersen   NA    NA   NA

``` r
#test to confirm a summary tibble is generated
test_that("A table is generated as the ouput", 
          {expect_gt(length(summarize_data(penguins,species,summ_vars=bill_length_mm
,na.rm=TRUE)),0)
          expect_gt(nrow(summarize_data(penguins,species,summ_vars=bill_length_mm,
na.rm=FALSE)),0)
  })
```

    ## Test passed 🎉

``` r
#run into error because providing non_numeric variable or providing a group_by variable not existed in the data 
test_that("run into error with non_numeric summ_var",
 {expect_error(summarize_data(penguins,island,summ_vars=sex
,na.rm=TRUE))
  expect_error(summarize_data(penguins,languages,summ_vars=sex
,na.rm=TRUE))
})
```

    ## Test passed 🥇
