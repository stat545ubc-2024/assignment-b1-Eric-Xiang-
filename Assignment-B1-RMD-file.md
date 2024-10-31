Assignment B1
================
Eric Xiang
2024-10-31

``` r
library(palmerpenguins)# use penguin dataset to test the function 
library(dplyr)#for data manipulation
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
library(tidyverse)#data manipulation 
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
library(testthat)#for testing the function 
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

**Exercise 1:** Create the function. The function summarize_data intends
to calculate the median, mean, and standard deviation (sd) under certain
grouping condition in a dataset. An na.rm argument is used to indicate
if missing value (NA) should be removed or not from the data.

It will show error message if summ_vars is not a numeric.

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

**Exercise 2:** Documentation with Roxygen2

``` r
#' Title: Summarize the variables in the provided data 
#'Function description: This function is to summarize the numeric variable in a dataset when given one or more grouping condition. It will generate a summary tibble to show the calculated median, mean and standard deviation of the numeric variable under the indicated grouping conditions. There is an option for user to ignore missing value with na.rm arguement. 

#' @param data: a data set in the data frame format 
#' @param ...  ellipse, indicating one or multiple grouping variables, has to be a categorical variable existed in the dataset to group the population  
#' @param summ_vars: a summarizing variable must be numeric variable and existed in the dataset 
#'@param na.rm: logical variable to indicate if missing value should be removed or not. default is FALSE(not remove) 
#' @return A summary tibble with calculated  mean, median and standard deviation of the indicated variables. 
#' @examples
#' summarize_data(penguins,species,island,summ_vars=bill_length_mm,na.rm=TRUE)
#' summarize_data(penguins,sex,island,summ_vars=bill_depth_mm,na.rm=TRUE)
```

**Exercise3:** Exercise includes examples

**Working example:** I utilized the penguin data to provide example. The
function first grouped the data by species and island. Then, it
summarized the bill_length_mm in terms of mdian, mean and standard
deviation(sd) for each group.

**Not working example:** .I provided a summ_vars that is not numeric to
see if an error message appears. I observed that the expected error
message appeared.

I also provided with grouping variables not existed in the dataset and
found the expected error message appeared reminding me that the grouping
variable was not found in the dataset.

``` r
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
summarize_data(penguins,languages,island,summ_vars=bill_length_mm
,na.rm=TRUE)
```

    ## Error in `group_by()`:
    ## ! Must group by variables found in `.data`.
    ## ✖ Column `languages` is not found.

**Exercise_4:** Test the function

Three tests were created to test the ouput of my function.

The **first test** is to show if the table is generated. Ideally, a
summary tibble should be generated with one or more than one row(s) and
column(s).

The **second test** is to confirm error messages are shown when giving
the function unexpected output, either a non-numeric input for summ-vars
or a grouping variable not found in the dataset

The **third test** is to confirm if the expected values are calculated
when using the summarize_data function. The result of the function is
compared with the output generated using dplyr.

``` r
#Test to confirm a summary tibble is generated
test_that("A table is generated as the ouput", 
          {expect_gt(length(summarize_data(penguins,species,summ_vars=bill_length_mm
,na.rm=TRUE)),0)
          expect_gt(nrow(summarize_data(penguins,species,summ_vars=bill_length_mm,
na.rm=FALSE)),0)
  })
```

    ## Test passed 🥳

``` r
#Test if rejecting non_numeric variable as the summ_vars input or providing a group_by variable not existed in the data 
 
test_that("run into error with non_numeric summ_var",
 {expect_error(summarize_data(penguins,island,summ_vars=sex
,na.rm=TRUE))
  expect_error(summarize_data(penguins,languages,summ_vars=sex
,na.rm=TRUE))
})
```

    ## Test passed 😀

``` r
#Test if an expected value is generated 
#I first generated the median results using dplyr filtering to confirm the ideal output 

expected_mean<-penguins%>%
  group_by(species,island)%>%
  summarize(median=median(bill_length_mm,na.rm=TRUE),mean=mean(bill_length_mm,na.rm=TRUE),sd=sd(bill_length_mm,na.rm=TRUE))
```

    ## `summarise()` has grouped output by 'species'. You can override using the
    ## `.groups` argument.

``` r
#I then ran  the test to see if the same output is generated

test_that ("generate the same out put",{
 expect_equal(summarize_data(penguins,species,island,summ_vars=bill_length_mm
,na.rm=TRUE),expected_mean)
  })
```

    ## Test passed 🎊
