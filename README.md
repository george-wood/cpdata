
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpdata

<!-- badges: start -->
<!-- badges: end -->

cpdata provides customized preprocessing steps for several datasets
released by the Chicago Police Department.

## Installation

To install cpdata:

``` r
install.packages("devtools")
devtools::install_github("george-wood/cpdata")
```

## Example

Preprocess parking ticket data:

``` r
library(cpdata)
file <- "parking_tickets.csv"
tidy_ticket(
  system.file("extdata", file, package = "cpdata", mustWork = TRUE)
)
#>      star        eid                  dt violation_code
#>    <char>     <char>              <POSc>         <char>
#> 1:  12511   35125343 2001-03-02 18:50:00       0964100C
#> 2:  15985   32807228 2000-09-21 21:45:00       0964190A
#> 3:    178 9061102579 1999-10-08 07:57:00       0964150B
#> 4:  15985   32807221 2000-09-21 21:18:00       0964190A
#> 5:   3777   33939512 2000-09-21 21:20:00       0964150B
#>                                   violation department   unit     queue
#>                                      <char>     <char> <char>    <char>
#> 1:     BLOCK ACCESS/ALLEY/DRIVEWAY/FIRELANE        CPD     16      Paid
#> 2: EXP. METER NON-CENTRAL BUSINESS DISTRICT        CPD     24    Define
#> 3:      PARKING/STANDING PROHIBITED ANYTIME        DOF    498      Paid
#> 4: EXP. METER NON-CENTRAL BUSINESS DISTRICT        CPD     24    Define
#> 5:      PARKING/STANDING PROHIBITED ANYTIME        CPD     18 Dismissed
#>    queue_date disposition longitude latitude accuracy fine_1 fine_2
#>        <POSc>      <char>     <num>    <num>    <int>  <num>  <num>
#> 1: 2001-03-09        <NA> -87.69257 41.99795        1     50    100
#> 2: 2000-09-28        <NA> -87.61337 41.86597        1     30     60
#> 3: 2003-01-10        <NA> -87.57898 41.76267        1     30     60
#> 4: 2000-09-28        <NA> -87.61337 41.86597        1     30     60
#> 5: 2001-10-26  Not Liable        NA       NA       NA     50    100
#>    current_amount_due total_payments
#>                 <num>          <num>
#> 1:                  0             50
#> 2:                 30              0
#> 3:                  0             30
#> 4:                 30              0
#> 5:                  0             50
```
