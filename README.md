
<!-- README.md is generated from README.Rmd. Please edit that file -->

# cpdata

<!-- badges: start -->
<!-- badges: end -->

cpdata provides customized preprocessing steps for specific datasets
released by the Chicago Police Department.

## Installation

To install cpdata:

## Example

Preprocess ticket data:

``` r
library(cpdata)
file <- "parking_tickets.csv"
tidy_ticket(
  system.file("extdata", file, package = "cpdata", mustWork = TRUE)
)
#> Finished preprocessing: /private/var/folders/qw/kjq09mn10l5cmw355f10qr2h0000gn/T/RtmpM1OQSG/temp_libpath869a76936ae9/cpdata/extdata/parking_tickets.csv
#>     star     ticket                  dt violation_code
#>    <int>      <i64>              <POSc>         <char>
#> 1: 12511   35125343 2001-03-02 18:50:00       0964100C
#> 2: 15985   32807228 2000-09-21 21:45:00       0964190A
#> 3:   178 9061102579 1999-10-08 07:57:00       0964150B
#> 4: 15985   32807221 2000-09-21 21:18:00       0964190A
#> 5:  3777   33939512 2000-09-21 21:20:00       0964150B
#>                                   violation department  unit fine_1 fine_2
#>                                      <char>     <char> <int>  <int>  <int>
#> 1:     BLOCK ACCESS/ALLEY/DRIVEWAY/FIRELANE        CPD    16     50    100
#> 2: EXP. METER NON-CENTRAL BUSINESS DISTRICT        CPD    24     30     60
#> 3:      PARKING/STANDING PROHIBITED ANYTIME        DOF   498     30     60
#> 4: EXP. METER NON-CENTRAL BUSINESS DISTRICT        CPD    24     30     60
#> 5:      PARKING/STANDING PROHIBITED ANYTIME        CPD    18     50    100
#>    current_amount_due total_payments     queue queue_date hearing_disposition
#>                 <int>          <int>    <char>     <IDat>              <char>
#> 1:                  0             50      Paid 2001-03-09                    
#> 2:                 30              0    Define 2000-09-28                    
#> 3:                  0             30      Paid 2003-01-10                    
#> 4:                 30              0    Define 2000-09-28                    
#> 5:                  0             50 Dismissed 2001-10-26          Not Liable
#>     longitude  latitude accuracy
#>        <char>    <char>   <char>
#> 1: -87.692567 41.997949        1
#> 2: -87.613374 41.865965        1
#> 3: -87.578981 41.762668        1
#> 4: -87.613374 41.865965        1
#> 5:         NA        NA       NA
```
