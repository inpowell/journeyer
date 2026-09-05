
<!-- README.md is generated from README.Rmd. Please edit that file -->

# journeyer

<!-- badges: start -->

[![R-CMD-check](https://github.com/inpowell/journeyer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/inpowell/journeyer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `journeyer` is to provide flexible strategies to follow
patient journeys through administrative data from NSW Health. The
simplest administrative patient journey is the *period of hospital care*
(POHC), which aims to capture all episodes of care in a hospitalisation
from the time a patient is admitted to when they are formally
discharged.

## Installation

You can install the development version of `journeyer` from GitHub using
the `remotes` package:

``` r
remotes::install_github('inpowell/journeyer')
```

## Example

The following presents an example of how the `journeyer` package should
be used. This uses an artificial dataset that mimics some of the
patterns found in the NSW APDC. The toy APDC dataset contains 21,892
records and 9 variables. `RL_ID` is a unique record identifier, while
`PPN` is a person identifier. The facility columns all represent the
first mentioned facility for a person by A, the second by B, and so on.
The source of referral and mode of separation columns use codes as
defined in the [NSW APDC data
dictionary](https://www.cherel.org.au/data-dictionaries).

``` r
library(dplyr) # For data manipulation
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(data.table)
#> 
#> Attaching package: 'data.table'
#> The following objects are masked from 'package:dplyr':
#> 
#>     between, first, last
#> The following object is masked from 'package:base':
#> 
#>     %notin%

library(journeyer)

data(toy_apdc)
head(toy_apdc, 10L)
#>                                RL_ID   PPN source_of_referral_recode
#>                               <hash> <int>                    <char>
#>  1: b2cd17b85f89befacfdc89a194bc6b64  3411                        07
#>  2: 9a1426d5bc9a7232fdcf7fcc1ed3ce73  3411                        07
#>  3: 8f337886bb80377c4129fa4a708e163f 14729                        07
#>  4: 33915b935904e38e7299cd3d4e040682  9801                        07
#>  5: a0db5e74832b16a26b20052088e5c8bd   663                        07
#>  6: 69cbaaabc925e7714c346b3718bd3856  5579                        07
#>  7: cf63e6bc809ff6240852c470a931151f  6350                        07
#>  8: 79e219a17878df547879fb8d2b53df1c  1211                        01
#>  9: c45a57a0547971c9e6e0f10f3de9861c  1211                        04
#> 10: e03e795af41aca093831c1fe7cc8a095 16955                        07
#>     mode_of_separation_recode       episode_start         episode_end
#>                        <char>              <POSc>              <POSc>
#>  1:                         1 2001-07-20 01:05:55 2001-07-20 06:20:55
#>  2:                         1 2001-07-20 01:21:55 2001-07-20 06:20:55
#>  3:                         1 2012-03-27 13:47:51 2012-03-27 22:57:51
#>  4:                         1 2010-09-23 19:48:55 2010-09-25 01:03:55
#>  5:                         1 2019-11-11 07:56:24 2019-11-11 14:22:24
#>  6:                         1 2022-08-17 20:51:36 2022-08-18 00:51:36
#>  7:                         1 2015-10-31 19:44:48 2015-10-31 20:24:48
#>  8:                         5 2019-12-16 09:11:22 2019-12-19 03:02:22
#>  9:                         1 2019-12-19 04:02:22 2019-12-20 07:35:22
#> 10:                         1 2008-06-27 15:11:27 2008-06-27 19:51:27
#>     facility_identifier_recode facility_trans_from_recode
#>                         <char>                     <char>
#>  1:                          A                       <NA>
#>  2:                          B                       <NA>
#>  3:                          A                       <NA>
#>  4:                          A                       <NA>
#>  5:                          A                       <NA>
#>  6:                          A                       <NA>
#>  7:                          A                       <NA>
#>  8:                          A                       <NA>
#>  9:                          B                          A
#> 10:                          A                       <NA>
#>     facility_trans_to_recode
#>                       <char>
#>  1:                     <NA>
#>  2:                     <NA>
#>  3:                     <NA>
#>  4:                     <NA>
#>  5:                     <NA>
#>  6:                     <NA>
#>  7:                     <NA>
#>  8:                        B
#>  9:                     <NA>
#> 10:                     <NA>
```

Using this data we have read in, we now find links between these
episodes. To start, we need to join the dataset to itself using the PPN
column, and filter out pairs relating to the same record. By convention,
the suffixes to resolve name collisions should be `_parent` and
`_child`.

``` r
pairs <- inner_join(toy_apdc, toy_apdc, by = 'PPN',
                    suffix = c('_parent', '_child'),
                    relationship = 'many-to-many') |>
  filter(RL_ID_parent != RL_ID_child)
```

Now that the pairs are lined up side-by-side, we can use the default
predicates for the NSW APDC as they are present in the package. For
convenience, we keep only the identifier columns.

``` r
links <- pairs |>
  find_links(!!!default_predicates$NSW$APDC_APDC) |>
  select(RL_ID_parent, RL_ID_child)

head(links)
#>                        RL_ID_parent                      RL_ID_child
#>                              <hash>                           <hash>
#> 1: b2cd17b85f89befacfdc89a194bc6b64 9a1426d5bc9a7232fdcf7fcc1ed3ce73
#> 2: 79e219a17878df547879fb8d2b53df1c c45a57a0547971c9e6e0f10f3de9861c
#> 3: d7265e4a40c784680bf62aae4415dbc0 c7e865d78c9ea18a62a5dc5bdba36621
#> 4: f2082d1046964ab9ccfae6e94782299d 1f6f0b22513e651e6d30b10335f448a5
#> 5: 128360f7351815bf5dd330faee3cf3cf 1ea68b1bb2d47fdf631f2859ce4076c3
#> 6: cb86c78971f5b22104870cdc5be1fc63 2dcaf3ee77e154ed5f3df733333334f1
```

Now, we load on a new `journey` column onto the toy data, which requires
the toy data to be in `data.table` format.

``` r
setDT(toy_apdc)
collect_journeys(toy_apdc, links, identifier = 'RL_ID')

head(toy_apdc, 10L)
#>                                RL_ID   PPN source_of_referral_recode
#>                               <hash> <int>                    <char>
#>  1: b2cd17b85f89befacfdc89a194bc6b64  3411                        07
#>  2: 9a1426d5bc9a7232fdcf7fcc1ed3ce73  3411                        07
#>  3: 8f337886bb80377c4129fa4a708e163f 14729                        07
#>  4: 33915b935904e38e7299cd3d4e040682  9801                        07
#>  5: a0db5e74832b16a26b20052088e5c8bd   663                        07
#>  6: 69cbaaabc925e7714c346b3718bd3856  5579                        07
#>  7: cf63e6bc809ff6240852c470a931151f  6350                        07
#>  8: 79e219a17878df547879fb8d2b53df1c  1211                        01
#>  9: c45a57a0547971c9e6e0f10f3de9861c  1211                        04
#> 10: e03e795af41aca093831c1fe7cc8a095 16955                        07
#>     mode_of_separation_recode       episode_start         episode_end
#>                        <char>              <POSc>              <POSc>
#>  1:                         1 2001-07-20 01:05:55 2001-07-20 06:20:55
#>  2:                         1 2001-07-20 01:21:55 2001-07-20 06:20:55
#>  3:                         1 2012-03-27 13:47:51 2012-03-27 22:57:51
#>  4:                         1 2010-09-23 19:48:55 2010-09-25 01:03:55
#>  5:                         1 2019-11-11 07:56:24 2019-11-11 14:22:24
#>  6:                         1 2022-08-17 20:51:36 2022-08-18 00:51:36
#>  7:                         1 2015-10-31 19:44:48 2015-10-31 20:24:48
#>  8:                         5 2019-12-16 09:11:22 2019-12-19 03:02:22
#>  9:                         1 2019-12-19 04:02:22 2019-12-20 07:35:22
#> 10:                         1 2008-06-27 15:11:27 2008-06-27 19:51:27
#>     facility_identifier_recode facility_trans_from_recode
#>                         <char>                     <char>
#>  1:                          A                       <NA>
#>  2:                          B                       <NA>
#>  3:                          A                       <NA>
#>  4:                          A                       <NA>
#>  5:                          A                       <NA>
#>  6:                          A                       <NA>
#>  7:                          A                       <NA>
#>  8:                          A                       <NA>
#>  9:                          B                          A
#> 10:                          A                       <NA>
#>     facility_trans_to_recode journey
#>                       <char>   <int>
#>  1:                     <NA>       1
#>  2:                     <NA>       1
#>  3:                     <NA>       2
#>  4:                     <NA>       3
#>  5:                     <NA>       4
#>  6:                     <NA>       5
#>  7:                     <NA>       6
#>  8:                        B       7
#>  9:                     <NA>       7
#> 10:                     <NA>       8
```

Now, we can count the number of POHCs in the toy data:

``` r
toy_apdc[, uniqueN(journey)]
#> [1] 21033
```

We can find the start and end of each POHC, and count the number of
episodes in each POHC:

``` r
head(toy_apdc[, .(start = min(episode_start), end = max(episode_end), Neps = .N), by = 'journey'])
#>    journey               start                 end  Neps
#>      <int>              <POSc>              <POSc> <int>
#> 1:       1 2001-07-20 01:05:55 2001-07-20 06:20:55     2
#> 2:       2 2012-03-27 13:47:51 2012-03-27 22:57:51     1
#> 3:       3 2010-09-23 19:48:55 2010-09-25 01:03:55     1
#> 4:       4 2019-11-11 07:56:24 2019-11-11 14:22:24     1
#> 5:       5 2022-08-17 20:51:36 2022-08-18 00:51:36     1
#> 6:       6 2015-10-31 19:44:48 2015-10-31 20:24:48     1
```

A researcher may also calculate summary information for each POHC, such
as first primary procedure, the presence of certain diagnosis codes, and
demographic information. However, this data manipulation is beyond the
scope of this README.

Most POHCs have just one episode, which is similar to the NSW APDC:

``` r
toy_apdc[, .("Episodes in POHC" = .N), by = 'journey'][, .(POHCs = .N), keyby = 'Episodes in POHC']
#> Key: <Episodes in POHC>
#>    Episodes in POHC POHCs
#>               <int> <int>
#> 1:                1 20269
#> 2:                2   730
#> 3:                3    33
#> 4:                4     1
```

## Using different predicates

The above example uses default predicates which are coded in the
`journeyer` package. However, different predicates can be used! First,
let us consider a simple example where episodes are considered linked if
and only if they overlap. We could find these links as follows.

``` r
pairs <- inner_join(toy_apdc, toy_apdc, by = 'PPN',
                    suffix = c('_parent', '_child'),
                    relationship = 'many-to-many') |>
  filter(RL_ID_parent != RL_ID_child)

links <- pairs |>
  find_links(episode_start_parent <= episode_start_child & episode_start_child <= episode_end_parent) |>
  select(RL_ID_parent, RL_ID_child)

collect_journeys(toy_apdc, links, identifier = "RL_ID")
head(toy_apdc, 10L)
#>                                RL_ID   PPN source_of_referral_recode
#>                               <hash> <int>                    <char>
#>  1: b2cd17b85f89befacfdc89a194bc6b64  3411                        07
#>  2: 9a1426d5bc9a7232fdcf7fcc1ed3ce73  3411                        07
#>  3: 8f337886bb80377c4129fa4a708e163f 14729                        07
#>  4: 33915b935904e38e7299cd3d4e040682  9801                        07
#>  5: a0db5e74832b16a26b20052088e5c8bd   663                        07
#>  6: 69cbaaabc925e7714c346b3718bd3856  5579                        07
#>  7: cf63e6bc809ff6240852c470a931151f  6350                        07
#>  8: 79e219a17878df547879fb8d2b53df1c  1211                        01
#>  9: c45a57a0547971c9e6e0f10f3de9861c  1211                        04
#> 10: e03e795af41aca093831c1fe7cc8a095 16955                        07
#>     mode_of_separation_recode       episode_start         episode_end
#>                        <char>              <POSc>              <POSc>
#>  1:                         1 2001-07-20 01:05:55 2001-07-20 06:20:55
#>  2:                         1 2001-07-20 01:21:55 2001-07-20 06:20:55
#>  3:                         1 2012-03-27 13:47:51 2012-03-27 22:57:51
#>  4:                         1 2010-09-23 19:48:55 2010-09-25 01:03:55
#>  5:                         1 2019-11-11 07:56:24 2019-11-11 14:22:24
#>  6:                         1 2022-08-17 20:51:36 2022-08-18 00:51:36
#>  7:                         1 2015-10-31 19:44:48 2015-10-31 20:24:48
#>  8:                         5 2019-12-16 09:11:22 2019-12-19 03:02:22
#>  9:                         1 2019-12-19 04:02:22 2019-12-20 07:35:22
#> 10:                         1 2008-06-27 15:11:27 2008-06-27 19:51:27
#>     facility_identifier_recode facility_trans_from_recode
#>                         <char>                     <char>
#>  1:                          A                       <NA>
#>  2:                          B                       <NA>
#>  3:                          A                       <NA>
#>  4:                          A                       <NA>
#>  5:                          A                       <NA>
#>  6:                          A                       <NA>
#>  7:                          A                       <NA>
#>  8:                          A                       <NA>
#>  9:                          B                          A
#> 10:                          A                       <NA>
#>     facility_trans_to_recode journey
#>                       <char>   <int>
#>  1:                     <NA>       1
#>  2:                     <NA>       1
#>  3:                     <NA>       2
#>  4:                     <NA>       3
#>  5:                     <NA>       4
#>  6:                     <NA>       5
#>  7:                     <NA>       6
#>  8:                        B       7
#>  9:                     <NA>       8
#> 10:                     <NA>       9
```

Here, we have used the fact that if two episodes overlap, then one
episode starts during the other.

Some of the common types of links have been implemented as functions in
the `journeyer` package. For a complete list of these predicates, see
`?LinkPredicates`. To use these, note that you will need to use the
bang-bang operator `!!` from the tidyverse. For example, to find
transfers where both source and destination episode have evidence of a
transfer, or overlapping episodes, we could do:

``` r
links <- pairs |>
  find_links(
    overlap = !!overlap_episodes(episode_start_parent, episode_end_parent, episode_start_child),
    transfer = !!transfers_both(
      episode_end_parent, mode_of_separation_recode_parent,
      facility_identifier_recode_parent, facility_trans_to_recode_parent,
      episode_start_child, source_of_referral_recode_child,
      facility_identifier_recode_child, facility_trans_from_recode_child
    )
  ) |>
  select(RL_ID_parent, RL_ID_child)

collect_journeys(toy_apdc, links, identifier = "RL_ID")
head(toy_apdc, 10L)
#>                                RL_ID   PPN source_of_referral_recode
#>                               <hash> <int>                    <char>
#>  1: b2cd17b85f89befacfdc89a194bc6b64  3411                        07
#>  2: 9a1426d5bc9a7232fdcf7fcc1ed3ce73  3411                        07
#>  3: 8f337886bb80377c4129fa4a708e163f 14729                        07
#>  4: 33915b935904e38e7299cd3d4e040682  9801                        07
#>  5: a0db5e74832b16a26b20052088e5c8bd   663                        07
#>  6: 69cbaaabc925e7714c346b3718bd3856  5579                        07
#>  7: cf63e6bc809ff6240852c470a931151f  6350                        07
#>  8: 79e219a17878df547879fb8d2b53df1c  1211                        01
#>  9: c45a57a0547971c9e6e0f10f3de9861c  1211                        04
#> 10: e03e795af41aca093831c1fe7cc8a095 16955                        07
#>     mode_of_separation_recode       episode_start         episode_end
#>                        <char>              <POSc>              <POSc>
#>  1:                         1 2001-07-20 01:05:55 2001-07-20 06:20:55
#>  2:                         1 2001-07-20 01:21:55 2001-07-20 06:20:55
#>  3:                         1 2012-03-27 13:47:51 2012-03-27 22:57:51
#>  4:                         1 2010-09-23 19:48:55 2010-09-25 01:03:55
#>  5:                         1 2019-11-11 07:56:24 2019-11-11 14:22:24
#>  6:                         1 2022-08-17 20:51:36 2022-08-18 00:51:36
#>  7:                         1 2015-10-31 19:44:48 2015-10-31 20:24:48
#>  8:                         5 2019-12-16 09:11:22 2019-12-19 03:02:22
#>  9:                         1 2019-12-19 04:02:22 2019-12-20 07:35:22
#> 10:                         1 2008-06-27 15:11:27 2008-06-27 19:51:27
#>     facility_identifier_recode facility_trans_from_recode
#>                         <char>                     <char>
#>  1:                          A                       <NA>
#>  2:                          B                       <NA>
#>  3:                          A                       <NA>
#>  4:                          A                       <NA>
#>  5:                          A                       <NA>
#>  6:                          A                       <NA>
#>  7:                          A                       <NA>
#>  8:                          A                       <NA>
#>  9:                          B                          A
#> 10:                          A                       <NA>
#>     facility_trans_to_recode journey
#>                       <char>   <int>
#>  1:                     <NA>       1
#>  2:                     <NA>       1
#>  3:                     <NA>       2
#>  4:                     <NA>       3
#>  5:                     <NA>       4
#>  6:                     <NA>       5
#>  7:                     <NA>       6
#>  8:                        B       7
#>  9:                     <NA>       7
#> 10:                     <NA>       8
```

## Using `dbplyr` to work with large datasets

For large datasets, with many billions of pairs to check, a researcher
may reasonably be concerned about system load while doing this with base
R. Fortunately, the default predicates have been carefully constructed
to interface with `dbplyr`, allowing the work to be done through a
database management system with better resource management. For example,
using the `duckdb` package, an in-process OLAP system:

``` r
library(duckdb)
#> Loading required package: DBI
library(dbplyr)
#> 
#> Attaching package: 'dbplyr'
#> The following objects are masked from 'package:dplyr':
#> 
#>     ident, sql, sql_escape_ident, sql_escape_string

con <- dbConnect(duckdb(shared_home = FALSE), timezone_out = '', tz_out_convert = 'force')

# Tell DuckDB to read the data frame from R; DuckDB can also read Parquet files
duckdb_register(con, 'toy_apdc', toy_apdc)
toydata <- tbl(con, 'toy_apdc')

pairs <- inner_join(toydata, toydata, by = 'PPN', suffix = c('_parent', '_child')) |>
  filter(RL_ID_parent != RL_ID_child)

links <- pairs |>
  find_links(!!!default_predicates$NSW$APDC_APDC) |>
  select(RL_ID_parent, RL_ID_child) |>
  collect() # Tell dbplyr to run the query

toyids <- toydata |>
  select(RL_ID) |>
  collect()
setDT(toyids)

collect_journeys(toyids, links, identifier = 'RL_ID')
head(toyids, 10L)
#>                                RL_ID journey
#>                               <char>   <int>
#>  1: b2cd17b85f89befacfdc89a194bc6b64       1
#>  2: 9a1426d5bc9a7232fdcf7fcc1ed3ce73       1
#>  3: 8f337886bb80377c4129fa4a708e163f       2
#>  4: 33915b935904e38e7299cd3d4e040682       3
#>  5: a0db5e74832b16a26b20052088e5c8bd       4
#>  6: 69cbaaabc925e7714c346b3718bd3856       5
#>  7: cf63e6bc809ff6240852c470a931151f       6
#>  8: 79e219a17878df547879fb8d2b53df1c       7
#>  9: c45a57a0547971c9e6e0f10f3de9861c       7
#> 10: e03e795af41aca093831c1fe7cc8a095       8

on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
```

This table now contains the record identifier and the POHC number. It
can be inserted back into or registered with the database to conduct
further analyses.
