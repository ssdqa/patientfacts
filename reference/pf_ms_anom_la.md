# **Multi-Site, Anomaly, Longitudinal**

**Multi-Site, Anomaly, Longitudinal**

## Usage

``` r
pf_ms_anom_la(
  process_output,
  domain_filter,
  visit_filter,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- process_output:

  output of `pf_process` function

- domain_filter:

  one of the user provided domains in the process_output table to be
  used to filter down the output

- visit_filter:

  one of the user provided visit types in the process_output table to be
  used to filter down the output

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

returns three graphs:

1.  line graph that shows the smoothed proportion of a domain across
    time computation with the Euclidean distance associated with each
    line

2.  line graph that shows the raw proportion of a domain across time
    computation with the Euclidean distance associated with each line

3.  a bar graph with the Euclidean distance value for each site, with
    the average proportion as the fill
