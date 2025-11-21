# **Multi-Site, Exploratory, Longitudinal**

**Multi-Site, Exploratory, Longitudinal**

## Usage

``` r
pf_ms_exp_la(
  data_tbl,
  output,
  facet = NULL,
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- data_tbl:

  output from `pf_process` function

- output:

  desired output - have 2 options:

  - `median_fact_ct`: the median number of facts for each domain during
    the specified time period

  - `sum_fact_ct`: the sum of facts for each domain during the specified
    time period

- facet:

  variables to facet (e.g., `domain`); vector of strings

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

line graph representing the output variable of interest across time for
each of the sites of interest; each site is represented by one line
