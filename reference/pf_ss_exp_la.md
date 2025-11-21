# **Single Site, Exploratory, Longitudinal**

**Single Site, Exploratory, Longitudinal**

## Usage

``` r
pf_ss_exp_la(data_tbl, output, facet, date_breaks_str = "1 year")
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

- date_breaks_str:

  string to denote how time should be broken up in the chart

## Value

a dot and line chart displaying the `output` variable of interest per
domain across the user specified time period
