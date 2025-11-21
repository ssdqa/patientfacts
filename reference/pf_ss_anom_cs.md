# **Single Site, Anomaly Detection, Cross-Sectional**

**Single Site, Anomaly Detection, Cross-Sectional**

## Usage

``` r
pf_ss_anom_cs(data_tbl, output, facet = c("visit_type"))
```

## Arguments

- data_tbl:

  output from `pf_process` function

- output:

  desired output - have 4 options:

  - `outlier_fact`: the number of facts overall (i.e. not grouped by
    site) that fall 2 SD away from the mean

  - `prop_outlier_fact`: the proportion of facts overall that fall 2 SD
    away from the mean

  - `outlier_site_fact`: the number of facts per site that fall 2 SD
    away from the mean

  - `prop_outlier_site_fact`: the proportion of facts per site that fall
    2 SD away from the mean

- facet:

  variables to facet (e.g., `domain`); vector of strings

## Value

a bar graph displaying the output value of interest, which represents
patients falling +/- 2 standard deviations away from the mean facts per
follow-up for a given domain
