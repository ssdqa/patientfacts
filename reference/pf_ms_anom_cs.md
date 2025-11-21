# **Multi-Site, Anomaly Detection, Cross-Sectional**

**Multi-Site, Anomaly Detection, Cross-Sectional**

## Usage

``` r
pf_ms_anom_cs(
  data_tbl,
  facet = NULL,
  visit_filter = "inpatient",
  large_n = FALSE,
  large_n_sites = NULL
)
```

## Arguments

- data_tbl:

  output from `pf_process` function

- facet:

  variables to facet (e.g., `domain`); vector of strings

- visit_filter:

  the single visit_type of interest to be used in the analysis

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot where the shape of the dot represents whether the point is
anomalous, the color of the dot represents the proportion of patients
for a given domain, and the size of the dot represents the mean
proportion across all sites

        if no groups were eligible for anomaly detection analysis, a heatmap summarizing
        the proportion of patients for a given domain & site and a dot plot summarizing
        the average standard deviation for each site are returned as an alternative
