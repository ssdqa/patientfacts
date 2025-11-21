# **Multi-Site, Exploratory, Cross-Sectional**

**Multi-Site, Exploratory, Cross-Sectional**

## Usage

``` r
pf_ms_exp_cs(data_tbl, output, facet, large_n = FALSE, large_n_sites = NULL)
```

## Arguments

- data_tbl:

  output from `pf_process` function

- output:

  desired output - have 2 options:

  - `median_site_with0s`: specific site median, including patients with
    no evidence of patient fact (e.g., if domain = labs, includes in the
    median all patients with and without any labs)

  - `median_site_without0s`: specific site median, not including
    patients without evidence of patient fact (e.g., if domain = labs,
    only includes median for patients with evidence of a lab)

- facet:

  variables to facet (e.g., `domain`); vector of strings

- large_n:

  a boolean indicating whether the large N visualization, intended for a
  high volume of sites, should be used; defaults to FALSE

- large_n_sites:

  a vector of site names that can optionally generate a filtered
  visualization

## Value

a dot plot displaying the median facts per follow up for each domain and
site compared to the all-site median (star icon)
