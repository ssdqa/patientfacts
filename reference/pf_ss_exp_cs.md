# **Single-Site, Exploratory, Cross-Sectional**

**Single-Site, Exploratory, Cross-Sectional**

## Usage

``` r
pf_ss_exp_cs(data_tbl, output, facet)
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

## Value

a bar graph displaying the median facts per follow-up for each domain
and visit_type
