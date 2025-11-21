# **Single Site, Anomaly, Longitudinal**

**Single Site, Anomaly, Longitudinal**

## Usage

``` r
pf_ss_anom_la(data_tbl, facet, visit_filter, domain_filter)
```

## Arguments

- data_tbl:

  output from `pf_process` function

- facet:

  variables to facet (e.g., `domain`); vector of strings

- visit_filter:

  the single visit type of focus for the output

- domain_filter:

  the single domain of focus for the output

## Value

if analysis was executed by year or greater, a P Prime control chart is
returned with outliers marked with orange dots

        if analysis was executed by month or smaller, an STL regression is
        conducted and outliers are marked with red dots. the graphs representing
        the data removed in the regression are also returned
