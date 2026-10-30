# Compute skewness & Pearson's skew

Compute skewness & Pearson's skew

## Usage

``` r
compute_skew_metrics(patient_facts_output, site_col, val_col = "var_val")
```

## Arguments

- patient_facts_output:

  the patient-level output produced by pf_process

- site_col:

  the column in the data that holds site names

- val_col:

  the column with the value that should be used as the metric for
  computing skew

## Value

a summary table with the site, visit type, & domain and their associated
skew metrics for the full distribution and a trimmed (top & bottom 2.5%
removed) distribution
