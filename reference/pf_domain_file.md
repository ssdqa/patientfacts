# Patient Facts Domain File Sample

A sample version of the file structure expected for the `domain_tbl`
parameter in the `pf_process` function. The user should recreate this
file and include their own domain definitions.

## Usage

``` r
pf_domain_file
```

## Format

### pf_domain_file

A dataframe with 3 columns:

- domain:

  An arbitrary string to label the domain of interest

- domain_tbl:

  The name of the CDM table associated with the domain of interest

- filter_logic:

  (optional) a string to be parsed as logic to filter the domain_tbl as
  needed to best represent the domain
