# Compute patient facts per year of cohort follow up for each domain defined in the domain_tbl

Compute patient facts per year of cohort follow up for each domain
defined in the domain_tbl

## Usage

``` r
compute_pf_omop(cohort, pf_input_tbl, grouped_list, domain_tbl)
```

## Arguments

- cohort:

  the cohort of patients of interest for the study

- pf_input_tbl:

  a table with the relevant domain of interest filtered to the desired
  visit type of interest

- grouped_list:

  a vector of column names by which the input should be grouped

- domain_tbl:

  a table with definitions for each domain (i.e.
  patientfacts::pf_domain_file)

## Value

a dataframe with one column for each of the specified domains associated
with the visit type of interest summarizing the facts per year of follow
up for each patient in the cohort
