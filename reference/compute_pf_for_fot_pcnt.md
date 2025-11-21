# Compute facts per patient for each time_period within a user-defined time span

Compute facts per patient for each time_period within a user-defined
time span

## Usage

``` r
compute_pf_for_fot_pcnt(cohort, pf_input_tbl, grouped_list, domain_tbl)
```

## Arguments

- cohort:

  the cohort of patients of interest for the study

- pf_input_tbl:

  a table with the relevant domain of interest filtered to the desired
  visit type and time period of interest

- grouped_list:

  a vector of column names by which the input should be grouped

- domain_tbl:

  a table with definitions for each domain (i.e.
  patientfacts::pf_domain_file)

## Value

a dataframe with one column for each of the specified domains associated
with the visit type of interest summarizing the facts for each time
period within the time span for each patient in the cohort
