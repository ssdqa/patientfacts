# Combine the output of compute_pf or compute_pf_for_fot into one dataframe

Combine the output of compute_pf or compute_pf_for_fot into one
dataframe

## Usage

``` r
combine_study_facts(pf_tbl, study_abbr, domain_list, time, visit_type_list)
```

## Arguments

- pf_tbl:

  the table output by `compute_pf` or `compute_pf_for_fot`

- study_abbr:

  string that corresponds with the study/studies of interest

- domain_list:

  a table with definitions for each domain (i.e.
  patientfacts::pf_domain_file)

- time:

  a logical indicating whether the analysis was conducted over time

- visit_type_list:

  the types of visits in the PF output; defaults to: inpatient,
  outpatient, other_visit, all

## Value

a dataframe containing information associated with all sites, domains,
and visit types for the study listed in `study_abbr`
