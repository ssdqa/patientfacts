# Patient Facts Visit File Sample – OMOP version

A sample version of the file structure expected for the `visit_type_tbl`
parameter in the `pf_process` function where the selected CDM is OMOP.
The user should recreate this file and include their own domain
definitions. Only ONE of visit_concept_id OR visit_detail_concept_id
should be used.

## Usage

``` r
pf_visit_file_omop
```

## Format

### pf_visit_file_omop

A dataframe with 2 columns:

- visit\_(detail)\_concept_id:

  The visit_concept_id OR visit_detail_concept_id as it appears in the
  visit_occurrence table

- visit_type:

  A string to label the visit type of the visit_concept_id; this string
  is what should be referenced in the `visit_types` parameter
