# Patient Facts Visit File Sample – PCORnet version

A sample version of the file structure expected for the `visit_type_tbl`
parameter in the `pf_process` function where the selected CDM is
PCORnet. The user should recreate this file and include their own domain
definitions.

## Usage

``` r
pf_visit_file_pcornet
```

## Format

### pf_visit_file_pcornet

A dataframe with 2 columns:

- enc_type:

  The enc_type as it appears in the encounter table

- visit_type:

  A string to label the visit type of the enc_type; this string is what
  should be referenced in the `visit_types` parameter
