# Multi-Site Analysis for Independent Data Sources

The multi-site analyses included in this suite are intended to be
executed against data that are all stored in the same place. However,
there may be some instances where the data associated with each site is
stored in independent locations. This vignette outlines how the
multi-site analysis can be executed in these instances.

After following the instructions to reproduce the analysis, you will
also need to change the `output_function` column to tell the `pf_output`
function which check you executed. Reference the table below for the
labels that are associated with each check:

| Check Type                                     | output_function |
|:-----------------------------------------------|:----------------|
| Multi Site, Exploratory, Cross-Sectional       | pf_ms_exp_cs    |
| Multi Site, Exploratory, Longitudinal          | pf_ms_exp_la    |
| Multi Site, Anomaly Detection, Cross-Sectional | pf_ms_anom_cs   |
| Multi Site, Anomaly Detection, Longitudinal    | pf_ms_anom_la   |

## Multi-Site Exploratory Analysis

### Cross-Sectional

First, execute the **Single Site, Exploratory, Cross-Sectional**
analysis, configured appropriately for your study, against each data
source.

``` r
library(patientfacts)

my_table <- pf_process(cohort = my_cohort,
                       multi_or_single_site = 'single',
                       anomaly_or_exploratory = 'exploratory',
                       time = F,
                       patient_level_tbl = T / F,
                       ...)
```

The `median_all_*` columns will not reflect the overall median using
this method. This is typically used as a baseline comparison metric in
`pf_output`. If you do not mind the lack of an overall comparison metric
and would just like to compare site-level medians, all you need to do is
union each table together and adjust the `output_function` accordingly.

``` r
my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = 'pf_ms_exp_cs')
```

If you would like to have the overall median available, you will need to
output patient-level results in addition to the standard summary output
(`patient_level_tbl = T`). Once patient-level output is returned from
each data source, you will need to union these tables together and pass
them through the functions below.

``` r
my_combined_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = 'pf_ms_exp_cs')

my_final_results <- 
  compute_pf_medians(data_input = my_combined_results,
                     site_col = 'site',
                     agegrp = age_groups # (as provided in pf_process)
                    )
```

### Longitudinal

The Longitudinal analysis does not output any overall comparative
median, so for this check, just execute the **Single Site, Exploratory,
Longitudinal** check against each data source and union the results
together.

Outputting patient level results is not required, but this option is
still available for the longitudinal analysis if needed.

``` r
my_table <- pf_process(cohort = my_cohort,
                       multi_or_single_site = 'single',
                       anomaly_or_exploratory = 'exploratory',
                       time = T,
                       patient_level_tbl = T / F,
                       ...)

my_final_results <- my_table1 %>% dplyr::union(my_table2) ... %>%
  dplyr::union(my_table_n) %>%
  dplyr::mutate(output_function = 'pf_ms_exp_la')
```

## Multi-Site Anomaly Detection Analysis

### Cross-Sectional

Follow the instructions above for producing the
`Multi-Site, Exploratory, Cross-Sectional` output **with patient-level
results**. This is required for this anomaly detection process as it is
not possible to back-compute from the standard aggregated results.

Once a combined table of patient-level results from each data source has
been created, execute the code below to run the anomaly detection
analysis. The `p_value` can be selected by the user.

``` r
my_summary_table <- my_combo_results %>% 
  group_by(site, visit_type, domain) %>%
  summarise(tot_pt = n_distinct(person_id), 
            n_pt_fact = sum(var_ever)) %>%
  mutate(prop_pt_fact = n_pt_fact / tot_pt)

df_start <- compute_dist_anomalies(df_tbl = my_summary_table,
                                   grp_vars = c('domain', 'visit_type'),
                                   var_col = 'prop_pt_fact',
                                   denom_cols = c('domain', 'visit_type', 
                                                  'tot_pt'))

df_final <- detect_outliers(df_tbl = df_start,
                            tail_input = 'both',
                            p_input = p_value,
                            column_analysis = 'prop_pt_fact',
                            column_variable = c('domain', 'visit_type'))
```

### Longitudinal

For a longitudinal analysis, start by following the instructions for the
`Multi-Site, Exploratory, Longitudinal` analysis. Then, the
`ms_anom_euclidean` function, available through the `squba.gen` package,
should be executed against your results. Copy the code below, inputting
the data you generated.

``` r
df_final <- ms_anom_euclidean(fot_input_tbl = my_table %>% 
                                mutate(prop_pts_fact = pts_w_fact / pts_w_visit),
                              grp_vars = c('site', 'visit_type', 'domain'),
                              var_col = 'prop_pts_fact')
```
