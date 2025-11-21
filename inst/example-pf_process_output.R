
#' Source setup file
source(system.file('setup.R', package = 'patientfacts'))

#' Create in-memory RSQLite database using data in extdata directory
conn <- mk_testdb_omop()

#' Establish connection to database and generate internal configurations
initialize_dq_session(session_name = 'pf_process_test',
                      working_directory = my_directory,
                      db_conn = conn,
                      is_json = FALSE,
                      file_subdirectory = my_file_folder,
                      cdm_schema = NA)

## turn of SQL trace for example
config('db_trace', FALSE)

#' Build mock study cohort
cohort <- cdm_tbl('person') %>% dplyr::distinct(person_id) %>%
  dplyr::mutate(start_date = as.Date(-5000), # RSQLite does not store date objects,
                                      # hence the numerics
                end_date = as.Date(15000),
                site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

#' Execute `pf_process` function
#' This example will use the single site, exploratory, cross sectional
#' configuration
pf_process_example <- pf_process(cohort = cohort,
                                 study_name = 'example_study',
                                 multi_or_single_site = 'single',
                                 anomaly_or_exploratory = 'exploratory',
                                 visit_type_table =
                                   patientfacts::pf_visit_file_omop,
                                 omop_or_pcornet = 'omop',
                                 visit_types = c('all'),
                                 domain_tbl = patientfacts::pf_domain_file %>%
                                   dplyr::filter(domain == 'diagnoses')) %>%
  suppressMessages()

pf_process_example

#' Execute `pf_output` function
#' The output was edited for a better indication of what the visualization will
#' look like.
#' The 0s are a limitation of the small sample data set used for this example
pf_output_example <- pf_output(process_output = pf_process_example %>%
                                 dplyr::mutate(median_site_without0s = 4),
                                 ## tweak synthetic output for example
                               output = 'median_site_without0s')

pf_output_example

#' Easily convert the graph into an interactive ggiraph or plotly object with
#' `make_interactive_squba()`

make_interactive_squba(pf_output_example)
