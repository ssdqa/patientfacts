
## Testing error functionality
test_that('only single & multi are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(pf_process(cohort = cht,
                            multi_or_single_site = 'test',
                            anomaly_or_exploratory = 'exploratory',
                            omop_or_pcornet = 'omop'))
})


test_that('only anomaly & exploratory are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(pf_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'test',
                          omop_or_pcornet = 'omop'))
})

test_that('only omop & pcornet are allowed inputs', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  expect_error(pf_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'test'))
})

## Test process function

test_that('pf ss/ms exp nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'pf_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(pf_process(cohort = cohort,
                             study_name = 'test',
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory = 'exploratory',
                             omop_or_pcornet = 'omop',
                             domain_tbl = patientfacts::pf_domain_file %>%
                               filter(domain == 'diagnoses')))
})

test_that('pf ss anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'pf_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(pf_process(cohort = cohort,
                             study_name = 'test',
                             multi_or_single_site = 'single',
                             anomaly_or_exploratory = 'anomaly',
                             omop_or_pcornet = 'omop',
                             domain_tbl = patientfacts::pf_domain_file %>%
                               filter(domain == 'diagnoses')))
})

test_that('pf ms anom nt -- omop', {

  rlang::is_installed("DBI")
  rlang::is_installed("readr")
  rlang::is_installed('RSQLite')

  conn <- mk_testdb_omop()

  initialize_dq_session(session_name = 'pf_process_test',
                        working_directory = getwd(),
                        db_conn = conn,
                        is_json = FALSE,
                        file_subdirectory = 'testdata',
                        cdm_schema = NA)

  cohort <- cdm_tbl('person') %>% distinct(person_id) %>%
    mutate(start_date = as.Date(-5000),
           end_date = as.Date(15000),
           site = ifelse(person_id %in% c(1:6), 'synth1', 'synth2'))

  expect_no_error(pf_process(cohort = cohort,
                             study_name = 'test',
                             multi_or_single_site = 'multi',
                             anomaly_or_exploratory = 'anomaly',
                             omop_or_pcornet = 'omop',
                             domain_tbl = patientfacts::pf_domain_file %>%
                               filter(domain == 'diagnoses')))
})

test_that('testing pcornet version', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c('2007-01-01','2008-01-01'),
                    'end_date' = c('2011-01-01','2009-01-01'))

  sess <- argos$new()

  set_argos_default(sess)

  config('results_name_tag', '')

  expect_error(pf_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'pcornet'))
})


