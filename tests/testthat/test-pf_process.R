
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


## Generally checking that code runs
test_that('testing omop version', {

  cht <- data.frame('person_id' = c(1000, 1001),
                    'site' = c('a', 'b'),
                    'start_date' = c(as.Date('2007-01-01'),as.Date('2008-01-01')),
                    'end_date' = c(as.Date('2011-01-01'),as.Date('2009-01-01')))

  sess <- argos$new()

  set_argos_default(sess)

  config('results_name_tag', '')

  expect_error(pf_process(cohort = cht,
                          multi_or_single_site = 'single',
                          anomaly_or_exploratory = 'exploratory',
                          omop_or_pcornet = 'omop'))
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
