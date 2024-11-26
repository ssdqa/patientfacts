
test_that('errors on incorrect output_function', {

  tbl_test <- data.frame('test'= c(1, 2, 3))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_test'))
})


test_that('single site, exploratory, no time', {

  tbl_test <- tidyr::tibble('study' = 'test',
                            'site' = 'a',
                            'visit_type' = 'inpatient',
                            'domain' = 'diagnoses',
                            'median_all_with0s' = 0,
                            'median_all_without0s' = 5.1,
                            'n_tot' = 100,
                            'n_w_fact' = 82,
                            'median_site_with0s' = 0,
                            'median_site_without0s' = 5.1)

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ss_exp_cs',
                            output = 'median_site_without0s'))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_ss_exp_cs',
                         output = 'test'))

})


test_that('multi site, exploratory, no time', {

  tbl_test <- tidyr::tibble('study' = c('test', 'test', 'test'),
                            'site' = c('a', 'b', 'c'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses'),
                            'median_all_with0s' = c(0,0,0),
                            'median_all_without0s' = c(5.1, 7.9, 4.2),
                            'n_tot' = c(100, 500, 250),
                            'n_w_fact' = c(82, 473, 198),
                            'median_site_with0s' = c(0,0,0),
                            'median_site_without0s' = c(4.7, 9.9, 3.3))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ms_exp_cs',
                            output = 'median_site_without0s'))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_ms_exp_cs',
                         output = 'test'))

})

test_that('multi site, anomaly, no time', {


  tbl_test <- tidyr::tibble('site' = c('a', 'b', 'c'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient'),
                            'tot_pt' = c(100, 500, 250),
                            'n_pt_fact' = c(82, 473, 198),
                            'prop_pt_fact' = c(0.82, 0.95, 0.79),
                            'mean_val' = c(0.85, 0.85, 0.85),
                            'median_val' = c(0.82, 0.82, 0.82),
                            'sd_val' = c(0.05, 0.05, 0.05),
                            'mad_val' = c(0.02, 0.02, 0.02),
                            'cov_val' = c(0.01, 0.01, 0.01),
                            'max_val' = c(0.95, 0.95, 0.95),
                            'min_val' = c(0.79, 0.79, 0.79),
                            'range_val' = c(0.16, 0.16, 0.16),
                            'total_ct' = c(3,3,3),
                            'analysis_eligible' = c('yes','yes','yes'),
                            'lower_tail' = c(0.8134, 0.8134, 0.8134),
                            'upper_tail' = c(0.932, 0.932, 0.932),
                            'anomaly_yn' = c('no outlier', 'outlier', 'outlier'))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ms_anom_cs',
                            visit_filter = 'inpatient'))

  expect_no_error(pf_output(process_output = tbl_test %>% dplyr::mutate(anomaly_yn = 'no outlier in group'),
                            output_function = 'pf_ms_anom_cs',
                            visit_filter = 'inpatient'))


})


test_that('single site, anomaly, no time', {

  tbl_test <- tidyr::tibble('study' = 'test',
                            'site' = 'a',
                            'visit_type' = 'inpatient',
                            'domain' = 'diagnoses',
                            'prop_outlier_fact' = 0.04)

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ss_anom_cs',
                            output = 'prop_outlier_fact'))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_ss_anom_cs',
                         output = 'test'))

})


test_that('single site, exploratory, time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'time_increment' = c('year', 'year', 'year', 'year', 'year'),
                            'median_fact_ct' = c(5, 6, 7, 8, 9))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ss_exp_la',
                            output = 'median_fact_ct'))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_ss_exp_la',
                         output = 'test'))

})


test_that('multi site, exploratory, time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses',
                                         'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient',
                                             'inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01',
                                             '2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'time_increment' = c('year', 'year', 'year', 'year', 'year',
                                                 'year', 'year', 'year', 'year', 'year'),
                            'median_fact_ct' = c(5, 6, 7, 8, 9, 2, 4, 6, 8, 10))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ms_exp_la',
                            output = 'median_fact_ct'))

  expect_error(pf_output(process_output = tbl_test,
                         output_function = 'pf_ms_exp_la',
                         output = 'test'))

})


test_that('multi site, anomaly, time', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01',
                                             '2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient',
                                             'inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses',
                                         'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'prop_pts_fact' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94, 0.94),
                            'mean_allsiteprop' = c(0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83, 0.83),
                            'median' = c(0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87, 0.87),
                            'date_numeric' = c(17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000, 17000),
                            'site_loess' = c(0.84, 0.87, 0.89, 0.91, 0.89, 0.73, 0.81, 0.83, 0.94, 0.94),
                            'dist_eucl_mean' = c(0.84,0.84,0.84,0.84,0.84,0.9,0.9,0.9,0.9,0.9))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ms_anom_la',
                            visit_filter = 'inpatient',
                            domain_filter = 'diagnoses'))

})


test_that('single site, anomaly, time - year', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'time_increment' = c('year', 'year', 'year', 'year', 'year'),
                            'prop_pts_fact' = c(0.5, 0.6, 0.7, 0.8, 0.9),
                            'pts_w_fact' = c(5, 6, 7, 8, 9),
                            'pts_w_visit' = c(10,11,12,13,14))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ss_anom_la',
                            visit_filter = 'inpatient',
                            domain_filter = 'diagnoses'))


})


test_that('single site, anomaly, time - month', {

  tbl_test <- tidyr::tibble('site' = c('a', 'a', 'a', 'a', 'a'),
                            'domain' = c('diagnoses', 'diagnoses', 'diagnoses', 'diagnoses', 'diagnoses'),
                            'visit_type' = c('inpatient', 'inpatient', 'inpatient', 'inpatient', 'inpatient'),
                            'time_start' = c('2018-01-01', '2019-01-01', '2020-01-01', '2021-01-01', '2022-01-01'),
                            'time_increment' = c('month', 'month', 'month', 'month', 'month'),
                            'prop_pts_fact' = c(0.5, 0.6, 0.7, 0.8, 0.9),
                            'pts_w_fact' = c(5, 6, 7, 8, 9),
                            'pts_w_visit' = c(10,11,12,13,14),
                            'observed' = c(0.5, 0.6, 0.7, 0.8, 0.9),
                            'season' = c(1,2,3,4,5),
                            'trend' = c(1,2,3,4,5),
                            'remainder' = c(0.46, 0.57, 0.69, 0.82, 0.88),
                            'seasonadj' = c(1,2,3,4,5),
                            'anomaly' = c('Yes', 'No', 'No', 'No', 'Yes'),
                            'anomaly_direction' = c(-1,0,0,0,1),
                            'anomaly_score' = c(1,2,3,4,5),
                            'recomposed_l1' = c(0.44, 0.6, 0.5, 0.49, 0.46),
                            'recomposed_l2' = c(0.84, 0.8, 0.8, 0.89, 0.86),
                            'observed_clean' = c(0.46, 0.57, 0.69, 0.82, 0.88))

  expect_no_error(pf_output(process_output = tbl_test,
                            output_function = 'pf_ss_anom_la',
                            visit_filter = 'inpatient',
                            domain_filter = 'diagnoses'))


})
