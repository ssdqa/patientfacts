
#' Clinical Facts per Patient
#'
#' This is a completeness check that will compute the number of facts per years of follow-up for each patient in
#' a cohort. The user will provide the domains (`domain_tbl`) and visit types (`visit_type_tbl`) of interest.
#' Sample versions of these inputs are included as data in the package and are accessible with `patientfacts::`.
#' Results can optionally be stratified by site, age group, visit type, and/or time.
#'
#' This version of the function is compatible with both the OMOP and the PCORnet CDMs based on the user's selection.
#'
#' @param cohort A dataframe with the cohort of patients for your study. Should include the columns:
#' - person_id
#' - start_date
#' - end_date
#' - site
#' @param study_name A custom string label with the name of your study
#' @param patient_level_tbl logical indicating whether an additional table with patient level results should be output;
#'                          if TRUE, the output of this function will be a list containing both the summary and patient level
#'                          output. Otherwise, this function will just output the summary dataframe
#' @param visit_types A list of visit types by which the output should be stratified. Options for visit types can be found or adjusted
#'                    in the provided `pf_visit_file_(omop/pcornet)` file. If you would not like to stratify your results by visit type, please
#'                    set the visit_types argument equal to `all`
#' @param omop_or_pcornet Option to run the function using the OMOP or PCORnet CDM as the default CDM
#' - `omop`: run the [pf_process_omop()] function against an OMOP CDM instance
#' - `pcornet`: run the [pf_process_pcornet()] function against a PCORnet CDM instance
#' @param multi_or_single_site Option to run the function on a single vs multiple sites
#' - `single`: run the function for a single site
#' - `multi`: run the function for multiple sites
#' @param time a logical that tells the function whether you would like to look at the output over time
#' @param time_span when `time = TRUE`, this argument defines the start and end dates for the time period of interest. should be
#'                  formatted as `c(start date, end date)` in `yyyy-mm-dd` date format.
#' @param time_period when `time = TRUE`, this argument defines the distance between dates within the specified time period. defaults
#'                    to `year`, but other time periods such as `month` or `week` are also acceptable
#' @param p_value an integer indicating the p value to be used as a threshold in the multi-site anomaly detection analysis
#' @param age_groups If you would like to stratify the results by age group, create a table or CSV file with the following
#'                   columns and include it as the `age_groups` function parameter:
#' - `min_age`: the minimum age for the group (i.e. 10)
#' - `max_age`: the maximum age for the group (i.e. 20)
#' - `group`: a string label for the group (i.e. "10-20", "Young Adult", etc.)
#'
#' If you would *not* like to stratify by age group, leave the argument as `NULL`
#'
#' @param anomaly_or_exploratory Option to conduct an `exploratory` or `anomaly` detection analysis.
#'                               Exploratory analyses give a high level summary of the data to examine the
#'                               fact representation within the cohort. Anomaly detection analyses are specialized to identify
#'                               outliers within the cohort.
#' @param domain_tbl a table that defines the domains where facts should be identified. defaults to the provided
#'                   `pf_domain_file` table, which contains the following fields:
#' - `domain`: a string label for the domain being examined (i.e. prescription drugs)
#' - `domain_tbl`: the CDM table where information for this domain can be found (i.e. drug_exposure)
#' - `filter_logic`: an optional string to be parsed as logic to filter the domain_tbl as needed to best represent the domain
#'
#' @param visit_type_table a table that defines available visit types that are called in `visit_types.` defaults to the provided
#'                           `pf_visit_file_(omop/pcornet)` file, which contains the following fields:
#' - `visit_concept_id` or `enc_type`: the visit_concept_id or enc_type that represents the visit type of interest (i.e. 9201 or IP)
#' - `visit_type`: the string label to describe the visit type; this label can be used multiple times
#'                                           within the file if multiple visit_concept_ids/enc_types represent the visit type
#'
#' @return a dataframe with summary results (i.e. medians) that can be used as the input for `pf_output` to generate graphical output
#' @return if `patient_level_tbl` is TRUE, an additional dataframe is returned in a list format with patient level output
#'
#' @import argos
#' @import ssdqa.gen
#' @import dplyr
#' @import cli
#' @importFrom stringr str_wrap
#' @importFrom rlang current_env
#'
#' @export
#'

pf_process<- function(cohort = cohort,
                      study_name,
                      patient_level_tbl = FALSE,
                      visit_types = c('outpatient','inpatient'),
                      omop_or_pcornet = 'omop',
                      multi_or_single_site = 'multi',
                      time = FALSE,
                      time_span = c('2014-01-01', '2023-01-01'),
                      time_period = 'year',
                      p_value = 0.9,
                      age_groups = NULL,
                      anomaly_or_exploratory='anomaly',
                      domain_tbl=patientfacts::pf_domain_file,
                      visit_type_table=patientfacts::pf_visit_file_omop){

  ## Check proper arguments
  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(!multi_or_single_site %in% c('single', 'multi')){cli::cli_abort('Invalid argument for {.code multi_or_single_site}: please enter either {.code multi} or {.code single}')}
  if(!anomaly_or_exploratory %in% c('anomaly', 'exploratory')){cli::cli_abort('Invalid argument for {.code anomaly_or_exploratory}: please enter either {.code anomaly} or {.code exploratory}')}

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'pf',
                                             as.list(environment())))

  if(tolower(omop_or_pcornet) == 'omop'){

    pf_rslt <- pf_process_omop(cohort = cohort,
                               study_name = study_name,
                               patient_level_tbl = patient_level_tbl,
                               visit_types = visit_types,
                               multi_or_single_site = multi_or_single_site,
                               time = time,
                               time_span = time_span,
                               time_period = time_period,
                               p_value = p_value,
                               age_groups = age_groups,
                               #codeset = NULL,
                               anomaly_or_exploratory=anomaly_or_exploratory,
                               domain_tbl=domain_tbl,
                               visit_type_table=visit_type_table)

  }else if(tolower(omop_or_pcornet) == 'pcornet'){

    pf_rslt <- pf_process_pcornet(cohort = cohort,
                                  study_name = study_name,
                                  patient_level_tbl = patient_level_tbl,
                                  visit_types = visit_types,
                                  multi_or_single_site = multi_or_single_site,
                                  time = time,
                                  time_span = time_span,
                                  time_period = time_period,
                                  p_value = p_value,
                                  age_groups = age_groups,
                                  #codeset = NULL,
                                  anomaly_or_exploratory=anomaly_or_exploratory,
                                  domain_tbl=domain_tbl,
                                  visit_type_table=visit_type_table)

  }else{cli::cli_abort('Invalid argument for {.code omop_or_pcornet}: this function is only compatible with {.code omop} or {.code pcornet}')}

  cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output_function in pf_output: ', output_type, '.')))

  return(pf_rslt)
}
