
#' Clinical Facts per Patient -- OMOP
#'
#' This is a completeness check that will compute the number of facts per years of follow-up for each patient in
#' a cohort. The user will provide the domains (`domain_tbl`) and visit types (`visit_type_tbl`) of interest.
#' Sample versions of these inputs are included as data in the package and are accessible with `patientfacts::`.
#' Results can optionally be stratified by site, age group, visit type, and/or time.
#'
#' This version of the function is compatible with the OMOP CDM.
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
#'

pf_process_omop <- function(cohort = cohort,
                            study_name = 'glom',
                            patient_level_tbl = FALSE,
                            visit_types = c('outpatient','inpatient'),
                            multi_or_single_site = 'multi',
                            time = FALSE,
                            time_span = c('2014-01-01', '2023-01-01'),
                            time_period = 'year',
                            p_value = 0.9,
                            age_groups = NULL,
                            #codeset = NULL,
                            anomaly_or_exploratory='anomaly',
                            domain_tbl=patientfacts::pf_domain_file,
                            visit_type_table=read_codeset('pf_visit_types','ic')){

  ## Step 0: Set cohort name for table output
  config('cohort', study_name)

  ## parameter summary output
  output_type <- suppressWarnings(param_summ(check_string = 'pf',
                                             as.list(environment())))

  ## Step 1: Check Sites
  site_filter <- check_site_type(cohort = cohort,
                                 multi_or_single_site = multi_or_single_site)
  cohort_filter <- site_filter$cohort
  grouped_list <- site_filter$grouped_list
  site_col <- site_filter$grouped_list
  site_list_adj <- site_filter$site_list_adj

  ## Step 2: Prep cohort

  cohort_prep <- prepare_cohort(cohort_tbl = cohort_filter, age_groups = age_groups)

  ## Step 3: Run Function

  if(!time){grouped_list <- grouped_list %>% append(c('person_id','start_date','end_date','fu'))}else{
    grouped_list <- grouped_list %>% append(c('person_id','time_start','time_increment','fu'))}

  if(is.data.frame(age_groups)){grouped_list <- grouped_list %>% append('age_grp')}
  #if(is.data.frame(codeset)){grouped_list <- grouped_list %>% append('flag')}

  if(time){

    grouped_list <- grouped_list[! grouped_list %in% 'fu']

    pf_int <- compute_fot(cohort = cohort_prep,
                            site_col = site_col,
                            reduce_id = 'visit_type',
                            time_period = time_period,
                            time_span = time_span,
                            site_list = site_list_adj,
                            check_func = function(dat){
                              loop_through_visits(cohort_tbl = dat,
                                                  check_func = function(cht, t){
                                                    compute_pf_for_fot_omop(cohort = cht,
                                                                            pf_input_tbl = t,
                                                                            grouped_list = grouped_list,
                                                                            domain_tbl = domain_tbl)},
                                                  site_col = site_col,
                                                  time = TRUE,
                                                  visit_type_tbl=visit_type_table,
                                                  site_list=site_list_adj,
                                                  visit_list=visit_types,
                                                  grouped_list=grouped_list,
                                                  domain_tbl=domain_tbl)
                            })

  } else {
    pf_tbl <- loop_through_visits(
      cohort_tbl=cohort_prep,
      check_func = function(cht, t){
        compute_pf_omop(cohort = cht,
                        pf_input_tbl = t,
                        grouped_list = grouped_list,
                        domain_tbl = domain_tbl)},
      site_col = site_col,
      time = FALSE,
      site_list=site_list_adj,
      visit_list=visit_types,
      visit_type_tbl=visit_type_table,
      grouped_list=grouped_list,
      domain_tbl = domain_tbl
    )

    pf_int <- combine_study_facts(pf_tbl=pf_tbl,
                                  domain_list = domain_tbl,
                                  study_abbr = study_name,
                                  time = time,
                                  visit_type_list = visit_types) %>% collect()
    }

  ## Step 4: Summarise (Medians, SD)
  if(!time) {
    if(anomaly_or_exploratory=='anomaly' && multi_or_single_site=='single') {
      pf_final <- compute_dist_mean_pf(pf_int,
                                       n_sd = 2,
                                       site_col = site_col,
                                       agegrp= age_groups
                                       #codeset = codeset
                                       )
    } else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){

      pf_int_summ <- pf_int %>% group_by(site, visit_type, domain) %>%
        summarise(tot_pt = n_distinct(person_id), n_pt_fact = sum(var_ever)) %>%
        mutate(prop_pt_fact = n_pt_fact / tot_pt)

      pf_anom_int <- compute_dist_anomalies(df_tbl = pf_int_summ %>% replace_site_col(),
                                            grp_vars = c('domain', 'visit_type'),
                                            var_col = 'prop_pt_fact',
                                            denom_cols = c('domain', 'visit_type', 'tot_pt'))

      pf_final <- detect_outliers(df_tbl = pf_anom_int,
                                  tail_input = 'both',
                                  p_input = p_value,
                                  column_analysis = 'prop_pt_fact',
                                  column_variable = c('domain', 'visit_type'))

    } else {pf_final <- compute_pf_medians(data_input=pf_int,
                                            site_col = site_col,
                                            agegrp = age_groups
                                            #codeset=codeset
                                           )}

  }else{

    if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'multi'){

      pf_final <- ms_anom_euclidean(fot_input_tbl = pf_int %>% mutate(prop_pts_fact = fact_ct_denom / site_visit_ct),
                                    grp_vars = c('site', 'visit_type', 'domain'),
                                    var_col = 'prop_pts_fact')

    }else if(anomaly_or_exploratory == 'anomaly' && multi_or_single_site == 'single'){

      pf_final <- anomalize_ss_anom_at(fot_input_tbl = pf_int %>% mutate(prop_pts_fact = fact_ct_denom / site_visit_ct),
                                       time_var = 'time_start',
                                       grp_vars = c('domain', 'visit_type'),
                                       var_col = 'prop_pts_fact')

      }else{pf_final <- pf_int}

    }

  cli::cli_inform(str_wrap(paste0('Based on your chosen parameters, we recommend using the following
                       output_function in pf_output: ', output_type, '.')))

  # Output results
  if(patient_level_tbl){

    output <- list('pf_summary_results' = pf_final %>% replace_site_col(),
                   'pf_patient_level_results' = pf_int %>% replace_site_col())

    return(output)

  }else{return(pf_final %>% replace_site_col())}

}
