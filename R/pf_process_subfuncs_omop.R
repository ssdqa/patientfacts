
#'
#' Compute patient facts per year of cohort follow up for each domain
#' defined in the domain_tbl
#'
#' @param cohort the cohort of patients of interest for the study
#' @param pf_input_tbl a table with the relevant domain of interest
#' filtered to the desired visit type of interest
#' @param grouped_list a vector of column names by which the input should be grouped
#' @param domain_tbl a table with definitions for each domain (i.e. patientfacts::pf_domain_file)
#'
#' @return a dataframe with one column for each of the specified domains associated with
#'         the visit type of interest summarizing the facts per year of follow up for each
#'         patient in the cohort
#'
#' @importFrom rlang parse_expr
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#'
#' @keywords internal
#'
compute_pf_omop <- function(cohort,
                            pf_input_tbl,
                            grouped_list,
                            domain_tbl) {

  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))


  for (i in 1:length(domain_list)) {

    domain_name = domain_list[[i]]$domain
    message(paste0('Starting domain ', domain_list[[i]]$domain))

    ## checks to see if the table needs to be filtered in any way;
    ## allow for one filtering operation
    if(! is.na(domain_list[[i]]$filter_logic)) {
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl)) %>%
        filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl))}

    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <-
      pf_input_tbl %>%
      inner_join(select(domain_tbl_use,
                        visit_occurrence_id)) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>%
      ungroup() %>%
      mutate(domain=domain_name) %>%
      mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                fu >= 0.1 & fu < 1 ~ 10,
                                TRUE ~ 1),
             fact_ct_strat=ifelse(fu != 0,round(total_strat_ct/(fu * k_mult),2),0)) %>%
      #select(-c(total_strat_ct, k_mult)) %>%
      select(person_id,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>%
      right_join(cohort) %>%
      relocate(person_id) %>%
      compute_new()

    domain_results[[domain_name]] <- pf
  }

  domain_results_left_join <-
    reduce(.x=domain_results,
           .f=left_join)
}


#'
#' Compute facts per patient for each time period within a user-defined
#' time span
#'
#' @param cohort the cohort of patients of interest for the study
#' @param pf_input_tbl a table with the relevant domain of interest
#' filtered to the desired visit type and time period of interest
#' @param grouped_list a vector of column names by which the input should be grouped
#' @param domain_tbl a table with definitions for each domain (i.e. patientfacts::pf_domain_file)
#'
#' @return a dataframe with one column for each of the specified domains associated with
#'         the visit type of interest summarizing the facts for each time period within the time span
#'         for each patient in the cohort
#'
#' @keywords internal
#'
compute_pf_for_fot_omop <- function(cohort, pf_input_tbl,
                                    grouped_list,
                                    domain_tbl) {

  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))


  for (i in 1:length(domain_list)) {

    domain_name = domain_list[[i]]$domain
    message(paste0('Starting domain ', domain_list[[i]]$domain))

    ## checks to see if the table needs to be filtered in any way;
    ## allow for one filtering operation
    if(! is.na(domain_list[[i]]$filter_logic)) {
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl)) %>%
        filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl))}

    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <-
      pf_input_tbl %>%
      inner_join(select(domain_tbl_use,
                        visit_occurrence_id)) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>%
      mutate(domain=domain_name) %>% ungroup()

    # new_group <- grouped_list[! grouped_list %in% c('person_id')]

    pf_cohort_final <-
      pf %>% right_join(select(cohort,
                               person_id)) %>%
      distinct(person_id) %>% summarise(ct=n()) %>% pull()

    site_visit_ct_num <-
      pf_input_tbl %>% summarise(ct=n_distinct(person_id)) %>%
      pull()

    # if (!class(config("db_src")) %in% "PqConnection") {
    #   pf_final <-
    #     pf %>% group_by(
    #       !!! syms(new_group)
    #     ) %>% group_by(domain, .add = TRUE) %>%
    #     mutate(pts_w_fact=n(),
    #            sum_fact_ct=sum(total_strat_ct),
    #            median_fact_ct=median(total_strat_ct)) %>%
    #     select(group_cols(), pts_w_fact, sum_fact_ct, median_fact_ct) %>%
    #     distinct() %>%
    #     ungroup()
    # }else{
    #   pf_final <-
    #     pf %>% group_by(
    #       !!! syms(new_group)
    #     ) %>% group_by(domain, .add = TRUE) %>%
    #     summarise(pts_w_fact=n(),
    #               sum_fact_ct=sum(total_strat_ct),
    #               median_fact_ct=median(total_strat_ct)) %>%
    #     ungroup()
    # }

    finalized <-
      pf %>%
      mutate(pt_ct_denom=pf_cohort_final,
             pts_w_visit=site_visit_ct_num) %>% collect()


    domain_results[[domain_name]] <- finalized
  }


  reduce(.x=domain_results,
         .f=dplyr::union)
}
