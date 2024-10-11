
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
compute_pf_pcnt <- function(cohort,
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

    # if(!is.na(domain_list[[i]][[3]]) && !is.na(domain_list[[i]][[4]])) {
    #
    #   filter_var <- domain_list[[i]][[3]]
    #   filter_vec <- strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]]
    #   domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #     filter(!! sym(filter_var) %in% c(filter_vec))
    #
    # } else if(!is.na(domain_list[[i]][[3]]) && is.na(domain_list[[i]][[4]])){
    #
    #   filter_var <- domain_list[[i]][[3]]
    #   samp <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% select(!!sym(filter_var)) %>%
    #     head(1) %>% collect()
    #   var_class <- unlist(lapply(samp, class))
    #
    #   if(var_class == 'character'){
    #     domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #       filter(!!sym(filter_var) != 'NI', !!sym(filter_var) != 'OT',
    #              !!sym(filter_var) != 'UN', !is.na(!!sym(filter_var)))
    #   }else{
    #     domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #       filter(! is.na(!!sym(filter_var)))
    #   }
    #
    # }else{domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}

    ## computes facts per patient by a named list of grouped variables
    ## assumes patid is part of named list
    pf <-
      pf_input_tbl %>%
      inner_join(select(domain_tbl_use,
                        encounterid)) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>%
      ungroup() %>%
      mutate(domain=domain_name) %>%
      mutate(k_mult = case_when(fu < 0.1 ~ 100,
                                fu >= 0.1 & fu < 1 ~ 10,
                                TRUE ~ 1),
             fact_ct_strat=ifelse(fu != 0,round(total_strat_ct/(fu * k_mult),2),0)) %>%
      select(-total_strat_ct) %>%
      select(patid,
             domain,
             fact_ct_strat) %>%
      pivot_wider(names_from=domain,
                  values_from=fact_ct_strat) %>%
      right_join(cohort) %>%
      relocate(patid) %>%
      compute_new()

    domain_results[[domain_name]] <- pf
  }

  domain_results_left_join <-
    reduce(.x=domain_results,
           .f=left_join)
}

#'
#' Compute facts per patient for each time_period within a user-defined
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
compute_pf_for_fot_pcnt <- function(cohort,
                                    pf_input_tbl,
                                    grouped_list,
                                    domain_tbl) {

  on.exit(gc())

  domain_results <- list()
  domain_list <- split(domain_tbl, seq(nrow(domain_tbl)))


  for (i in 1:length(domain_list)) {

    domain_name = domain_list[[i]][[1]]
    message(paste0('Starting domain ', domain_list[[i]][1]))

    ## checks to see if the table needs to be filtered in any way;
    ## allow for one filtering operation
    if(! is.na(domain_list[[i]]$filter_logic)) {
      domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl)) %>%
        filter(!! rlang::parse_expr(domain_list[[i]]$filter_logic))
    } else {domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]]$domain_tbl))}


    # if(!is.na(domain_list[[i]][[3]]) && !is.na(domain_list[[i]][[4]])) {
    #
    #   filter_var <- domain_list[[i]][[3]]
    #   filter_vec <- strsplit(domain_list[[i]][[4]],split=',',fixed = TRUE)[[1]]
    #   domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #     filter(!! sym(filter_var) %in% c(filter_vec))
    #
    # } else if(!is.na(domain_list[[i]][[3]]) && is.na(domain_list[[i]][[4]])){
    #
    #   filter_var <- domain_list[[i]][[3]]
    #   samp <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>% select(!!sym(filter_var)) %>%
    #     head(1) %>% collect()
    #   var_class <- unlist(lapply(samp, class))
    #
    #   if(var_class == 'character'){
    #     domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #       filter(!!sym(filter_var) != 'NI', !!sym(filter_var) != 'OT',
    #              !!sym(filter_var) != 'UN', !is.na(!!sym(filter_var)))
    #   }else{
    #     domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]])) %>%
    #       filter(! is.na(!!sym(filter_var)))
    #   }
    #
    # }else{domain_tbl_use <- cdm_tbl(paste0(domain_list[[i]][[2]]))}

    ## computes facts per patient by a named list of grouped variables
    ## assumes person_id is part of named list
    pf <-
      pf_input_tbl %>%
      inner_join(select(domain_tbl_use,
                        encounterid)) %>%
      group_by(
        !!! syms(grouped_list)
      ) %>% summarise(total_strat_ct=n()) %>%
      mutate(domain=domain_name) %>% ungroup()

    new_group <- grouped_list[! grouped_list %in% c('patid')]

    pf_cohort_final <-
      pf %>% right_join(select(cohort,
                               patid)) %>%
      distinct(patid) %>% summarise(ct=n()) %>% pull()

    site_visit_ct_num <-
      pf_input_tbl %>% summarise(ct=n_distinct(patid)) %>%
      pull()

    if (!class(config("db_src")) %in% "PqConnection") {
      pf_final <-
        pf %>% group_by(
          !!! syms(new_group)
        ) %>% group_by(domain, .add = TRUE) %>%
        mutate(pts_w_fact=n(),
               sum_fact_ct=sum(total_strat_ct),
               median_fact_ct=median(total_strat_ct)) %>%
        select(group_cols(), pts_w_fact, sum_fact_ct, median_fact_ct) %>%
        distinct() %>%
        ungroup()
    }else{
      pf_final <-
        pf %>% group_by(
          !!! syms(new_group)
        ) %>% group_by(domain, .add = TRUE) %>%
        summarise(pts_w_fact=n(),
                  sum_fact_ct=sum(total_strat_ct),
                  median_fact_ct=median(total_strat_ct)) %>%
        ungroup()
    }


    finalized <-
      pf_final %>%
      mutate(pt_ct_denom=pf_cohort_final,
             pts_w_visit=site_visit_ct_num) %>% collect()


    domain_results[[domain_name]] <- finalized
  }


  reduce(.x=domain_results,
         .f=dplyr::union)
}
