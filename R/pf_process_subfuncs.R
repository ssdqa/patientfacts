
#' function to take in domain and compute patient facts
#'
#' @param cohort the cohort for which to iterate
#' @param pf_input_tbl the tbl that should be iterated
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl the config CSV file
#'
#' @return the column `person_id`, for the specified visit type,
#' the number of facts for the person - each fact corresponds to the
#' fact in the CSV file; each column is a domain
#'
#' @importFrom rlang parse_expr
#' @importFrom tidyr pivot_wider
#' @importFrom purrr reduce
#'

compute_pf <- function(cohort,
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
      compute_new(indexes=list('person_id'))

    domain_results[[domain_name]] <- pf
  }

  domain_results_left_join <-
    reduce(.x=domain_results,
           .f=left_join)
}


#' function to take in domain and compute patient facts
#'
#' @param cohort the cohort for which to iterate
#' @param pf_input_tbl the tbl that should be iterated
#' so person facts are computed
#' @param grouped_list a vector to group input by. Defaults to `person_id`,
#' `start_date`, `end_date`, `fu`, `site`
#' @param domain_tbl a CSV file with configuration information about each
#'                   domain of interest
#'
#' @return the column `person_id`, for the specified visit type,
#' the number of facts for the person - each fact corresponds to the
#' fact in the CSV file; each column is a domain
#'

compute_pf_for_fot <- function(cohort, pf_input_tbl,
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

    new_group <- grouped_list[! grouped_list %in% c('person_id')]

    pf_cohort_final <-
      pf %>% right_join(select(cohort,
                               person_id)) %>%
      distinct(person_id) %>% summarise(ct=n()) %>% pull()

    site_visit_ct_num <-
      pf_input_tbl %>% summarise(ct=n_distinct(person_id)) %>%
      pull()

    if (!class(config("db_src")) %in% "PqConnection") {
      pf_final <-
        pf %>% group_by(
          !!! syms(new_group)
        ) %>% group_by(domain, .add = TRUE) %>%
        mutate(fact_ct_denom=n(),
               sum_fact_ct=sum(total_strat_ct),
               median_fact_ct=median(total_strat_ct)) %>%
        select(group_cols(), fact_ct_denom, sum_fact_ct, median_fact_ct) %>%
        distinct() %>%
        #relocate(site) %>%
        ungroup()
    }else{
      pf_final <-
        pf %>% group_by(
          !!! syms(new_group)
        ) %>% group_by(domain, .add = TRUE) %>%
        summarise(fact_ct_denom=n(),
                  sum_fact_ct=sum(total_strat_ct),
                  median_fact_ct=median(total_strat_ct)) %>%
        #select(group_cols(), fact_ct_denom, sum_fact_ct, median_fact_ct) %>%
        #distinct() %>%
        #relocate(site) %>%
        ungroup()
    }


    finalized <-
      pf_final %>%
      mutate(pt_ct_denom=pf_cohort_final,
             site_visit_ct=site_visit_ct_num) %>% collect()


    domain_results[[domain_name]] <- finalized
  }


  reduce(.x=domain_results,
         .f=dplyr::union)
  # domain_results_left_join <-
  #   reduce(.x=domain_results,
  #          .f=left_join)
}


#' combine PF output into one table per study
#'
#' @param pf_tbl the table output by `compute_pf` or `compute_pf_for_fot`
#' @param study_abbr string that corresponds with the study/studies of interest
#' @param visit_type_list the types of visits in the PF output; defaults to:
#'                        inpatient, outpatient, other_visit, all
#' @param domain_list a CSV file with configuration information about each
#'                    domain of interest
#' @param time a logical indicating whether the analysis was conducted over time
#'
#' @return a list of dataframes that contain all sites and all domains for each study
#'         listed in `study_abbr`. the resulting dataframes will also have an
#'         age grouping column added.
#'
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#'
combine_study_facts <- function(pf_tbl,
                                study_abbr,
                                domain_list,
                                time = FALSE,
                                visit_type_list = list('inpatient','outpatient',
                                                       'other_visit','all')) {
  final_list <- list()

  pf_visits <-
    str_remove(names(pf_tbl),'(pf_)')

  names(pf_tbl) <- str_remove(names(pf_tbl), '(pf_)')

  for(i in 1:length(visit_type_list)) {

    possible_cols <-  domain_list %>%
      select(domain) %>% c()

    visit_type_pulled <-
      paste0(visit_type_list[[i]])

    pf_tbl_visittype <-
      pf_tbl[[visit_type_pulled]]

    # visit_type_pulled <-
    #   get_results(paste0(visit_type_list[[i]]))  %>%
    #   select(-any_of('fact_ct_strat'))

    tbl_cols <- pf_tbl_visittype %>% colnames()

    selected_cols <-
      intersect(possible_cols[[1]],
                tbl_cols)

    if(length(selected_cols) == 0){

      mutated_tbl <- NULL

      }else{

        if(!time){
        mutated_tbl <-
          pf_tbl_visittype %>%
          pivot_longer(cols=all_of(selected_cols),
                       names_to='domain',
                       #names_to='var_name',
                       values_to='var_val') %>%
          mutate(var_ever=case_when(!is.na(var_val)~1L,
                                    TRUE~0L)) %>%
          mutate(var_val=case_when(is.na(var_val) ~ 0,
                                    TRUE ~ var_val)) %>%
           mutate(study=study_abbr,
                  visit_type=visit_type_pulled)
        } else {mutated_tbl <- pf_tbl_visittype %>% mutate(study=study_abbr,
                                                     visit_type=visit_type_pulled)}

      }


    final_list[[i]] <- mutated_tbl

  }

  final_list_reduce <- reduce(.x=final_list,
                              .f=dplyr::union)
  final_list_reduce

}

#' Compute median fact counts by visit & fact type and by site, visit, & fact type
#'
#' @param data_input the list output of `combine_study_facts`
#' @param site_col the column in the data that holds site names
#' @param agegrp boolean that determines whether the output should also
#'               be grouped by age group
#' @param codeset boolean that determines whether the output should also
#'               be grouped by a user provided codeset flag
#'
#' @return dataframe that contains the total median number of facts for that visit & fact_type
#'         as well as the median number of facts for the site, visit, & fact type for each
#'         study
#'
#' @importFrom stats median
#'
compute_pf_medians <- function(data_input,
                               site_col,
                               agegrp=NULL,
                               codeset=NULL) {


  data_input_cols <- data_input %>% colnames()

  if('cohort' %in% data_input_cols) {
    data_input_grp <-
      data_input %>% group_by(cohort)
  } else {data_input_grp <- data_input}

  if(is.data.frame(agegrp)) {data_input_grp <- data_input_grp %>% group_by(age_grp,.add=TRUE) %>%
    mutate(age_grp = ifelse(is.na(age_grp), 'None', age_grp))}
  # if(is.data.frame(codeset)) {data_input_grp <- data_input_grp %>% group_by(flag,.add=TRUE) %>%
  #   mutate(flag = ifelse(is.na(flag), 'None', flag))}

  site_distance_medians_tbl <-
    data_input_grp %>%
    group_by(study,
             visit_type,
             domain,
             .add=TRUE) %>%
    mutate(median_all_with0s=median(var_val),
           median_all_without0s=median(var_val[var_val!=0])) %>%
    ungroup()

  site_distance_final <-
    data_input_grp %>%
    left_join(site_distance_medians_tbl) %>%
    group_by(study,
             !!sym(site_col),
             visit_type,
             domain,
             median_all_with0s,
             median_all_without0s,
             .add=TRUE) %>%
    summarise(n_tot=n(),
              n_w_fact=sum(var_ever),
              median_site_with0s=median(var_val),
              median_site_without0s=median(var_val[var_val!=0])) %>% ungroup() #%>%
  #mutate(prop_all_w_fact=round(n_w_fact/n_tot,3))

  site_distance_final <-
    site_distance_final %>% replace(is.na(.), 0) %>%
    mutate(across(everything(), ~ replace(.x, is.nan(.x), 0)))


  site_distance_final

}


#' Compute distance from mean
#'
#' @param data_input input table generated by `loop_through_visits` and reduced with `combine_study_facts`
#' @param site_col the column in the data that holds site names
#' @param n_sd the number of standard deviations that should be used as a threshold to
#'             identify an outlier
#' @param agegrp a csv file with user designated age groupings, based on age at cohort entry
#' @param codeset a csv file with user designated cohort flags based on a user provided codeset
#'
#' @return a dataframe that summarises the number of patients with fact counts that fall +/- 3 SD away from the mean
#'         both at fact and site + fact levels
#'
#'         contains columns: person_id, start_date, end_date, fu, site, domain, var_val, var_ever, study,
#'                           visit_type, n_fact, zscore_fact, outlier_fact, prop_outlier_fact, n_site_fact,
#'                           zscore_site_fact, outlier_site_fact, prop_outlier_site_fact
#'
#' @importFrom stats sd
#'
compute_dist_mean_pf <- function(data_input,
                                 site_col,
                                 n_sd = 2,
                                 agegrp = NULL,
                                 codeset = NULL) {

  if(is.data.frame(agegrp)) {data_input <- data_input %>% group_by(age_grp,.add=TRUE) %>%
    mutate(age_grp = ifelse(is.na(age_grp), 'None', age_grp))}
  # if(is.data.frame(codeset)) {data_input <- data_input %>% group_by(flag,.add=TRUE) %>%
  #   mutate(flag = ifelse(is.na(flag), 'None', flag))}

  site_dist_means_tbl <-
    data_input %>%
    group_by(study,
             visit_type,
             domain,
             .add=TRUE) %>%
    mutate(n_fact=n(),
           mean_fact=mean(var_val),
           sd_fact=sd(var_val),
           zscore_fact = ((var_val - mean_fact) / sd_fact),
           abs_z = abs(zscore_fact),
           outlier = case_when(abs_z > n_sd ~ 1L,
                               TRUE ~ 0L),
           outlier_fact = sum(outlier),
           prop_outlier_fact = round(outlier_fact / n_fact, 3)) %>%
    ungroup() %>%
    select(-c(outlier, abs_z, mean_fact, sd_fact))


  site_dist_means_final <-
    data_input %>%
    left_join(site_dist_means_tbl) %>%
    group_by(study,
             !!sym(site_col),
             visit_type,
             domain,
             n_fact,
             outlier_fact,
             prop_outlier_fact,
             .add=TRUE) %>%
    summarise(n_site_fact=n(),
              mean_site_fact=mean(var_val),
              sd_site_fact=sd(var_val),
              zscore_site_fact = ((var_val - mean_site_fact) / sd_site_fact),
              abs_z = abs(zscore_site_fact),
              outlier = case_when(abs_z > n_sd ~ 1L,
                                  TRUE ~ 0L),
              outlier_site_fact = sum(outlier),
              prop_outlier_site_fact = round(outlier_site_fact / n_site_fact, 3)) %>%
    ungroup() %>%
    select(-c(outlier, abs_z, mean_site_fact, sd_site_fact, zscore_site_fact)) %>% distinct()

  site_dist_means_final <-
    site_dist_means_final %>% replace(is.na(.), 0)

  site_dist_means_final
}
