
#' Clinical Facet per Patient -- Output Generation
#'
#' @param process_output the summary dataframe output by the `pf_process` function.
#'
#'                       Note any patient-level results generated are not intended to be used with this function.
#' @param output_function - the name of the output function that should be executed, provided in the message output
#'                          to the console after `pf_process` has been executed
#' @param output the numerical variable in `process_output` that should be used to generate the graph
#' @param date_breaks_str for `pf_ss_exp_at` only, a string that informs the program how the
#'                        time period should be divided (i.e. '1 year', '3 months', etc). Defaults to 1 year.
#' @param domain_filter for `pf_ms_anom_at` only, the single domain to which the graph should be filtered for displaying
#'                      Euclidean distance values
#' @param visit_filter for `pf_ms_anom_at` only, the single visit type to which the graph should be filtered for displaying
#'                     Euclidean distance values
#'
#' @return a graph summarizing the data output by `pf_process`; see individual output functions for specific details
#'
#' @export
#'

pf_output <- function(process_output,
                      output_function,
                      output,
                      #facet = NULL,
                      date_breaks_str = '1 year',
                      domain_filter = 'conditions_all',
                      visit_filter = 'outpatient'){

  if('age_grp' %in% colnames(process_output)){facet <- 'age_grp'}else{facet <- NULL}

  ## Run output functions
  if(output_function == 'pf_ms_anom_nt'){
    pf_output <- pf_ms_anom_nt(data_tbl = process_output,
                               facet = facet,
                               visit_filter = visit_filter)
  }else if(output_function == 'pf_ss_anom_nt'){
    pf_output <- pf_ss_anom_nt(data_tbl = process_output,
                               output = output,
                               facet = facet)
  }else if(output_function == 'pf_ms_exp_nt'){
    pf_output <- pf_ms_exp_nt(data_tbl = process_output,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ss_exp_nt'){
    pf_output <- pf_ss_exp_nt(data_tbl = process_output,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ms_anom_at'){
    pf_output <- pf_ms_anom_at(process_output = process_output,
                               domain_filter = domain_filter,
                               visit_filter = visit_filter)
  }else if(output_function == 'pf_ss_anom_at'){
    pf_output <- pf_ss_anom_at(data_tbl = process_output,
                               #output = output,
                               facet = facet,
                               visit_filter = visit_filter,
                               domain_filter = domain_filter)
  }else if(output_function == 'pf_ms_exp_at'){
    pf_output <- pf_ms_exp_at(data_tbl = process_output,
                              output = output,
                              facet = facet)
  }else if(output_function == 'pf_ss_exp_at'){
    pf_output <- pf_ss_exp_at(data_tbl = process_output,
                              output = output,
                              facet = facet,
                              date_breaks_str = date_breaks_str)
  }else(cli::cli_abort('Please enter a valid output_function for this check type.'))

  return(pf_output)
}
