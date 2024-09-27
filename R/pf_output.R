
#' Clinical Facet per Patient -- Output Generation
#'
#' @param process_output - the summary dataframe output by the `pf_process` function.
#'
#'                     Note any intermediate table generated is not intended to be used with this function.
#' @param output_function - the name of the output function that should be used provided in the `parameter_summary` csv
#'                          file that is output to the provided results folder after running the `pf_process` function
#' @param output - which output variable you would like to use in the graphs. available options based on check
#'                 configuration are provided in the `parameter_summary` csv file
#' @param facet - the variables by which you would like to facet the graph. available and/or recommended options for
#'                faceting variables are provided in the `parameter_summary` csv file
#' @param time_span - the length of time that should be displayed for relevant over-time graphs. this time period should
#'                    either be the same as or a subset of the time period used in the `pf_process` function
#' @param date_breaks_str - only for single-site, exploratory, over time; a string that informs the program how the
#'                          time period should be divided (i.e. '1 year', '3 months', etc). Defaults to 1 year.
#' @param domain_filter for `pf_ms_anom_at` only, the single domain to which the graph should be filtered for displaying
#'                      AUC values
#' @param visit_filter for `pf_ms_anom_at` only, the single visit type to which the graph should be filtered for displaying
#'                     AUC values
#'
#' @return "raw" output that can be called within the R environment to generate the graph in the Viewer window
#'

pf_output <- function(process_output,
                      output_function,
                      output,
                      facet = NULL,
                      time_span = c('2012-01-01', '2023-01-01'),
                      date_breaks_str = '1 year',
                      domain_filter = 'conditions_all',
                      visit_filter = 'outpatient'){

  ## Run output functions
  if(output_function == 'pf_ms_anom_nt'){
    pf_output <- pf_ms_anom_nt(data_tbl = process_output,
                               #output = output,
                               facet = facet,
                               #kmeans_clusters = kmeans_clusters
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
                              facet = facet,
                              time_span = time_span)
  }else if(output_function == 'pf_ss_exp_at'){
    pf_output <- pf_ss_exp_at(data_tbl = process_output,
                              output = output,
                              facet = facet,
                              date_breaks_str = date_breaks_str)
  }else(cli::cli_abort('Please enter a valid output_function for this check type.'))

  return(pf_output)
}
