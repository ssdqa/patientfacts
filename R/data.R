
#' Patient Facts Domain File Sample
#'
#' A sample version of the file structure expected for the `domain_tbl`
#' parameter in the `pf_process` function. The user should recreate
#' this file and include their own domain definitions.
#'
#' @format ## pf_domain_file
#' A dataframe with 3 columns:
#' \describe{
#'   \item{domain}{An arbitrary string to label the domain of interest}
#'   \item{domain_tbl}{The name of the CDM table associated with the domain of interest}
#'   \item{filter_logic}{(optional) a string to be parsed as logic to filter the domain_tbl as needed to best represent the domain}
#' }
#'
"pf_domain_file"

#' Patient Facts Visit File Sample -- OMOP version
#'
#' A sample version of the file structure expected for the `visit_type_tbl`
#' parameter in the `pf_process` function where the selected CDM is OMOP.
#' The user should recreate this file and include their own domain definitions.
#' Only ONE of visit_concept_id OR visit_detail_concept_id should be used.
#'
#' @format ## pf_visit_file_omop
#' A dataframe with 2 columns:
#' \describe{
#'   \item{visit_(detail)_concept_id}{The visit_concept_id OR visit_detail_concept_id as it appears in the visit_occurrence table}
#'   \item{visit_type}{A string to label the visit type of the visit_concept_id; this string is what should be referenced in the `visit_types` parameter}
#' }
#'
"pf_visit_file_omop"

#' Patient Facts Visit File Sample -- PCORnet version
#'
#' A sample version of the file structure expected for the `visit_type_tbl`
#' parameter in the `pf_process` function where the selected CDM is PCORnet.
#' The user should recreate this file and include their own domain definitions.
#'
#' @format ## pf_visit_file_pcornet
#' A dataframe with 2 columns:
#' \describe{
#'   \item{enc_type}{The enc_type as it appears in the encounter table}
#'   \item{visit_type}{A string to label the visit type of the enc_type; this string is what should be referenced in the `visit_types` parameter}
#' }
#'
"pf_visit_file_pcornet"
