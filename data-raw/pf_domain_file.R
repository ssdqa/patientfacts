## code to prepare `pf_domain_file` dataset goes here

pf_domain_file <- tidyr::tibble('domain' = c('diagnoses', 'prescription drugs'),
                                'domain_tbl' = c('condition_occurrence', 'drug_exposure'),
                                'filter_logic' = c(NA, 'drug_type_concept_id == 38000177'))

usethis::use_data(pf_domain_file, overwrite = TRUE)
