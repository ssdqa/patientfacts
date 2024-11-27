

output_cols <- c("study", "site", "visit_type", "domain", "median_all_with0s",
                 "median_all_without0s", "n_tot", "n_w_fact",
                 "median_site_with0s", "median_site_without0s",
                 "tot_pt", "n_pt_fact", "prop_pt_fact", "mean_val",
                 "median_val", "sd_val", "mad_val", "cov_val",
                 "max_val", "min_val", "range_val", "total_ct",
                 "analysis_eligible", "lower_tail", "upper_tail",
                 "anomaly_yn", "mean_fact", "sd_fact",
                 "outlier_fact", "prop_outlier_fact", "outlier_tot",
                 "mean_tot", "sd_tot", "prop_outlier_tot", "time_start",
                 "time_increment", "pts_w_fact", "sum_fact_ct",
                 "median_fact_ct", "pt_ct_denom", "pts_w_visit",
                 "prop_pts_fact", "mean_allsiteprop", "median",
                 "date_numeric", "site_loess", "dist_eucl_mean", "observed",
                 "season", "trend", "remainder", "seasadj", "anomaly",
                 "anomaly_direction", "anomaly_score", "recomposed_l1",
                 "recomposed_l2", "observed_clean")

utils::globalVariables(c('.', 'site', 'person_id', 'patid', 'visit_occurrence_id',
                       'encounterid', 'visit_type', 'age_grp', 'var_val', 'zscore_tot',
                       'zscore_fact', 'outlier', 'var_ever', 'abs_z', 'total_strat_ct',
                       'fu', 'k_mult', 'fact_ct_strat', 'x', 'y', 'lcl', 'ucl', 'cl',
                       'facet_col', output_cols))
