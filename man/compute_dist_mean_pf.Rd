% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pf_process_subfuncs.R
\name{compute_dist_mean_pf}
\alias{compute_dist_mean_pf}
\title{Compute distance from mean to identify patients with an anomalous number of facts per year
of follow up}
\usage{
compute_dist_mean_pf(data_input, site_col, n_sd = 2, agegrp = NULL)
}
\arguments{
\item{data_input}{input table generated by \code{loop_through_visits} and reduced with \code{combine_study_facts}}

\item{site_col}{the column in the data that holds site names}

\item{n_sd}{the number of standard deviations that should be used as a threshold to
identify an outlier}

\item{agegrp}{If you would like to stratify the results by age group, create a table or CSV file with the following
columns and include it as the \code{agegrp} function parameter:
\itemize{
\item \code{min_age}: the minimum age for the group (i.e. 10)
\item \code{max_age}: the maximum age for the group (i.e. 20)
\item \code{group}: a string label for the group (i.e. "10-20", "Young Adult", etc.)
}}
}
\value{
a dataframe that summarises the number of patients with fact counts that fall +/- \code{n_sd} away from
the mean both at visit_type + domain and site, visit_type, + domain levels

\if{html}{\out{<div class="sourceCode">}}\preformatted{    contains columns: person_id, start_date, end_date, fu, site, domain, var_val, var_ever, study,
                      visit_type, n_fact, zscore_fact, outlier_fact, prop_outlier_fact, n_site_fact,
                      zscore_site_fact, outlier_site_fact, prop_outlier_site_fact
}\if{html}{\out{</div>}}
}
\description{
Compute distance from mean to identify patients with an anomalous number of facts per year
of follow up
}
