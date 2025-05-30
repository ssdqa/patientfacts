% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pf_output_subfuncs.R
\name{pf_ss_exp_la}
\alias{pf_ss_exp_la}
\title{\strong{Single Site, Exploratory, Longitudinal}}
\usage{
pf_ss_exp_la(data_tbl, output, facet, date_breaks_str = "1 year")
}
\arguments{
\item{data_tbl}{output from \code{pf_process} function}

\item{output}{desired output - have 2 options:
\itemize{
\item \code{median_fact_ct}: the median number of facts for each domain during the
specified time period
\item \code{sum_fact_ct}: the sum of facts for each domain during the specified time period
}}

\item{facet}{variables to facet (e.g., \code{domain}); vector of strings}

\item{date_breaks_str}{string to denote how time should be broken up in
the chart}
}
\value{
a dot and line chart displaying the \code{output} variable of interest per
domain across the user specified time period
}
\description{
\strong{Single Site, Exploratory, Longitudinal}
}
