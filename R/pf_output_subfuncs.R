
#'
#' @import ggplot2
#' @import ggiraph
#' @importFrom tidyr tibble
#' @importFrom tidyr unite
#' @importFrom qicharts2 qic
#' @importFrom timetk plot_anomalies
#' @importFrom timetk plot_anomalies_decomp
#' @importFrom plotly layout
#' @importFrom graphics text
#'
NULL


#' **Single Site, Anomaly, Over Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' @param visit_filter the single visit type of focus for the output
#' @param domain_filter the single domain of focus for the output
#'
#' @return if analysis was executed by year or greater, a P Prime control chart
#'         is returned with outliers marked with orange dots
#'
#'         if analysis was executed by month or smaller, an STL regression is
#'         conducted and outliers are marked with red dots. the graphs representing
#'         the data removed in the regression are also returned
#'
pf_ss_anom_at <- function(data_tbl,
                          facet,
                          visit_filter,
                          domain_filter){

  facet <- facet %>% append('domain') %>% append('visit_type') %>% unique()

  time_inc <- data_tbl %>% filter(!is.na(time_increment)) %>% distinct(time_increment) %>% pull()

  op_w_facet <- data_tbl %>%
    filter(domain == domain_filter,
           visit_type == visit_filter) %>%
    unite(facet_col, !!!syms(facet), sep = '\n') %>%
    mutate(prop = prop_pts_fact)

  if(time_inc == 'year'){

    pp_qi <- qic(data = op_w_facet, x = time_start, y = pts_w_fact, chart = 'pp', facets = ~facet_col,
                n = pts_w_visit, title = 'Control Chart: Proportion Patients with Fact', ylab = 'Proportion',
                xlab = 'Time', show.grid = TRUE)

    op_dat <- pp_qi$data

    new_c <- ggplot(op_dat,aes(x,y)) +
      geom_ribbon(aes(ymin = lcl,ymax = ucl), fill = "lightgray",alpha = 0.4) +
      geom_line(colour = ssdqa_colors_standard[[12]], size = .5) +
      geom_line(aes(x,cl)) +
      geom_point(colour = ssdqa_colors_standard[[6]] , fill = ssdqa_colors_standard[[6]], size = 1) +
      geom_point(data = subset(op_dat, y >= ucl), color = ssdqa_colors_standard[[3]], size = 2) +
      geom_point(data = subset(op_dat, y <= lcl), color = ssdqa_colors_standard[[3]], size = 2) +
      #facet_wrap(~facet1) +
      theme_minimal() +
      ggtitle(label = paste0('Control Chart: Proportion Patients with ', visit_filter, ' ', domain_filter)) +
      labs(x = 'Time',
           y = 'Proportion')

    new_c[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                  'tooltip' = FALSE)

    output <- new_c

  }else{

    anomalies <-
      plot_anomalies(.data=data_tbl %>% filter(visit_type == visit_filter,
                                               domain == domain_filter),
                     .date_var=time_start,
                     .interactive = FALSE,
                     .title = paste0('Anomalous ', visit_filter, ' ', domain_filter, ' Over Time')) #%>%
      #layout(title = paste0('Anomalous ', visit_filter, ' ', domain_filter, ' Over Time'))

    decomp <-
      plot_anomalies_decomp(.data=data_tbl %>% filter(visit_type == visit_filter,
                                                      domain == domain_filter),
                            .date_var=time_start,
                            .interactive=FALSE,
                            .title = paste0('Anomalous ', visit_filter, ' ', domain_filter, ' Over Time')) #%>%
      #layout(title = paste0('Anomalous ', visit_filter, ' ', domain_filter, ' Over Time'))

    anomalies[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                      'tooltip' = FALSE)
    decomp[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                                   'tooltip' = FALSE)

    output <- list(anomalies, decomp)
  }

  return(output)

}

#' **Single Site, Exploratory, Over Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param output desired output - have 2 options:
#' - `median_fact_ct`: the median number of facts for each domain during the
#'                                   specified time period
#' - `sum_fact_ct`: the sum of facts for each domain during the specified time period
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' @param date_breaks_str string to denote how time should be broken up in
#'                        the chart
#'
#' @return a dot and line chart displaying the `output` variable of interest per
#'         domain across the user specified time period
#'

pf_ss_exp_at <- function(data_tbl,
                         output,
                         facet,
                         date_breaks_str = '1 year'){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  facet <- facet %>% append('visit_type') %>% unique()

  if(output=='median_fact_ct'){
    y_title='Median Fact Count'
  }else if(output=='sum_fact_ct'){
      y_title='Sum of Facts'
  }else(cli::cli_abort('Please select a valid output: {.code median_fact_ct} or {.code sum_fact_ct}'))
  # if(output=='fact_ct_denom') {y_title='What does this represent'}

  p <- ggplot(data_tbl, aes(x=time_start, y=!! sym(output), group=domain, fill=domain)) +
    geom_point(aes(color=domain)) +
    geom_smooth(method='loess',formula=y~x, size=0.5) +
    facet_wrap((facet), scales = 'free_y') +
    scale_fill_ssdqa() +
    scale_color_ssdqa() +
    theme_minimal() +
    scale_x_date(date_breaks=date_breaks_str) +
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    labs(x='Time Period',
         y=y_title)

  p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = FALSE)

  return(p)

}

#' **Multi-Site, Anomaly, Over Time**
#'
#' @param process_output output of `pf_process` function
#' @param domain_filter one of the user provided domains in the process_output table to be used
#'                      to filter down the output
#' @param visit_filter one of the user provided visit types in the process_output table to be used
#'                     to filter down the output
#'
#' @return returns three graphs:
#'    1) line graph that shows the smoothed proportion of a
#'    domain across time computation with the Euclidean distance associated with each line
#'    2) line graph that shows the raw proportion of a
#'    domain across time computation with the Euclidean distance associated with each line
#'    3) a bar graph with the Euclidean distance value for each site, with the average
#'    proportion as the fill
#'
pf_ms_anom_at <- function(process_output,
                          domain_filter,
                          visit_filter) {


  filt_op <- process_output %>% filter(visit_type == visit_filter,
                                       domain == domain_filter)

  allsites <-
    filt_op %>%
    select(time_start,visit_type,domain,mean_allsiteprop) %>% distinct() %>%
    rename(prop=mean_allsiteprop) %>%
    mutate(site='all site average') %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Proportion Patients with Fact: ",prop),
           text_raw=paste0("Site: ", site,
                           "\n","Proportion Patients with Fact: ",prop))

  dat_to_plot <-
    filt_op %>%
    rename(prop = prop_pts_fact) %>%
    mutate(text_smooth=paste0("Site: ", site,
                              "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean),
           text_raw=paste0("Site: ", site,
                           "\n","Site Proportion: ",prop,
                           "\n","Site Smoothed Proportion: ",site_loess,
                           "\n","Euclidean Distance from All-Site Mean: ",dist_eucl_mean))

  p <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site, group = site, text = text_smooth)) +
    geom_line(data=allsites, linewidth=1.1) +
    geom_smooth(se=TRUE,alpha=0.1,linewidth=0.5, formula = y ~ x) +
    scale_color_ssdqa() +
    theme_minimal() +
    labs(y = 'Proportion \n(Loess)',
         x = 'Time',
         title = paste0('Smoothed Proportion Patients with ', visit_filter, ' ', domain_filter, ' Across Time'))

  q <- dat_to_plot %>%
    ggplot(aes(y = prop, x = time_start, color = site,
               group=site, text=text_raw)) +
    geom_line(data=allsites,linewidth=1.1) +
    geom_line(linewidth=0.2) +
    scale_color_ssdqa() +
    theme_minimal() +
    labs(x = 'Time',
         y = 'Proportion',
         title = paste0('Proportion Patients with ', visit_filter, ' ', domain_filter, ' Across Time'))

  t <- dat_to_plot %>%
    distinct(site, dist_eucl_mean, site_loess) %>%
    group_by(site, dist_eucl_mean) %>%
    summarise(mean_site_loess = mean(site_loess)) %>%
    mutate(tooltip = paste0('Site: ', site,
                            '\nEuclidean Distance: ', dist_eucl_mean,
                            '\nAverage Loess Proportion: ', mean_site_loess)) %>%
    ggplot(aes(x = site, y = dist_eucl_mean, fill = mean_site_loess, tooltip = tooltip)) +
    geom_col_interactive() +
    coord_radial(r.axis.inside = FALSE, rotate.angle = TRUE) +
    guides(theta = guide_axis_theta(angle = 0)) +
    theme_minimal() +
    scale_fill_ssdqa(palette = 'diverging', discrete = FALSE) +
    labs(fill = 'Avg. Proportion \n(Loess)',
         y ='Euclidean Distance',
         x = '',
         title = paste0('Euclidean Distance for ', visit_filter, ' ', domain_filter))

  p[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  q[['metadata']] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = TRUE)

  t[['metadata']] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  output <- list(p,
                 q,
                 t)

  return(output)

}

#' **Multi-Site, Exploratory, Across Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param output desired output - have 2 options:
#' - `median_fact_ct`: the median number of facts for each domain during the
#'                                   specified time period
#' - `sum_fact_ct`: the sum of facts for each domain during the specified time period
#' @param facet variables to facet (e.g., `domain`); vector of strings
#'
#' @return line graph representing the output variable of interest across time
#'         for each of the sites of interest; each site is represented by one line
#'
pf_ms_exp_at <- function(data_tbl,
                         output,
                         facet = NULL){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(output=='median_fact_ct'){
    title <- 'Median Fact Count Across Time'
    y_lab <- 'Median Fact Count'
  }else if(output=='sum_fact_ct'){
    title <- 'Total Fact Count Across Time'
    y_lab <- 'Sum Fact Count'
  }else(cli::cli_abort('Please select a valid output: {.code median_fact_ct} or {.code sum_fact_ct}'))

  facet <- facet %>% append('domain') %>% unique()

  p <- data_tbl %>%
    ggplot(aes(x = time_start, y = !!sym(output), fill = site, color = site)) +
    geom_line() +
    facet_wrap((facet)) +
    theme_minimal() +
    scale_fill_ssdqa() +
    scale_color_ssdqa() +
    labs(title = title,
         x = 'Time',
         y = y_lab)

  p[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                            'tooltip' = FALSE)

  return(p)
}

#' **Single Site, Anomaly Detection, No Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param output desired output - have 4 options:
#' - `outlier_fact`: the number of facts overall (i.e. not grouped by site) that fall 2 SD
#'                                 away from the mean
#' - `prop_outlier_fact`: the proportion of facts overall that fall 2 SD away from the mean
#' - `outlier_site_fact`: the number of facts per site that fall 2 SD away from the mean
#' - `prop_outlier_site_fact`: the proportion of facts per site that fall 2 SD away from the mean
#' @param facet variables to facet (e.g., `domain`); vector of strings
#'
#' @return a bar graph displaying the output value of interest, which represents
#'         patients falling +/- 2 standard deviations away from the mean facts
#'         per follow-up for a given domain
#'
pf_ss_anom_nt <- function(data_tbl,
                          output,
                          facet=c('visit_type')){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  facet <- facet %>% append('visit_type') %>% unique()

  if(output=='outlier_fact'){
    y_title = 'Number of Patients with Fact +/- 2 SD Away from Mean'
    x_lab = 'Number of Anomalous Patients with Fact'
  }else if(output=='prop_outlier_fact'){
    y_title = 'Proportion of Patients with Fact +/- 2 SD Away from Mean'
    x_lab = 'Proportion of Anomalous Patients with Fact'
  }else if(output=='outlier_tot'){
    y_title = 'Number of Total Patients +/- 2 SD Away from Mean'
    x_lab = 'Number of Anomalous Total Patients'
  }else if(output=='prop_outlier_tot'){
      y_title = 'Proportion of Total Patients +/- 2 SD Away from Mean'
      x_lab = 'Proportion of Anomalous Total Patients'
  }else(cli::cli_abort('Please select a valid output: {.code outlier_fact}, {.code prop_outlier_fact}, {.code outlier_tot}, or
             {.code prop_outlier_tot}'))

  plt <- ggplot(data_tbl,
         aes(x = !!sym(output), y = domain, fill = domain)) +
    geom_col() +
    facet_wrap((facet)) +
    scale_fill_ssdqa() +
    labs(title = y_title,
         y = 'Domain',
         x = x_lab) +
    theme_minimal() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = FALSE)

  return(plt)

}


#' **Single-Site, Exploratory, No Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param output desired output - have 2 options:
#' - `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#'                                       (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' - `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#'                                         (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' @param facet variables to facet (e.g., `domain`); vector of strings
#'
#'
#' @return a bar graph displaying the median facts per follow-up for each domain
#'         and visit_type
#'
pf_ss_exp_nt <- function(data_tbl,
                         output,
                         facet) {

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  facet <- facet %>% append('visit_type') %>% unique()

  if(output=='median_site_with0s'){
    y_title='Median Facts / Follow-Up for All Patients'
  }else if(output=='median_site_without0s'){
      y_title='Median Facts / Follow-Up for Patients with Fact'
  }else(cli::cli_abort('Please select a valid output: {.code median_site_with0s} or {.code median_site_without0s}'))

  plt <- ggplot(data_tbl,
         aes(x=domain, y=!! sym(output), fill=domain)) +
    geom_bar(stat='identity', show.legend = FALSE) +
    facet_wrap((facet)) +
    labs(y=y_title,
         x='Domain') +
    scale_fill_ssdqa() +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid.major = element_line(size=0.4, linetype = 'solid'),
          panel.grid.minor = element_line(size=0.2, linetype = 'dashed'))

  plt[["metadata"]] <- tibble('pkg_backend' = 'plotly',
                              'tooltip' = FALSE)

  return(plt)

}



#' **Multi-Site, Anomaly Detection, No Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param facet variables to facet (e.g., `domain`); vector of strings
#' @param visit_filter the single visit_type of interest to be used in the analysis
#'
#' @return a dot plot where the shape of the dot represents whether the point is
#'         anomalous, the color of the dot represents the proportion of patients
#'         for a given domain, and the size of the dot represents the mean proportion
#'         across all sites
#'
#'         if no groups were eligible for anomaly detection analysis, a heatmap summarizing
#'         the proportion of patients for a given domain & site and a dot plot summarizing
#'         the average standard deviation for each site are returned as an alternative
#'
pf_ms_anom_nt <- function(data_tbl,
                          facet = NULL,
                          visit_filter = 'inpatient'){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  comparison_col <- 'prop_pt_fact'

  check_n <- data_tbl %>%
    filter(anomaly_yn != 'no outlier in group')

  dat_to_plot <- data_tbl %>%
    filter(visit_type == visit_filter) %>%
    mutate(text=paste("Domain: ", domain,
                      "\nSite: ",site,
                      "\nProportion: ",round(!!sym(comparison_col),2),
                      "\nMean proportion:",round(mean_val,2),
                      '\nSD: ', round(sd_val,2),
                      "\nMedian proportion: ",round(median_val,2),
                      "\nMAD: ", round(mad_val,2))) %>%
    mutate(anomaly_yn = ifelse(anomaly_yn == 'no outlier in group', 'not outlier', anomaly_yn))


  if(nrow(check_n) > 0){
    plt<-ggplot(dat_to_plot,
                aes(x=site, y=domain, text=text, color=!!sym(comparison_col)))+
      geom_point_interactive(aes(size=mean_val,shape=anomaly_yn, tooltip = text))+
      geom_point_interactive(data = dat_to_plot %>% filter(anomaly_yn == 'not outlier'),
                             aes(size=mean_val,shape=anomaly_yn, tooltip = text), shape = 1, color = 'black')+
      scale_color_ssdqa(palette = 'diverging', discrete = FALSE) +
      scale_shape_manual(values=c(19,8))+
      scale_y_discrete(labels = function(x) str_wrap(x, width = 60)) +
      theme_minimal() +
      facet_wrap((facet)) +
      labs(size="",
           title=paste0('Anomalous Proportion of Patients with Facts \nfor ', visit_filter, ' Visits'),
           subtitle = 'Dot size is the mean proportion per domain',
           y = 'Domain',
           x = 'Site') +
      guides(color = guide_colorbar(title = 'Proportion'),
             shape = guide_legend(title = 'Anomaly'),
             size = 'none')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)

    return(plt)

  }else{
    plt <- ggplot(dat_to_plot, aes(x = site, y = domain, fill = !!sym(comparison_col),
                                   tooltip = text)) +
      geom_tile_interactive() +
      theme_minimal() +
      scale_fill_ssdqa(discrete = FALSE, palette = 'diverging') +
      labs(y = 'Domain',
           x = 'Site',
           fill = 'Proportion')

    # Test Site Score using SD Computation
    test_site_score <- data_tbl %>%
      mutate(dist_mean = (!!sym(comparison_col) - mean_val)^2) %>%
      group_by(site) %>%
      summarise(n_grp = n(),
                dist_mean_sum = sum(dist_mean),
                overall_sd = sqrt(dist_mean_sum / n_grp)) %>%
      mutate(tooltip = paste0('Site: ', site,
                              '\nStandard Deviation: ', round(overall_sd, 3)))

    ylim_max <- test_site_score %>% filter(overall_sd == max(overall_sd)) %>% pull(overall_sd) + 1
    ylim_min <- test_site_score %>% filter(overall_sd == min(overall_sd)) %>% pull(overall_sd) - 1

    g2 <- ggplot(test_site_score, aes(y = overall_sd, x = site, color = site,
                                      tooltip = tooltip)) +
      geom_point_interactive(show.legend = FALSE) +
      theme_minimal() +
      scale_color_ssdqa() +
      geom_hline(yintercept = 0, linetype = 'solid') +
      labs(title = 'Average Standard Deviation per Site',
           y = 'Average Standard Deviation',
           x = 'Site')

    plt[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                                'tooltip' = TRUE)
    g2[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                               'tooltip' = TRUE)

    opt <- list(plt,
                g2)

    return(opt)
  }

}

#' **Multi-Site, Exploratory, No Time**
#'
#' @param data_tbl output from `pf_process` function
#' @param output desired output - have 2 options:
#' - `median_site_with0s`: specific site median, including patients with no evidence of patient fact
#'                                       (e.g., if domain = labs, includes in the median all patients with and without any labs)
#' - `median_site_without0s`: specific site median, not including patients without evidence of patient fact
#'                                          (e.g., if domain = labs, only includes median for patients with evidence of a lab)
#' @param facet variables to facet (e.g., `domain`); vector of strings
#'
#' @return a dot plot displaying the median facts per follow up for each domain
#'         and site compared to the all-site median (star icon)
#'

pf_ms_exp_nt <- function(data_tbl,
                         output,
                         facet){

  cli::cli_div(theme = list(span.code = list(color = 'blue')))

  if(output=='median_site_with0s'){
    y_title='Median Facts / Follow-Up for All Patients Across Sites'
    comp_var = 'median_all_with0s'
  }else if(output=='median_site_without0s'){
      y_title='Median Facts / Follow-Up for Patients with Fact Across Sites'
      comp_var = 'median_all_without0s'
  }else(cli::cli_abort('Please select a valid output: {.code median_site_with0s} or {.code median_site_without0s}'))

  facet <- facet %>% append('visit_type') %>% unique()

  data_format <- data_tbl %>%
    mutate(n_w_fact = format(n_w_fact, big.mark = ',', scientific = FALSE),
           site_lab = paste0(site, ' \n(No. of Patients = ', n_w_fact, ')'))

  r <- ggplot(data_format,
              aes(x=domain,y=!! sym(output), colour=site))+
    geom_point_interactive(aes(data_id=site_lab, tooltip = site_lab), size=3)+
    geom_point(aes(x=domain, y=!! sym(comp_var)), shape=8, size=3, color="black")+
    scale_color_ssdqa() +
    facet_wrap((facet), scales="free_x", ncol=2)+
    theme_minimal() +
    labs(y = y_title,
         x = 'Domain') +
    coord_flip()

  r[["metadata"]] <- tibble('pkg_backend' = 'ggiraph',
                            'tooltip' = TRUE)

  return(r)
}
