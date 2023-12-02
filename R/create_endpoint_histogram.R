#' Create an Endpoint Histogram
#'
#' @title create_endpoint_histogram
#' @description This function creates a histogram plot visualizing the distribution of endpoints met in clinical studies. It allows for optional filtering based on sponsor type, status type, endpoints, and a keyword search in study titles.
#' @param studies A data frame containing the details of studies.
#' @param sponsor_type A variable representing the sponsor type to filter by. Default is 1.
#' @param status_type A variable representing the status type to filter by. Default is 1.
#' @param endpoints A dataset of endpoints to join with the studies' data.
#' @param kw A keyword to search for in study titles. Default is an empty string.
#' @importFrom dplyr select collect filter left_join group_by summarize
#' @importFrom ggplot2 ggplot geom_col scale_y_log10 theme_bw
#' @return A histogram plot displaying the distribution of endpoints met in studies based on the specified criteria.
create_endpoint_histogram = function(studies, sponsor_type, status_type, endpoints, kw) {

  em = query_kwds(studies, kw, "brief_title", match_all = TRUE) |>
    select(nct_id, source_class, overall_status) |>
    collect()

  if (sponsor_type != 1){
    em = em |> filter(source_class == sponsor_type)
  }

  if (status_type != 1) {
    em = em |> filter(overall_status == status_type)
  }

  em = em |>
    left_join(endpoints, by = "nct_id") |>
    group_by(endpoint_met) |>
    summarize(n = n())

  ggplot(em, aes(x = endpoint_met, y = n)) +
    geom_col() +
    scale_y_log10() +
    theme_bw()
}
