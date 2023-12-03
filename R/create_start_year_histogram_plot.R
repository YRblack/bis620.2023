#' Create a Histogram Plot for Different Start Years
#'
#' @title create_start_year_histogram_plot
#' @description This function creates a histogram plot displaying the distribution of study start years in clinical studies. It allows for optional filtering based on sponsor type, status type, and a keyword search in study titles, making it a versatile tool for exploring the temporal distribution of studies.
#' @author Yi Ren
#' @param studies A dataset of clinical studies, serving as the primary source of study information.
#' @param sponsor_type A variable representing the sponsor type to filter by. By default, it considers all sponsor types (sponsor_type = 1), but you can specify a specific sponsor type to focus the analysis.
#' @param status_type A variable representing the status type to filter by. By default, it considers all status types (status_type = 1), but you can specify a specific status type to narrow down the analysis.
#' @param brief_title_kw A keyword to search for in study titles. The default is an empty string, enabling you to further refine the analysis by focusing on studies with specific keywords in their titles.
#' @importFrom dplyr mutate group_by summarise
#' @importFrom ggplot2 ggplot geom_col theme_bw theme scale_y_log10 xlab ylab
#' @importFrom utils head
#' @return A histogram plot that visualizes the distribution of study start years based on the specified criteria. The plot helps in understanding the historical trend of clinical studies and can reveal patterns related to specific sponsor types or status types.
create_start_year_histogram_plot = function(studies, sponsor_type, status_type, brief_title_kw) {
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |>
      head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }

  d |>
    mutate(start_date = as.Date(start_date)) |>
    mutate(start_year = format(start_date,"%Y"))  |>
    group_by(start_year) |>
    summarise(n = n()) |>
    ggplot(aes(x = start_year, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270, hjust = 0)) +
    scale_y_log10() +
    xlab("Start Year") +
    ylab("Count")
}
