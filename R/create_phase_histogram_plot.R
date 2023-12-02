#' Create a Histogram Plot for Studies in Each Phase
#'
#' @title create_phase_histogram_plot
#' @description This function creates a histogram plot visualizing the distribution of study phases in a dataset. It allows for optional filtering based on sponsor type, status type, and a keyword search in study titles.
#' @param studies A data frame containing the details of studies.
#' @param sponsor_type A variable representing the sponsor type to filter by. Default is 1.
#' @param status_type A variable representing the status type to filter by. Default is 1.
#' @param brief_title_kw A keyword to search for in study titles. Default is an empty string.
#' @importFrom dplyr select group_by summarize
#' @importFrom ggplot2 ggplot geom_col theme_bw xlab ylab
#' @importFrom utils head
#' @return A histogram plot showing the distribution of study phases based on the specified criteria.
create_phase_histogram_plot = function(studies, sponsor_type, status_type, brief_title_kw) {
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |>
      head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  d$phase[is.na(d$phase)] = "NA"
  phase_level = c("Early Phase 1", "Phase 1",
                  "Phase 1/Phase 2", "Phase 2", "Phase 2/Phase 3",
                  "Phase 3", "Phase 4", "NA", "Not Applicable")
  d = d |>
    select(phase) |>
    group_by(phase) |>
    summarize(n = n())
  for (i in phase_level){
    if (!(i %in% d$phase)){
      d <- rbind(d, data.frame(phase = i, n = 0))
    }
  }
  d$phase = factor(d$phase, levels = phase_level)
  ggplot(d, aes(x = phase, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Phase") +
    ylab("Count")
}
