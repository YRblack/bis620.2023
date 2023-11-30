#' Create a Histogram Plot for Study Types
#'
#' @description
#' This feature generates the histogram and enables subsetting of the data based on various study types.
#' There are five distinct study types: Observational, Observational [Patient Registry], Interventional, Expanded access, unknown.
#' The "unknown" types showed as "NA" in the histogram.
#' By showing the frequency of each study type, researchers can perform comparative analyses to understand how different types of studies contribute to the field.
#' For instance, if interventional studies are more common than observational ones, it may reflect a research trend or indicate where more resources are being allocated.
#'
#' @author Yiran Liu
#'
#' @param studies A data frame containing the details of studies.
#' @param sponsor_type The type of sponsor for the studies to be included in the plot.
#' @param status_type The type of status of the studies to be included in the plot.
#' @param brief_title_kw A keyword to filter the studies.
#'
#' @return A ggplot object representing the histogram of the number of studies by study type.
create_study_type_histogram_plot = function(studies, sponsor_type, status_type, brief_title_kw) {
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |>
      head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  d$study_type[is.na(d$study_type)] = "NA"
  study_type_level = c("Observational", "Observational [Patient Registry]",
                       "Interventional", "Expanded Access", "NA")
  d = d |>
    select(study_type) |>
    group_by(study_type) |>
    summarize(n = n())
  for (i in study_type_level){
    if (!(i %in% d$study_type)){
      d <- rbind(d, data.frame(study_type = i, n = 0))
    }
  }
  d$study_type = factor(d$study_type, levels = study_type_level)
  ggplot(d, aes(x = study_type, y = n)) +
    geom_col() +
    theme_bw() +
    xlab("Study Type") +
    ylab("Count")
}
