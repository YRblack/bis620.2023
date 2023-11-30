#' Create a Histogram for Different Conditions
#'
#' @title create_condition_histogram
#' @description This function creates a histogram plot displaying the distribution of study conditions in clinical studies. It allows for optional filtering based on sponsor type, status type, and a keyword search in study titles.
#' @param studies A data frame containing the details of studies.
#' @param conditions A dataset of study conditions to join with the studies' data.
#' @param sponsor_type A variable representing the sponsor type to filter by. Default is 1.
#' @param status_type A variable representing the status type to filter by. Default is 1.
#' @param brief_title_kw A keyword to search for in study titles. Default is an empty string.
#' @return A histogram plot showing the distribution of study conditions based on the specified criteria.
create_condition_histogram = function(studies, conditions, sponsor_type, status_type, brief_title_kw) {
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |> head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  conditions = conditions |> collect()
  # select rows from the conditions table based on nct-id
  nct_list = d$nct_id
  cons = conditions |> filter(nct_id %in% nct_list)


  cons |>
    mutate(name = fct_lump_prop(name, prop = 0.0025)) |>
    group_by(name) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    mutate(Name = factor(name, levels = name)) |> # level order in desc order
    ggplot(aes(x = Name, y = n)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 270, hjust = 0)) +
    scale_y_log10() +
    ylab("Count") +
    xlab("Condition Names")
}
