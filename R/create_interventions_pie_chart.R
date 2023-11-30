#' Create a Pie Chart of Interventions
#'
#' @description
#' This feature makes a colorful pie plot based on different intervention types.
#' There are eleven distinct intervention types: Behavioral, Biological, Combination Product, Device, Diagnostic Test, Dietary Supplement, Drug, Genetic, Other, Procedure, Radiation.
#' Each of them are represented in different colors.
#'
#' @author Yiran Liu
#'
#' @param studies A data frame containing the details of the studies.
#' @param interventions A data frame containing the interventions details.
#' @param sponsor_type The type of sponsor for the studies to be included in the chart.
#' @param status_type The status of studies to be included in the chart.
#' @param brief_title_kw Keyword(s) for filtering studies by their brief titles.
#' If this parameter is empty, only the first 1000 studies are considered.
#'
#' @return A ggplot object representing the pie chart for the interventions of
#' the filtered studies.
create_interventions_pie_chart = function(studies, interventions, sponsor_type, status_type, brief_title_kw){
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |> head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  interventions = interventions |> collect()
  # select rows from the interventions table based on nct-id
  nct_list = d$nct_id
  ints = interventions |> filter(nct_id %in% nct_list)

  ints |>
    mutate(name = fct_lump_prop(intervention_type, prop = 0.0025)) |>
    group_by(intervention_type) |>
    summarize(n = n()) |>
    arrange(desc(n)) |>
    ggplot(aes(x = "", y = n, fill = intervention_type)) +
    geom_bar(stat="identity", width=1) +
    coord_polar(theta = "y") + # Converts the bar chart to a pie chart
    theme_void() + # Removes unnecessary labels and axes
    labs(title = "Distribution of Intervention Types",
         fill = "Intervention Type")
}
