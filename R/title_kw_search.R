#' Title Keyword Search
#'
#' @title title_kw_search
#' @description This function searches for studies in a given dataset based on keyword matching in the "brief_title" column.
#' It allows optional filtering based on sponsor type and status type.
#' @param studies A data frame containing the details of studies.
#' @param sponsor_type A variable representing the sponsor type to filter by. Default is 1.
#' @param status_type A variable representing the status type to filter by. Default is 1.
#' @param kw A keyword to search for in study titles.
#' @importFrom dplyr filter collect
#' @return A filtered dataset of studies matching the specified criteria.
title_kw_search = function(studies, sponsor_type, status_type, kw) {
  x = query_kwds(studies, kw, "brief_title", match_all = TRUE) |> collect()
  if (sponsor_type != 1){
    x = x |> filter(source_class == sponsor_type)
  }
  if (status_type != 1){
    x = x |> filter(overall_status == status_type)
  }
  return(x)
}
