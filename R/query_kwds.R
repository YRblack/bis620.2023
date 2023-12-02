#' Construct SQL query for Filtering
#'
#' @title query_kwds
#' @description This function constructs an SQL query for filtering a dataset based on keyword search criteria applied to a specific column.
#' @param tbl A dataset to filter.
#' @param kwds Keywords to search for.
#' @param column The name of the column in the dataset to perform the search.
#' @param ignore_case (Optional) A logical value indicating whether the search should be case-insensitive. Default is TRUE.
#' @param match_all (Optional) A logical value indicating whether all keywords must match (AND) or any keyword can match (OR). Default is FALSE.
#' @importFrom dplyr filter sql
#' @return A filtered dataset based on the constructed SQL query.
query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  kwds <- paste0("%", kwds, "%") |>
    gsub("'", "''", x = _)
  if (ignore_case) {
    like <- " ilike "
  } else{
    like <- " like "
  }
  query <- paste(
    paste0(column, like, "'", kwds, "'"),
    collapse = ifelse(match_all, " AND ", " OR ")
  )

  dplyr::filter(tbl, dplyr::sql(query))
}
