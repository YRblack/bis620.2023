#' Construct  dplyr query for Filtering
#'
#' @title query_kwds
#' @description This function construct dplyr query for filtering a dataset based on keyword search criteria applied to a specific column.
#' @param tbl A dataset to filter.
#' @param kwds Keywords to search for.
#' @param column The name of the column in the dataset to perform the search.
#' @param ignore_case (Optional) A logical value indicating whether the search should be case-insensitive. Default is TRUE.
#' @param match_all (Optional) A logical value indicating whether all keywords must match (AND) or any keyword can match (OR). Default is FALSE.
#' @importFrom dplyr filter sql
#' @return A filtered dataset based on the constructed SQL query.

query_kwds <- function(tbl, kwds, column, ignore_case = TRUE, match_all = FALSE) {
  # Prepare keywords for matching
  kwds <- paste0(".*", kwds, ".*")

  # Case-insensitive matching if specified
  if (ignore_case) {
    kwds <- paste0("(?i)", kwds)
  }

  # Construct the regular expression
  regex <- paste(kwds, collapse = ifelse(match_all, "|", ".*|.*"))

  # Use dplyr::filter to filter the table based on the constructed regex
  dplyr::filter(tbl, grepl(regex, tbl[[column]], perl = TRUE))
}
