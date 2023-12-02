#' Filtered Clinical Trials Distribution World Map
#'
#' @description
#' Generates a world map visualization showing the distribution of the clinical
#' trial counts across different countries based on the specified search criteria.
#' @author Houmin Xing
#' @param studies the 'studies' data frame containing information about all clinical trials
#' @param sponsor_type the specified sponsor type for studies filtering
#' @param status_type the specified status type for studies filtering
#' @param brief_title_kw Keyword(s) for filtering studies by their brief titles.
#' If this parameter is empty, only the first 1000 studies are considered.
#' @param countries the 'countries' data frame in our ctgov database
#' @importFrom dplyr collect filter group_by summarise n
#' @importFrom countrycode countrycode
#' @importFrom rworldmap joinCountryData2Map mapCountryData
#' @importFrom graphics par
#' @importFrom utils head
#' @returns Displays a world map showing the distribution of clinical trial counts (in log10 scale) based on specified filters.
create_world_map = function(studies, sponsor_type, status_type, brief_title_kw, countries) {
  if (brief_title_kw == ""){
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw) |>
      head(1000)
  }
  else{
    d = title_kw_search(studies, sponsor_type, status_type, brief_title_kw)
  }
  countries = countries |> collect()
  countries_subset = countries |> filter(nct_id %in% d$nct_id)
  x = countries_subset |> group_by(name) |>
    summarise(associated_trials_count = n())
  iso3 <- countrycode(sourcevar = x$name,
                      origin = "country.name", destination = "iso3c")
  x$name <- iso3
  x$log10_associated_trials_count <- log10(x$associated_trials_count)
  full_map_data <- joinCountryData2Map(x, joinCode = "ISO3",
                                       nameJoinColumn = "name")
  par(mai = c(0, 0, 0.4, 0), xaxs = "i", yaxs = "i")
  mapCountryData(full_map_data,
                 nameColumnToPlot = "log10_associated_trials_count")
}
