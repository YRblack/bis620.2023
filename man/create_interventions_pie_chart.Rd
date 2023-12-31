% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_interventions_pie_chart.R
\name{create_interventions_pie_chart}
\alias{create_interventions_pie_chart}
\title{Create a Pie Chart of Interventions}
\usage{
create_interventions_pie_chart(
  studies,
  interventions,
  sponsor_type,
  status_type,
  brief_title_kw
)
}
\arguments{
\item{studies}{A data frame containing the details of the studies.}

\item{interventions}{A data frame containing the interventions details.}

\item{sponsor_type}{The type of sponsor for the studies to be included in the chart.}

\item{status_type}{The status of studies to be included in the chart.}

\item{brief_title_kw}{Keyword(s) for filtering studies by their brief titles.
If this parameter is empty, only the first 1000 studies are considered.}
}
\value{
A ggplot object representing the pie chart for the interventions of
the filtered studies.
}
\description{
This feature makes a colorful pie plot based on different intervention types.
There are eleven distinct intervention types: Behavioral, Biological, Combination Product, Device, Diagnostic Test, Dietary Supplement, Drug, Genetic, Other, Procedure, Radiation.
Each of them are represented in different colors.
}
\author{
Yiran Liu
}
