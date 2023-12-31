% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_endpoint_histogram.R
\name{create_endpoint_histogram}
\alias{create_endpoint_histogram}
\title{create_endpoint_histogram}
\usage{
create_endpoint_histogram(studies, sponsor_type, status_type, endpoints, kw)
}
\arguments{
\item{studies}{A data frame containing the details of studies.}

\item{sponsor_type}{A variable representing the sponsor type to filter by. Default is 1.}

\item{status_type}{A variable representing the status type to filter by. Default is 1.}

\item{endpoints}{A dataset of endpoints to join with the studies' data.}

\item{kw}{A keyword to search for in study titles. Default is an empty string.}
}
\value{
A histogram plot displaying the distribution of endpoints met in studies based on the specified criteria.
}
\description{
This function creates a histogram plot visualizing the distribution of endpoints met in clinical studies. It allows for optional filtering based on sponsor type, status type, endpoints, and a keyword search in study titles.
}
\details{
Create an Endpoint Histogram
}
