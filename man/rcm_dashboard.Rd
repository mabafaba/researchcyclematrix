% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/country_overview.R
\name{rcm_dashboard}
\alias{rcm_dashboard}
\title{compile an overview report of each countries status}
\usage{
rcm_dashboard(rcm = NULL, subs = NULL, update_rcm = T,
  unit = "data")
}
\arguments{
\item{rcm}{can be ommited}

\item{subs}{can be ommited}

\item{update_rcm}{logical: whether to update the rcm from the submissions sheet before producing the report. Slows it down a bit but hey at least your report is up to date afterwards. default is TRUE}

\item{unit}{which unit(s) to include? Defaults to 'data'. uses regex: for all units, enter a dot "."; for multiple units, use | operator (i.e. "data|reporting|research design|gis")}
}
\value{
nothing interesting, but writes a html file into the working directory and opens it in the browser
}
\description{
compile an overview report of each countries status
}
