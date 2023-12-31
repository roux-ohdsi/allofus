% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aou_obs_period.R
\name{aou_observation_period}
\alias{aou_observation_period}
\title{Generate an observation period table based on OMOP Conventions}
\usage{
aou_observation_period(
  cohort = NULL,
  persistence_window = 548,
  end_date_buffer = 60,
  exclude_aou_visits = FALSE,
  con = getOption("aou.default.con"),
  collect = FALSE,
  ...
)
}
\arguments{
\item{cohort}{Query to a cohort or local dataframe with column "person_id". If no cohort is provided,
defaults to the entire All of Us cohort}

\item{persistence_window}{Longest allowable time between visits for the same observation period. Defaults to 548 (see details)}

\item{end_date_buffer}{Number of days to add to last observed date. Defaults to 60 (see details)}

\item{exclude_aou_visits}{Whether to exclude All of Us clinical visits (i.e., for program-specific measurements,
not part of the participants' typical EHR) from the observation period. Defaults to \code{FALSE}}

\item{con}{Connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use \code{aou_connect()}}

\item{collect}{Whether to collect the data or keep as SQL query. Defaults to \code{FALSE}.}

\item{...}{Further arguments passed along to \code{collect()} if \code{collect = TRUE}}
}
\value{
a sql query or local data frame
}
\description{
Generate an observation period table based on OMOP Conventions
}
\details{
Follows conventions described here: \url{https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html}
}
\examples{
\dontshow{if (on_workbench()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
cohort <- dplyr::tbl(con, "cb_search_person") \%>\%
  dplyr::filter(has_ehr_data == 1) \%>\%
  head(100) \%>\%
  dplyr::select(person_id)

observation_periods <- aou_observation_period(cohort,
  persistence_window = 548,
  end_date_buffer = 60,
  collect = FALSE
)
\dontshow{\}) # examplesIf}
}
