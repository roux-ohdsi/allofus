% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aou_atlas_cohort.R
\name{aou_atlas_cohort}
\alias{aou_atlas_cohort}
\title{Retrieve a cohort from ATLAS for use in All of Us}
\usage{
aou_atlas_cohort(cohort_definition, cohort_sql, debug = FALSE, ...)
}
\arguments{
\item{cohort_definition}{A cohort definition generated using \verb{getCohortDefinition() from ROhdsiWebApi}}

\item{cohort_sql}{The cohort_sql generated using \verb{getCohortSql() from ROhdsiWebApi}}

\item{debug}{Print the query to the console; useful for debugging.}

\item{...}{Further arguments passed along to \code{collect()} if \code{collect = TRUE}

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}}
}
\value{
A dataframe with the resulting cohort. The SQL query used to generate the cohort is stored as an attribute.
}
\description{
This function retrieves a cohort definition from ATLAS and generates the cohort in All of Us.
Observation periods are first generated for each subject using the \code{\link[=aou_observation_period]{aou_observation_period()}} function.
The resulting cohort is a dataframe with the cohort start and end dates for each subject.
The function is based on a similar function in \url{https://github.com/cmayer2/r4aou} with some tweaks
to generate the appropriate observation periods and incorporate other package functions.
Please see the online vignette for additional details insert link here
}
\details{
The current observation period table in the All of Us OMOP CDM is not always
appropriate for cohorts generated using OHDSI tools such as ATLAS. Some observation
periods are overly short and some participants have hundreds of observation periods.

This function generates an observation period table from the first occurrence of
a clinical event in the EHR tables to the last clinical event in the EHR tables.
It will only return a single observation period per person_id in the database. If
\code{collect = FALSE}, the function returns a query to a temporary table in the database
which can be referenced by typical dplyr functions.

Normal OMOP conventions for EHR suggest that long lapses of time bewteen clinical
events may indicate that the person was not "observed" during this period. However,
due to the diverse nature of clinical EHR data contributed to all of us, it seems
most conservative to assume that the person was observed from their first to last
clinical event. See https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html
for more details.

Some users have clinical events going back to before the time of widespread
electronic medical record use (e.g., the 1980s and 1990s). This function considers
all EHR data in the database, regardless of the date of the clinical event, but we
recommend that users consider the implications of including data from the 1980s and 1990s.
It may be more prudent to exclude data prior to a more recent cutoff date so that the EHR
data is more likely to be accurate, though this decision depends highly on the research
question (see example below).

Users should note that the aou_observation_period function will only generate observation periods for
participants who have at least one clinical observation. If participant in the AllofUs research
program who did not include electronic health record data are included in the cohort argument, or
elected to contribute data but have no data to contribute, they will not be included in the
generated observation period table.
}
\examples{
\dontshow{if (on_workbench()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# generate a simple stroke cohort
# see https://atlas-demo.ohdsi.org/#/cohortdefinition/1788061
# If this cohort is not available, you can create one, or choose one already made.
# aou_cohort_example contains the results of
# cd <- ROhdsiWebApi::getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
# cd_sql <- ROhdsiWebApi::getCohortSql(cd, "https://atlas-demo.ohdsi.org/WebAPI")

cohort <- aou_atlas_cohort(
  cohort_definition = aou_cohort_example$cd,
  cohort_sql = aou_cohort_example$cd_sql
)

# print query that was executed
cat(attr(cohort, "query"))
\dontshow{\}) # examplesIf}
}
