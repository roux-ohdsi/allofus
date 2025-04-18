% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aou_atlas_cohort.R
\name{aou_atlas_cohort}
\alias{aou_atlas_cohort}
\title{Retrieve a cohort from ATLAS for use in All of Us}
\usage{
aou_atlas_cohort(
  cohort_definition,
  cohort_sql,
  debug = FALSE,
  collect = FALSE,
  ...,
  con = getOption("aou.default.con")
)
}
\arguments{
\item{cohort_definition}{A cohort definition generated using
\verb{getCohortDefinition() from ROhdsiWebApi}}

\item{cohort_sql}{The cohort_sql generated using \verb{getCohortSql() from ROhdsiWebApi}}

\item{debug}{Print the query to the console; useful for debugging.}

\item{collect}{Whether to bring the resulting table into local memory
(\code{collect = TRUE}) as a dataframe or leave as a reference to a database
table (for continued analysis using, e.g., \code{dbplyr}). Defaults to \code{FALSE.}}

\item{...}{Further arguments passed along to \code{collect()} if \code{collect = TRUE}}

\item{con}{Connection to the allofus SQL database. Defaults to
getOption("aou.default.con"), which is set automatically if you use
\code{aou_connect()}}
}
\value{
A dataframe if \code{collect = TRUE}; a reference to a remote database
table if not. The SQL query used to generate the cohort is stored as an
attribute.
}
\description{
Retrieves a cohort definition from ATLAS and generates the cohort in All of
Us. Observation periods are first generated for each subject using the
\code{aou_observation_period()} function.The resulting cohort is a table with the
cohort start and end dates for each person_id.
}
\details{
The function is based on a similar function in
\url{https://github.com/cmayer2/r4aou} with some tweaks to generate the
appropriate observation periods and incorporate other package functions.
Please see the \href{https://roux-ohdsi.github.io/allofus/vignettes/atlas.html}{online vignette} for
additional details. Note that some cohorts may not be compatible with \code{aou_atlas_cohort()} but setting
generateStats = FALSE in \code{getCohortSql()} can resolve some issues.
}
\examples{
# generate a simple stroke cohort
# see https://atlas-demo.ohdsi.org/#/cohortdefinition/1788061
# If this cohort is not available, you can create one, or choose one already made.
# aou_cohort_example contains the results of
# cd <- ROhdsiWebApi::getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
# for some cohorts, you must use the argument generateStats = FALSE or the cohort (its stats)
# can't be generated on All of Us
# cd_sql <- ROhdsiWebApi::getCohortSql(cd,
#                                      "https://atlas-demo.ohdsi.org/WebAPI",
#                                      generateStats = FALSE)

\dontrun{
# connect to the database
con <- aou_connect()

cohort <- aou_atlas_cohort(
  cohort_definition = aou_cohort_example$cd,
  cohort_sql = aou_cohort_example$cd_sql
)

# print query that was executed
cat(attr(cohort, "query"))
}

}
