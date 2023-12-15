#' Retrieve a cohort from ATLAS for use in AllofUs
#'
#' This function retrieves a cohort definition from ATLAS and generates the cohort in All of Us.
#' Observation periods are first generated for each subject using the [aou_observation_period()] function.
#' The resulting cohort is a dataframe with the cohort start and end dates for each subject.
#' The function is based on a similar function in <https://github.com/cmayer2/r4aou> with some tweaks
#' to generate the appropriate observation periods and incorporate other package functions.
#' @param cohort_definition A cohort definition generated using `ROhdsiWebApi::getCohortDefinition(cohort_id, base_url)`
#' @param cohort_sql The cohort_sql generated using `ROhdsiWebApi::getCohortSql(cohort_definition, base_url)`
#' @inheritParams aou_observation_period
#' @inheritParams aou_sql
#' @inherit aou_observation_period details
#' @return A dataframe with the resulting cohort. The SQL query used to generate the cohort is stored as an attribute.
#' @export
#'
#' @examples
#'\dontrun{
#'# generate a simple stroke cohort
#'# see https://atlas-demo.ohdsi.org/#/cohortdefinition/1788061
#'cd <- getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
#'cd_sql <- getCohortSql(cd, "https://atlas-demo.ohdsi.org/WebAPI")
#'cohort <- aou_atlas_cohort(cohort_definition = cd, cohort_sql = cd_sql)
#'
#'# print query that was executed
#'cat(attr(cohort, "query"))
#'}
#'
#'
aou_atlas_cohort <- function(cohort_definition,
                             cohort_sql,
                            persistence_window = 548,
                            end_date_buffer = 60,
                            exclude_aou_visits = FALSE,
                            debug = FALSE,
                            ...){

  if (!("id" %in% names(cohort_definition))) {
    cli::cli_abort(
      c("This function is designed to be used with cohort definitions created by {.code ROhdsiWebApi::getCohortDefinition()}",
        "Please see example in the documentation.",
      "i" = "Use {.code remotes::install_github(\"ohdsi/ROhdsiWebApi\")} to install ROhdsiWebApi."
    ))
  }

  cohort_definition_ <- cohort_definition
  cohort_sql_ <- cohort_sql
  cohort_id_ <- cohort_definition$id

  cli::cli_inform(c("i" = "Querying ATLAS...generating a cohort can take a few minutes."))
  # Credit to https://github.com/cmayer2/r4aou with a few tweaks

  # Modify SQL
  modified_sql <- gsub("@results_database_schema.", "", cohort_sql_)
  modified_sql <- gsub("@vocabulary_database_schema", "@cdm_database_schema", modified_sql)
  modified_sql <- gsub("@target_database_schema.@", "#", modified_sql)
  modified_sql <- gsub("@target_cohort_id", cohort_id_, modified_sql)
  modified_sql <- gsub("delete from cohort_censor_stats where cohort_definition_id = \\d+;","",modified_sql)
  modified_sql <- gsub("@cdm_database_schema.observation_period","#observation_period2",modified_sql)
  modified_sql <- gsub("@cdm_database_schema","{cdr}",modified_sql)

  # Create observation period table
  suppressWarnings({
    obs_period_sql <- paste(
      "CREATE TEMP TABLE #observation_period2 AS (
", dbplyr::sql_render(
        allofus::aou_observation_period(
          persistence_window = persistence_window,
          end_date_buffer = end_date_buffer,
          exclude_aou_visits = exclude_aou_visits,
          collect = FALSE
        )
      ),
      "
);
"
    )
  })
  obs_period_sql <- gsub("`visit_occurrence`", "`{cdr}.visit_occurrence`", obs_period_sql)

  # Create target cohort table
  target_cohort_sql <- "
CREATE TEMP TABLE #target_cohort_table (
    cohort_definition_id INT64 not null,
    subject_id INT64 not null,
    cohort_start_date DATE,
    cohort_end_date DATE
);
"

  # Add select statement to SQL
  select_sql <- "
SELECT * FROM #target_cohort_table;
"

  # Combine SQL statements
  sql_all <- paste0(obs_period_sql, target_cohort_sql, modified_sql, select_sql)

  # Translate SQL to BigQuery dialect
  sql_translated <- SqlRender::translate(sql_all, targetDialect = "bigquery")
  sql_translated <- gsub("create table", "CREATE TEMP TABLE", sql_translated)
  sql_translated <- gsub("CREATE TABLE", "CREATE TEMP TABLE", sql_translated)

  # Execute SQL

  r <- allofus::aou_sql(sql_translated, debug = debug, ...) %>%
    dplyr::rename(person_id = 'subject_id')

  attr(r, "query") <- sql_translated

  return(r)

}
