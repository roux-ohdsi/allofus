
#' Retrieve a cohort from ATLAS for use in AllofUs
#'
#' @param cohort_id The ID of the cohort to retrieve
#' @param base_url The URL of the ATLAS instance to use ending in /WebAPI. Defaults to demo atlas at https://atlas-demo.ohdsi.org
#'
#' @return a dataframe or list with the resulting cohort. see include_query
#' @export
#'
#' @examples
#'
#' \dontrun{
#'  con <- allofus::aou_connect()
#'  cohort <- aou_atlas_cohort(1788061, baseUrl = "https://atlas-demo.ohdsi.org/WebAPI")
#' }
#'
aou_atlas_cohort <-function(cohort_id,
                            persistence_window = 548,
                            end_date_buffer = 60,
                            exclude_aou_visits = FALSE,
                            base_url = "http://api.ohdsi.org/WebAPI",
                            collect = FALSE){

  message("Querying ATLAS...generating a cohort can take a few minutes.")
  # Credit to https://github.com/cmayer2/r4aou with a few tweaks

  cohort_definition <- ROhdsiWebApi:::getCohortDefinition(cohort_id, base_url)
  cohort_sql <- ROhdsiWebApi:::getCohortSql(cohort_definition, base_url)

  # Modify SQL
  modified_sql <- gsub("@results_database_schema.", "", cohort_sql)
  modified_sql <- gsub("@vocabulary_database_schema", "@cdm_database_schema", modified_sql)
  modified_sql <- gsub("@target_database_schema.@", "#", modified_sql)
  modified_sql <- gsub("@target_cohort_id", cohort_id, modified_sql)
  modified_sql <- gsub("delete from cohort_censor_stats where cohort_definition_id = \\d+;","",modified_sql)
  modified_sql <- gsub("@cdm_database_schema.observation_period","#observation_period2",modified_sql)
  modified_sql <- gsub("@cdm_database_schema","{cdr}",modified_sql)

  # Create observation period table
  suppressWarnings({
  obs_period_sql <- paste("
                          CREATE TEMP TABLE #observation_period2 AS (
                          ",
                          dbplyr::sql_render(
                            aou_observation_period(persistence_window = persistence_window,
                                                   end_date_buffer = end_date_buffer,
                                                   exclude_aou_visits = exclude_aou_visits,
                                                   collect = FALSE)
                          ),
                          "
                          );
                          ")
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
  r <- aou_sql(sql_translated, collect = collect)

  return(r)

}
