
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
                            base_url = "http://api.ohdsi.org/WebAPI"){

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
  obs_period_sql <- paste("
                          CREATE TEMP TABLE #observation_period2 AS (
                          ",
                          dbplyr::sql_render(
                            aou_observation_period(persistence_window = persistence_window,
                                                   end_date_buffer = end_date_buffer,
                                                   exclude_aou_visits = exclude_aou_visits)
                          ),
                          "
                          );
                          ")
  #   obs_period_sql <- "
  # CREATE TEMP TABLE #observation_period2 AS (
  #     SELECT
  #         person_id,
  #         observation_start_date as observation_period_start_date,
  #         date_add(observation_end_date, interval 60 day) as observation_period_end_date
  #     FROM (
  #         SELECT
  #             person_id,
  #             obs_period,
  #             min(visit_start_date) as observation_start_date,
  #             max(visit_end_date) as observation_end_date
  #         FROM (
  #             SELECT
  #                 person_id,
  #                 visit_start_date,
  #                 visit_end_date,
  #                 last_end_date,
  #                 days_between_visits,
  #                 if(obs_period is null, 1.0, obs_period + 1.0) as obs_period
  #             FROM (
  #                 SELECT
  #                     *,
  #                     sum(if(days_between_visits > 548.0, 1.0, 0.0)) over (partition by person_id order by person_id, visit_start_date, visit_end_date rows unbounded preceding) as obs_period
  #                 FROM (
  #                     SELECT
  #                         *,
  #                         date_diff(visit_start_date, last_end_date, day) as days_between_visits
  #                     FROM (
  #                         SELECT DISTINCT *
  #                         FROM (
  #                             SELECT
  #                                 person_id,
  #                                 visit_start_date,
  #                                 visit_end_date
  #                             FROM {cdr}.visit_occurrence
  #                         )
  #                     )
  #                 )
  #             )
  #         )
  #         GROUP BY 1, 2
  #     )
  # );
  # "

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
  sql <- paste0(obs_period_sql, target_cohort_sql, modified_sql, select_sql)

  # Translate SQL to BigQuery dialect
  sql_translated <- SqlRender::translate(sql, targetDialect = "bigquery")
  sql_translated <- gsub("create table", "CREATE TEMP TABLE", sql_translated)
  sql_translated <- gsub("CREATE TABLE", "CREATE TEMP TABLE", sql_translated)

  # Execute SQL
  r <- aou_sql(sql5)


  return(r)

}
