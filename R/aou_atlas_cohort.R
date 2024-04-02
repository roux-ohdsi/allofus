#' Retrieve a cohort from ATLAS for use in All of Us
#'
#' Retrieves a cohort definition from ATLAS and generates the cohort in All of Us.
#' Observation periods are first generated for each subject using the `aou_observation_period()`
#' function.The resulting cohort is a table with the cohort start and end dates for each person_id.
#' @details The function is based on a similar function in <https://github.com/cmayer2/r4aou> with some tweaks
#' to generate the appropriate observation periods and incorporate other package functions.
#' Please see the [online vignette](https://roux-ohdsi.github.io/allofus/articles/atlas.html) for additional details.
#' @param cohort_definition A cohort definition generated using `getCohortDefinition() from ROhdsiWebApi`
#' @param cohort_sql The cohort_sql generated using `getCohortSql() from ROhdsiWebApi`
#' @inheritParams aou_observation_period
#' @inheritParams aou_sql
#' @inherit aou_observation_period details
#' @return A dataframe if `collect = TRUE`; a reference to a remote database table if not. The SQL query used to generate the cohort is stored as an attribute.
#' @export
#'
#' @examplesIf on_workbench()
#' # generate a simple stroke cohort
#' # see https://atlas-demo.ohdsi.org/#/cohortdefinition/1788061
#' # If this cohort is not available, you can create one, or choose one already made.
#' # aou_cohort_example contains the results of
#' # cd <- ROhdsiWebApi::getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
#' # cd_sql <- ROhdsiWebApi::getCohortSql(cd, "https://atlas-demo.ohdsi.org/WebAPI")
#'
#' cohort <- aou_atlas_cohort(
#'   cohort_definition = aou_cohort_example$cd,
#'   cohort_sql = aou_cohort_example$cd_sql
#' )
#'
#' # print query that was executed
#' cat(attr(cohort, "query"))
#'
aou_atlas_cohort <- function(cohort_definition,
                             cohort_sql,
                             debug = FALSE,
                             collect = FALSE,
                             ...,
                             con = getOption("aou.default.con")) {
  if (!("id" %in% names(cohort_definition))) {
    cli::cli_abort(
      c("This function is designed to be used with cohort definitions created by {.code ROhdsiWebApi::getCohortDefinition()}",
        "Please see example in the documentation.",
        "i" = "Use {.code remotes::install_github(\"ohdsi/ROhdsiWebApi\")} to install ROhdsiWebApi."
      )
    )
  }

  out <- tryCatch(
    {
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
      modified_sql <- gsub("delete from cohort_censor_stats where cohort_definition_id = \\d+;", "", modified_sql)
      modified_sql <- gsub("@cdm_database_schema.observation_period", "#observation_period2", modified_sql)
      modified_sql <- gsub("@cdm_database_schema", "{cdr}", modified_sql)

      # Modify SQL
      modified_sql <- gsub("@results_database_schema.", "", cohort_sql_)
      modified_sql <- gsub("@vocabulary_database_schema", "@cdm_database_schema", modified_sql)
      modified_sql <- gsub("@target_database_schema.@", "#", modified_sql)
      modified_sql <- gsub("@target_cohort_id", cohort_id_, modified_sql)
      modified_sql <- gsub("delete from cohort_censor_stats where cohort_definition_id = \\d+;", "", modified_sql)
      modified_sql <- gsub("@cdm_database_schema.observation_period", "#observation_period2", modified_sql)
      modified_sql <- gsub("@cdm_database_schema", "{cdr}", modified_sql)

      # Create observation period table
      suppressWarnings({
        obs_period_sql <- paste(
          "CREATE TEMP TABLE #observation_period2 AS (
          WITH ehr AS (
            SELECT  m.person_id AS participant, MIN( m.measurement_date) AS first_date, MAX( m.measurement_date) AS last_date
            FROM {CDR}.measurement m
            LEFT JOIN {CDR}.measurement_ext AS m_ext ON m.measurement_id = m_ext.measurement_id
            WHERE LOWER(m_ext.src_id) LIKE 'ehr site%'--ehr data from measurement table
            GROUP BY 1

            UNION DISTINCT


            SELECT co.person_id AS participant, MIN(co.condition_start_date) AS first_date,
            CASE WHEN MAX(co.condition_start_date)>= MAX(co.condition_end_date) OR MAX( co.condition_end_date) IS NULL THEN MAX(co.condition_start_date)
            WHEN MAX(co.condition_start_date) < MAX(co.condition_end_date) OR MAX(co.condition_start_date) IS NULL THEN MAX(co.condition_end_date)
            END AS last_date -- select the max of the lastest record for both date columns
            FROM {CDR}.condition_occurrence AS co
            LEFT JOIN {CDR}.condition_occurrence_ext AS co_ext ON co.condition_occurrence_id = co_ext.condition_occurrence_id
            WHERE LOWER(co_ext.src_id) LIKE 'ehr site%' -- ehr data from condition_occurrence table
            GROUP BY 1

            UNION DISTINCT

            SELECT d.person_id AS participant, MIN( d.device_exposure_start_date) AS first_date,
            CASE WHEN MAX(d.device_exposure_start_date)>= MAX(d.device_exposure_end_date) OR MAX(d.device_exposure_end_date) IS NULL THEN MAX(d.device_exposure_start_date)
            WHEN MAX(d.device_exposure_start_date) < MAX(d.device_exposure_end_date) OR  MAX(d.device_exposure_start_date) IS NULL THEN MAX(d.device_exposure_end_date)
            END AS last_date --select the max of the lastest record for both date columns
            FROM {CDR}.device_exposure AS d
            LEFT JOIN {CDR}.device_exposure_ext AS d_ext ON d.device_exposure_id = d_ext.device_exposure_id
            WHERE LOWER(d_ext.src_id) LIKE 'ehr site%'-- ehr data from device_exposure table
            GROUP BY 1

            UNION DISTINCT

            SELECT de.person_id as participant, MIN(de.drug_exposure_start_date) AS first_date,
            CASE WHEN MAX(de.drug_exposure_start_date) >= MAX(de.drug_exposure_end_date) OR MAX(de.drug_exposure_end_date) IS NULL THEN MAX(de.drug_exposure_start_date)
            WHEN MAX(de.drug_exposure_start_date)< MAX(de.drug_exposure_end_date) OR MAX(de.drug_exposure_start_date) IS NULL THEN MAX(de.drug_exposure_end_date)
            END AS last_date
            FROM {CDR}.drug_exposure AS de
            LEFT JOIN {CDR}.drug_exposure_ext AS de_ext ON de.drug_exposure_id = de_ext.drug_exposure_id
            WHERE LOWER(de_ext.src_id) LIKE 'ehr site%' --ehr data from drug_exposure table
            GROUP BY 1

            UNION DISTINCT

            SELECT o.person_id AS participant, MIN(o.observation_date) AS first_date,
            MAX(o.observation_date) AS last_date
            FROM {CDR}.observation AS o
            LEFT JOIN {CDR}.observation_ext AS o_ext ON o.observation_id = o_ext.observation_id
            WHERE LOWER(o_ext.src_id) LIKE 'ehr site%' --ehr data from observation table
            GROUP BY 1


            UNION DISTINCT

            SELECT po.person_id AS participant, MIN(po.procedure_date) AS first_date,
            MAX(po.procedure_date) AS last_date
            FROM {CDR}.procedure_occurrence AS po
            LEFT JOIN {CDR}.procedure_occurrence_ext AS po_ext ON po.procedure_occurrence_id = po_ext.procedure_occurrence_id
            WHERE LOWER(po_ext.src_id) LIKE 'ehr site%' -- ehr data from procedure_occurrence table
            GROUP BY 1


            UNION DISTINCT

            SELECT v.person_id AS participant, MIN( v.visit_start_date ) AS first_date,
            CASE WHEN MAX ( v.visit_start_date )>= MAX( v.visit_end_date ) OR MAX( v.visit_end_date ) IS NULL THEN  MAX(v.visit_start_date)
            WHEN MAX ( v.visit_start_date ) < MAX ( v.visit_end_date ) OR MAX( v.visit_start_date ) IS NULL THEN MAX ( v.visit_end_date )
            END AS  last_date -- select between the end and start date by using max()
            FROM {CDR}.visit_occurrence AS v
            LEFT JOIN {CDR}.visit_occurrence_ext AS v_ext ON v.visit_occurrence_id = v_ext.visit_occurrence_id
            WHERE LOWER(v_ext.src_id) LIKE 'ehr site%'-- ehr data from visit occurrence table
            GROUP BY 1
          )

          SELECT
          participant AS person_id,  MIN (first_date) AS observation_period_start_date, MAX (last_date) as observation_period_end_date
          FROM ehr
          GROUP BY 1
          ORDER BY 1
    );
    "
        )
      })
      #obs_period_sql <- gsub("`visit_occurrence`", "`{cdr}.visit_occurrence`", obs_period_sql)

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

      r <- allofus::aou_sql(sql_translated, debug = debug, collect = collect, ..., con = con) %>%
        dplyr::rename(person_id = "subject_id")

      attr(r, "query") <- sql_translated

      r
    },
    error = function(e) {
      cli::cli_abort(c("Unable to pull the cohort from ATLAS. Are you sure you have installed `ROhdsiWebApi` and provided a cohort definition and cohort sql?"), call = NULL)
      return(e)
    }
  )

  return(out)
}
