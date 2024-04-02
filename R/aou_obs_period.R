#' Generate an observation period table
#'
#' `r lifecycle::badge('experimental')`
#'
#' @description Generates a temporary observation period table based the first
#'   and last event in the electronic medical record data. Because some EHR
#'   sites have contributed data from several decades ago, researchers might
#'   want to consider further constraining this table to reasonable date ranges
#'   of interest (e.g., setting all observation_period_start_date values to no
#'   earlier than 01/01/2010).
#'
#' @param cohort Reference to a remote table or local dataframe with a column
#'   called "person_id"
#' @param con Connection to the allofus SQL database. Defaults to
#'   getOption("aou.default.con"), which is set automatically if you use
#'   `aou_connect()`
#' @param collect Whether to bring the resulting table into local memory
#'   (`collect = TRUE`) as a dataframe or leave as a reference to a database
#'   table (for continued analysis using, e.g., `dbplyr`). Defaults to `FALSE.`
#' @param ... Further arguments passed along to `collect()` if `collect = TRUE`
#'
#' @details
#'
#' The current observation period table in the All of Us OMOP CDM is not always
#' appropriate for cohorts generated using OHDSI tools such as ATLAS. Some
#' observation periods are overly short and some participants have hundreds of
#' observation periods.
#'
#' This function generates an observation period table from the first occurrence
#' of a clinical event in the EHR tables to the last clinical event in the EHR
#' tables. It will only return a single observation period per person_id in the
#' database. If `collect = FALSE`, the function returns a query to a temporary
#' table in the database which can be referenced by typical dplyr functions.
#'
#' Normal OMOP conventions for EHR suggest that long lapses of time bewteen
#' clinical events may indicate that the person was not "observed" during this
#' period. However, due to the diverse nature of clinical EHR data contributed
#' to all of us, it seems most conservative to assume that the person was
#' observed from their first to last clinical event. See
#' https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html for more details.
#'
#' Some users have clinical events going back to before the time of widespread
#' electronic medical record use (e.g., the 1980s and 1990s). This function
#' considers all EHR data in the database, regardless of the date of the
#' clinical event, but we recommend that users consider the implications of
#' including data from the 1980s and 1990s. It may be more prudent to exclude
#' data prior to a more recent cutoff date so that the EHR data is more likely
#' to be accurate, though this decision depends highly on the research question
#' (see example below).
#'
#' Users should note that the aou_observation_period function will only generate
#' observation periods for participants who have at least one clinical
#' observation. If participant in the AllofUs research program who did not
#' include electronic health record data are included in the cohort argument, or
#' elected to contribute data but have no data to contribute, they will not be
#' included in the generated observation period table.
#'
#' @return A dataframe if `collect = TRUE`; a reference to a remote database
#'   table if not. Columns will be "person_id", "observation_period_start_date",
#'   and "observation_period_end_date".
#' @export
#'
#' @examplesIf on_workbench()
#' # create observation_period table for everyone
#' observation_period_tbl <- aou_observation_period()
#'
#' # create an index date as the first date a survey was taken
#' index_date_tbl <- tbl(con, "ds_survey") %>%
#'   group_by(person_id) %>%
#'   filter(survey_datetime == min(survey_datetime)) %>%
#'   mutate(index_date = as.Date(survey_datetime)) %>%
#'   distinct(person_id, index_date)
#'
#' # create a cohort of participants with EHR data and at least one year
#' # of observation starting on the date they took the first survey
#' cohort <- dplyr::tbl(con, "cb_search_person") %>%
#'   dplyr::filter(has_ehr_data == 1) %>%
#'   inner_join(index_date_tbl, by = "person_id") %>%
#'   inner_join(observation_period_tbl, by = "person_id") %>%
#'   filter(
#'     observation_period_end_date >= DATE_ADD(
#'       index_date,
#'       dplyr::sql(paste0("INTERVAL ", 1, " year"))
#'     ),
#'     observation_period_start_date <= index_date
#'   ) %>%
#'   select(person_id, gender, sex_at_birth, race, ethnicity, age_at_consent)
#'
#' # head(cohort)
#'
#' # create an observation period table with a minimum start date (e.g., 2010-01-01)
#' observation_period_tbl %>%
#'   mutate(
#'     observation_period_start_date =
#'       if_else(observation_period_start_date < as.Date("2010-01-01"),
#'         as.Date("2010-01-01"),
#'         observation_period_start_date
#'       )
#'   ) %>%
#'   filter(observation_period_end_date > as.Date("2010-01-01"))
#'
aou_observation_period <- function(cohort = NULL,
                                   collect = FALSE,
                                   ...,
                                   con = getOption("aou.default.con")) {
  if (is.null(con)) {
    cli::cli_abort(c("No connection available.",
      "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
      "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling observation periods for entire All of Us cohort."))
    function_cohort <- dplyr::tbl(con, "person") %>% dplyr::select("person_id")
  } else if (!"person_id" %in% colnames(cohort)) {
    # ensure person_id is a column name in cohort

    cli::cli_abort(c("{.code person_id} column not found in cohort.",
      "i" = "Confirm that the cohort has a column named {.code person_id}"
    ))
  } else if (is.data.frame(cohort)) {
    function_cohort <- dplyr::tbl(con, "person") %>%
      dplyr::filter(.data$person_id %in% !!unique(cohort$person_id)) %>%
      dplyr::select("person_id") %>%
      aou_compute()
  } else {
    function_cohort <- cohort %>%
      dplyr::distinct(.data$person_id)
  }

  q <- "

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
      "

  # add the create temp table text before and after
  tmp1 <- "CREATE TEMP TABLE table1 AS"
  tmp2 <- "SELECT * FROM table1"

  # combine the three vars
  out <- paste0(tmp1, "\n", q, ";\n", tmp2)

  CDR <- getOption("aou.default.cdr")
  out <- stringr::str_glue(out)

  # cat(out)
  # execute the query
  merged <- dplyr::left_join(function_cohort,
    get_query_table(out, collect = FALSE, con = con),
    by = "person_id"
  )

  if (isFALSE(collect)) {
    return(merged)
  }

  return(collect(merged, ...))
}
