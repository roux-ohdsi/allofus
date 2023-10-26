# AoU helpers

#' Create a connection to the database in All of Us
#'
#'
#' @description Use this function to create a `BigQueryConnection` object.
#' You can reference this object to connect to the All of Us database and run
#' SQL code using, e.g., `dbplyr` or `DBI`. A message is printed with the connection
#' status (successful or not).
#' @param CDR The name of the "curated data repository" that will be used in any
#' references of the form "{CDR}" or "{cdr}" in the query. Defaults to
#' getOption("aou.default.cdr"), which is Sys.getenv('WORKSPACE_CDR') if not specified otherwise
#' (the "mainline" CDR). On the controlled tier, specify the "base" CDR with
#' `CDR = paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @return A `BigQueryConnection` object. This object is also saved as getOption("aou.default.con").
#' @export
#' @examples
#' \dontrun{
#' con <- aou_connect()
#' # reference the observation table in the database
#' dplyr::tbl(con, "observation")
#' # print a list of the tables in the database
#' DBI::dbListTables(con)
#' }
aou_connect <- function(CDR = getOption("aou.default.cdr")) {
  dataset <- strsplit(CDR, split = "\\.")[[1]]
  release <- dataset[2]
  prefix <- dataset[1]

  connection <- DBI::dbConnect(
    bigrquery::bigquery(),
    billing = Sys.getenv("GOOGLE_PROJECT"),
    project = prefix,
    dataset = release,
    bigint = "integer64" # fix for big integers
  )

  options(aou.default.con = connection)

  if (isTRUE(connection@dataset == release)) {
    cat(cli::col_green("Connected successfully!"))
  } else {
    cat(cli::col_red("Error: Unable to connect"))
  }

  return(connection)
}

#' Execute a SQL query
#'
#' @param query A SQL query (BigQuery dialect) to be executed. Interpreted
#' with `glue::glue()`, so expressions enclosed with braces will be evaluated.
#' References to `cdr` or `CDR` will be evaluated automatically.
#' @param CDR The name of the "curated data repository" that will be used in any
#' references of the form "{CDR}" or "{cdr}" in the query. Defaults to
#' getOption("aou.default.cdr"), which is Sys.getenv('WORKSPACE_CDR') if not specified otherwise
#' (the "mainline" CDR). On the controlled tier, specify the "base" CDR with
#' `CDR = paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @param collect Do you want to retrieve the result of the SQL query into
#' the local environment? Defaults to`TRUE`.
#' @param ... All other arguments passed to `bigrquery::bq_table_download()`
#'
#' @return A dataframe (if `collect = TRUE`); otherwise, a `bq_table` object
#' @export
#'
#' @examples
#' # Examples based on AoU snippets
#' \dontrun{
#' aou_sql('
#'   -- Compute the count of unique participants in our All of Us cohort.
#'   SELECT
#'   COUNT(DISTINCT person_id) AS total_number_of_participants
#'   FROM
#'   `{CDR}.person`
#' ')
#'
#' MEASUREMENT_OF_INTEREST <- "hemoglobin"
#' aou_sql('
#' -- Compute summary information for our measurements of interest for our cohort.
#' --
#' -- PARAMETERS:
#' --   MEASUREMENT_OF_INTEREST: a case-insensitive string, such as "hemoglobin", to be compared
#' --                            to all measurement concept names to identify those of interest
#'
#' WITH
#'   --
#'   -- Use a case insensitive string to search the measurement concept names of those
#'   -- measurements we do have in the measurements table.
#'   --
#'   labs_of_interest AS (
#'   SELECT
#'     measurement_concept_id,
#'     measurement_concept.concept_name AS measurement_name,
#'     unit_concept_id,
#'     unit_concept.concept_name AS unit_name
#'   FROM
#'     `{CDR}.measurement`
#'   LEFT JOIN `{CDR}.concept` AS measurement_concept
#'   ON measurement_concept.concept_id = measurement_concept_id
#'   LEFT JOIN `{CDR}.concept` AS unit_concept
#'   ON unit_concept.concept_id = unit_concept_id
#'   WHERE
#'     REGEXP_CONTAINS(measurement_concept.concept_name, r"(?i){MEASUREMENT_OF_INTEREST}")
#'   GROUP BY
#'     measurement_concept_id,
#'     unit_concept_id,
#'     measurement_concept.concept_name,
#'     unit_concept.concept_name
#' )
#'   --
#'   -- Summarize the information about each measurement concept of interest that our
#'   -- prior query identified.
#'   --
#' SELECT
#'   measurement_name AS measurement,
#'   IFNULL(unit_name, "NA") AS unit,
#'   COUNT(1) AS N,
#'   COUNTIF(value_as_number IS NULL
#'     AND (value_as_concept_id IS NULL
#'       OR value_as_concept_id = 0)) AS missing,
#'   MIN(value_as_number) AS min,
#'   MAX(value_as_number) AS max,
#'   AVG(value_as_number) AS avg,
#'   STDDEV(value_as_number) AS stddev,
#'   APPROX_QUANTILES(value_as_number, 4) AS quantiles,
#'   COUNTIF(value_as_number IS NOT NULL) AS num_numeric_values,
#'   COUNTIF(value_as_concept_id IS NOT NULL
#'       AND value_as_concept_id != 0) AS num_concept_values,
#'   COUNTIF(operator_concept_id IS NOT NULL) AS num_operators,
#'   IF(src_id = "PPI/PM", "PPI", "EHR") AS measurement_source,
#'   measurement_concept_id,
#'   unit_concept_id
#' FROM
#'   `{CDR}.measurement`
#' INNER JOIN
#'  labs_of_interest USING(measurement_concept_id, unit_concept_id)
#' LEFT JOIN
#'   `{CDR}.measurement_ext` USING(measurement_id)
#' GROUP BY
#'   measurement_concept_id,
#'   measurement_name,
#'   measurement_source,
#'   unit_concept_id,
#'   unit_name
#' ORDER BY
#'   N DESC
#' ')
#' }
aou_sql <- function(query, CDR = getOption("aou.default.cdr"), collect = TRUE, ...) {
  .cdr_objs <- ls(envir = .GlobalEnv, pattern = "^CDR$|^cdr$")
  if (length(.cdr_objs) == 0) {
    CDR <- CDR
    cdr <- CDR
  }

  q <- bigrquery::bq_project_query(
    Sys.getenv("GOOGLE_PROJECT"),
    query = glue::glue(query)
  )
  if (collect) {
    bigrquery::bq_table_download(q, ...)
  }
}
