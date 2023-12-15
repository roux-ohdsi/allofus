# AoU helpers

#' Check to see whether you are on the All of Us workbench
#' @description Use this function to check whether you are on the
#' All of Us Researcher Workbench. This is useful for writing code that
#' can be used both on the workbench and locally.
#' @return TRUE if you are on the workbench, FALSE otherwise
#' @export
#' @keywords internal
#' @examples
#' on_workbench()
on_workbench <- function() {
  Sys.getenv("WORKSPACE_CDR") != "" && Sys.getenv("WORKSPACE_BUCKET") != ""
}

#' Create a connection to the database in All of Us
#'
#'
#' @description Use this function to create a `BigQueryConnection` object.
#' You can reference this object to connect to the All of Us database and run
#' SQL code using, e.g., `dbplyr` or `DBI`. A message is printed with the connection
#' status (successful or not).
#' @param CDR The name of the "curated data repository" to connect to. Defaults to
#' `getOption("aou.default.cdr")`, which is `Sys.getenv('WORKSPACE_CDR')` if not specified otherwise
#' (i.e., [the "mainline" CDR](https://support.researchallofus.org/hc/en-us/articles/4616869437204-Controlled-CDR-Directory)).
#' On the controlled tier, specify the "base" CDR with `CDR = paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @param ... Further arguments passed along to `DBI::dbConnect()`.
#'
#' @return A `BigQueryConnection` object. This object is also saved as an option (`getOption("aou.default.con")`).
#' @export
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' # reference the observation table in the database
#' dplyr::tbl(con, "observation")
#' # print a list of the tables in the database
#' DBI::dbListTables(con)

aou_connect <- function(CDR = getOption("aou.default.cdr"), ...) {

  if (packageVersion("dbplyr") == '2.4.0') {
    stop('
         dbplyr v2.4.0 is not compatible with the AllofUs database (bigquery).
         Please install either dbplyr v2.3.4 or the development version of dbplyr.
         # Install {pak}
         install.packages("pak")
         # Install dbplyr v2.3.4
         pak::pkg_install("tidyverse/dbplyr@v2.3.4")
         # Or install development version of dbplyr
         pak::pkg_install("tidyverse/dbplyr")
         # restart your R kernel')
  }


  out <- tryCatch(
    {
      dataset <- strsplit(CDR, split = "\\.")[[1]]
      release <- dataset[2]
      prefix <- dataset[1]

      connection <- DBI::dbConnect(
        bigrquery::bigquery(),
        billing = Sys.getenv("GOOGLE_PROJECT"),
        project = prefix,
        dataset = release,
        bigint = "integer64", # fix for big integers
        ...
      )

      if (is.na(connection@dataset) | connection@dataset != release) {
        stop()
      }


      cli::cli_inform(c("v" = "Connected successfully!"))
      options(aou.default.con = connection)

      connection
    },
    error = function(e) {
      cli::cli_abort(c("Unable to connect to CDR {CDR}"), call = NULL)
      return(e)
    }
  )

  return(out)
}

#' Execute a SQL query on the All of Us database
#'
#' @param query A SQL query (BigQuery dialect) to be executed. Interpreted
#' with `glue::glue()`, so expressions enclosed with braces will be evaluated.
#' References to `"{CDR}"` or `"{cdr}"` will be evaluated automatically (see examples).
#' @param CDR The name of the "curated data repository" that will be used in any
#' references of the form `"{CDR}"` or `"{cdr}"` in the query (see examples). Defaults to
#' `getOption("aou.default.cdr")`, which is `Sys.getenv('WORKSPACE_CDR')` if not specified otherwise
#' (i.e., [the "mainline" CDR](https://support.researchallofus.org/hc/en-us/articles/4616869437204-Controlled-CDR-Directory)).
#' On the controlled tier, specify the "base" CDR with `CDR = paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @param debug Print the query to the console; useful for debugging.
#' @param ... All other arguments passed to `bigrquery::bq_table_download()`
#'
#' @return A dataframe with the results of the query.
#' @export
#'
#' @examplesIf on_workbench()
#' # Examples based on AoU snippets
#' aou_sql("
#'   -- Compute the count of unique participants in our All of Us cohort.
#'   SELECT
#'   COUNT(DISTINCT person_id) AS total_number_of_participants
#'   FROM
#'   `{CDR}.person`
#' ")
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
#' ', debug = TRUE)
aou_sql <- function(query, CDR = getOption("aou.default.cdr"), debug = FALSE, ...) {

  .cdr_objs <- ls(envir = .GlobalEnv, pattern = "^CDR$|^cdr$")
  if (length(.cdr_objs) == 0) {
    CDR <- CDR
    cdr <- CDR
  }

  if (debug) {
    cli::cli_h1("SQL QUERY")
    cli::cat_line(glue::glue(query))
    cli::cli_h1("END SQL QUERY")
  }

  if (Sys.getenv("GOOGLE_PROJECT") == "") {
    cli::cli_abort(c('This function only works on the Researcher Workbench. Please ensure you have a valid Google Cloud project set up by checking {.code Sys.getenv("GOOGLE_PROJECT")}.'),
                   call = NULL)
  }

  res <- tryCatch({
    q <- bigrquery::bq_project_query(
      Sys.getenv("GOOGLE_PROJECT"),
      query = glue::glue(query)
    )

    bigrquery::bq_table_download(q, ...)
  },
  error = function(e) {
    cli::cli_abort(c("SQL query did not result in a table. Please check to make sure SQL code is valid.",
                     "To print the query, run {.code aou_sql(query, debug = TRUE)}"),
                   call = NULL)
    return(e)
  })

  res
}


#' List tables in the AoU Database
#'
#' @param con Connection to the allofus SQL database. Defaults to `getOption("aou.default.con")`,
#' which is created automatically with `aou_connect()`
#' @param remove_na Whether to remove tables that are not in the data dictionary. Defaults to `TRUE`
#'
#' @return A dataframe with the table names and the number of columns
#' @export
#'
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' aou_tables()
#'
aou_tables <- function(con = getOption("aou.default.con"), remove_na = TRUE) {
  if (is.null(con)) {
    cli::cli_abort("No connection specified. Please specify a connection or run {.code aou_test_connect}() to create a connection.")
  }

  tbls <- DBI::dbListTables(con)

  tbls <- tibble::tibble(table_name = tbls) %>%
    dplyr::left_join(allofus::aou_table_info, by = "table_name") %>%
    dplyr::select('table_name', 'columns') %>%
    dplyr::arrange(.data$columns)

  if (isTRUE(remove_na)) {
    tbls <- tbls %>% tidyr::drop_na('columns')
  }

  cli::cli_inform(c("i" = "Tables not referenced in the Data Dictionary are omitted. View them by setting {.code remove_na = FALSE}."))

  cli::cli_inform(c("!" = 'Warning: The ds_survey table does not include "PMI_SKIP" (903096) responses for all survey questions. Use with caution.
          The {.pkg allofus} R package authors recommend using the observation table to query complete survey data.
          For assistance querying the observation table, see {.code allofus::aou_survey()}.'))

  return(tbls)
}
