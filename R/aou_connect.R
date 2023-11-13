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
aou_connect <- function(CDR = getOption("aou.default.cdr"), ...) {

  out <- tryCatch({

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

    stopifnot("CDR input is incorrect" = connection@dataset == release)
    stopifnot("Unable to establish bigquery connection" = DBI::dbIsValid(connection))

    cat(cli::col_green("Connected successfully!"), "\n")
    options(aou.default.con = connection)

    connection

  },
  error = function(e) {
    cat(cli::col_red("Error: Unable to connect"), "\n")
    return(e)
  }
  )

  return(out)

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
#' @param ... All other arguments passed to `bigrquery::bq_table_download()`
#'
#' @return A dataframe with the results of the query.
#' @export
#'
#' @examples
#' # Examples based on AoU snippets
#' \dontrun{
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
#' ')
#' }
aou_sql <- function(query, CDR = getOption("aou.default.cdr"), ...) {
  .cdr_objs <- ls(envir = .GlobalEnv, pattern = "^CDR$|^cdr$")
  if (length(.cdr_objs) == 0) {
    CDR <- CDR
    cdr <- CDR
  }

  q <- bigrquery::bq_project_query(
    Sys.getenv("GOOGLE_PROJECT"),
    query = glue::glue(query)
  )

  bigrquery::bq_table_download(q, ...)

}



#' Internal function to test allofus package on fake database
#'
#' @param cache logical indicating whether to cache the downloaded data
#' @param cache_dir character string specifying the directory to store cached data
#' @param overwrite logical indicating whether to overwrite existing cached data
#'
#' @return a database connection object
#'
#' @details This function downloads a test OMOP database from the OHDSI Eunomia package, renames tables and fields to lowercase,
#' removes tables that are not relevant to All of Us, adds missing fields to relevant tables, and drops fields that are not relevant to All of Us.
#' It then tests the connection to the database by checking if the person_id column is in the person table.
#'
#' @examples
#' \dontrun{
#' con <- aou_test_connect()
#' }
aou_test_connect <- function(cache = TRUE, cache_dir = Sys.getenv("AOU_CACHE_DIR"),
                             overwrite = FALSE) {
  if (cache_dir == "") {
    cache_dir <- tempdir()
    if (cache) {
      message(
        "No cache directory specified. Using temporary directory: ", cache_dir,
        ". To specify a cache directory, it is recommended to set the ",
        "AOU_CACHE_DIR environment variable."
      )
    }
  }
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir)
  }

  if ((!file.exists(file.path(cache_dir, "aou_test_data.sqlite"))) || overwrite) {
    download.file("https://github.com/OHDSI/Eunomia/raw/main/inst/sqlite/cdm.tar.xz",
                  destfile = file.path(cache_dir, "cdm.tar.xz")
    )
    file <- xzfile(file.path(cache_dir, "cdm.tar.xz"), open = "rb")
    untar(file, exdir = cache_dir)
    close(file)
    file.rename(
      from = file.path(cache_dir, "cdm.sqlite"),
      to = file.path(cache_dir, "aou_test_data.sqlite")
    )

    if (cache) {
      message("Cached test data to ", cache_dir)
    }

    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      file.path(cache_dir, "aou_test_data.sqlite")
    )

    # change table and field names to lowercase
    for (tb in DBI::dbListTables(con)) {
      # have to first make a copy of table
      DBI::dbExecute(con, glue::glue("ALTER TABLE {tb} RENAME TO l_{tolower(tb)}"))
      # then rename lowercase version to original name
      DBI::dbExecute(con, glue::glue("ALTER TABLE l_{tolower(tb)} RENAME TO {tolower(tb)}"))

      for (fld in DBI::dbListFields(con, tolower(tb))) {
        DBI::dbExecute(con, glue::glue("ALTER TABLE {tb} RENAME COLUMN {fld} TO {tolower(fld)}"))
      }
    }

    googlesheets4::gs4_deauth()

    tbls <- googlesheets4::read_sheet("1XLVq84LLd0VZMioF2sPwyiaPw3EFp5c8o1CTWGPH-Yc",
                                      sheet = "OMOP-Compatible Tables"
    ) |>
      janitor::clean_names()

    # remove tables that are not in All of Us
    for (tb in DBI::dbListTables(con)[!DBI::dbListTables(con) %in%
                                      unique(tbls$relevant_omop_table)]) {
      DBI::dbRemoveTable(con, tb)
      # message("Removed ", tb)
    }

    # check to see if each field name is present in the database table
    for (i in seq_len(nrow(tbls))) {
      if (!tbls$relevant_omop_table[i] %in%
          DBI::dbListTables(con)) {
        next
      }
      if (!tbls$field_name[i] %in%
          DBI::dbListFields(con, tbls$relevant_omop_table[i])) {
        field_type <- tbls$field_type[i]
        if (field_type == "bigint") field_type <- "integer"
        # add field to table with the correct type
        # generate some simulated data of that type to go in it
        DBI::dbExecute(
          con,
          paste0(
            "ALTER TABLE ",
            tbls$relevant_omop_table[i],
            " ADD COLUMN ",
            tbls$field_name[i],
            " ",
            field_type
          )
        )
        # message("Added ", tbls$field_name[i], " to ", tbls$relevant_omop_table[i])
      }
    }

    for (omop_table in unique(tbls$relevant_omop_table)) {
      # also remove any fields that aren't in all of us
      if (!omop_table %in% DBI::dbListTables(con)) next
      for (f in DBI::dbListFields(con, omop_table)) {
        if (!f %in% tbls$field_name[tbls$relevant_omop_table == omop_table]) {
          DBI::dbExecute(
            con,
            paste0(
              "ALTER TABLE ",
              omop_table,
              " DROP COLUMN ",
              f
            )
          )
          # message("Dropped ", f, " from ", omop_table)
        }
      }
    }

    # this one is not included on the googlesheet
    DBI::dbExecute(
      con,
      paste0(
        "ALTER TABLE observation",
        " ADD COLUMN value_source_value integer"
      )
    )
  } else {
    con <- DBI::dbConnect(
      RSQLite::SQLite(),
      dbname = file.path(cache_dir, "aou_test_data.sqlite")
    )
  }

  # arbitrary test to see if everything worked
  # is the person_id column in the person table?
  if ("person" %in% DBI::dbListTables(con) && "person_id" %in% DBI::dbListFields(con, "person")) {
    cat(cli::col_green("Connected successfully!"))
  } else {
    cat(cli::col_red("Error: Unable to connect"))
  }

  options(aou.default.con = con)

  return(con)
}





#' List tables in the AoU Database
#'
#' @param con connection to the database
#' @param remove_NA whether to remove tables that are not in the data dictionary
#'
#' @return a dataframe with the table names and the number of columns
#' @export
#'
#' @examples
#' \dontrun{
#' con <- aou_connect()
#' aou_tables()
#' }
aou_tables <- function(con = getOption("aou.default.con"), remove_NA = TRUE) {
  if (is.null(con)) {
    stop("No connection specified. Please specify a connection or run aou_test_connect() to create a connection.")
  }

  tbls <- DBI::dbListTables(con)

  tbls <- tibble(
    table_name = tbls
  ) |>
    dplyr::left_join(allofus::aou_table_info, by = "table_name") %>%
    dplyr::arrange(rowSums(is.na(dplyr::select(., columns, table_name))))

  if(isTRUE(remove_NA)){
    tbls <- tbls %>% drop_na(columns)
  }

  cat(
    cli::col_green('Tables not referenced in the Data Dictionary are omitted. View them by setting remove_NA = FALSE.\n\n')
  )

  cat(
    cli::col_red('Warning:\n The ds_survey table does not include "PMI_SKIP" (903096) responses for all survey questions, use with caution. \n
          The allofus R package authors recommend using the observation table to query complete survey data.\n
          For assistance querying the observation table, see allofus::aou_survey()./n')
  )

  return(tbls)
}
