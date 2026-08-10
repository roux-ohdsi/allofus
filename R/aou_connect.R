# AoU helpers

#' Check to see whether you are on the All of Us workbench
#' @description Use this function to check whether you are on the All of Us
#'   Researcher Workbench. This is useful for writing code that can be used both
#'   on the workbench and locally.
#' @return TRUE if you are on the workbench, FALSE otherwise
#' @export
#' @keywords internal
#' @examples
#' on_workbench()
on_workbench <- function() {
  Sys.getenv("WORKSPACE_CDR") != ""
}

#' Create a connection to the database in All of Us
#'
#' @description Connects to the All of Us database and returns a
#'   BigQueryConnection object. You can reference this object to query the
#'   database using R and or SQL code. A message is printed with the connection
#'   status (successful or not).
#' @details You can reference this object to connect to the All of Us database
#'   and run SQL code using, e.g., `dbplyr` or `DBI`. A message is printed with
#'   the connection status (successful or not). For RStudio users, setting quiet = TRUE
#'   will silence most (but not all) billing messages.
#' @param CDR The name of the "curated data repository" to connect to. Defaults
#'   to `getOption("aou.default.cdr")`, which is `Sys.getenv('WORKSPACE_CDR')`
#'   if not specified otherwise (i.e., the "mainline" CDR). On the controlled
#'   tier, specify the "base" CDR with `CDR =
#'   paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @param ... Further arguments passed along to `DBI::dbConnect()`.
#' @return A `BigQueryConnection` object. This object is also saved as an option
#'   (`getOption("aou.default.con")`).
#' @export
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' # reference the observation table in the database
#' dplyr::tbl(con, "observation")
#' # print a list of the tables in the database
#' DBI::dbListTables(con)
aou_connect <- function(CDR = getOption("aou.default.cdr"), ...) {
  if (packageVersion("dbplyr") == "2.4.0") {
    cli::cli_abort(c(
      "dbplyr v2.4.0 is not compatible with the All of Us database (bigquery).;",
      i = "Please install either dbplyr v2.3.4 or the development version of dbplyr:",
      "# Install pak",
      'install.packages("pak")',
      "# Install dbplyr v2.3.4",
      'pak::pkg_install("tidyverse/dbplyr@v2.3.4")',
      "# Or install development version of dbplyr",
      'pak::pkg_install("tidyverse/dbplyr")',
      "# restart your R kernel"
    ), call = NULL)
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
          stop(
            sprintf(
            "BigQuery connection dataset mismatch. Expected dataset '%s' but connection has dataset '%s'. Data project: '%s'. Billing project: '%s'. CDR input: '%s'.",
            release,
            as.character(connection@dataset),
            prefix,
            Sys.getenv("GOOGLE_PROJECT"),
            CDR
            ),
            call. = FALSE
        )
      }

      # also let it fail if there's no person_table
      test_table <- dplyr::tbl(connection, "person")

      cli::cli_inform(c("v" = "Connected successfully!"))
      options(aou.default.con = connection)

      connection
    },
    error = function(e) {
      cli::cli_abort(
      c(
        "Unable to connect to CDR {CDR}",
        "Caused by: {conditionMessage(e)}"
      ), call = NULL)
      return(e)
    }
  )

  return(out)
}

#' Execute a SQL query on the All of Us database
#' @description Executes an SQL query on the All of Us database
#'
#' @param query A SQL query (BigQuery dialect) to be executed. Interpreted with
#'   `glue::glue()`, so expressions enclosed with braces will be evaluated.
#'   References to `"{CDR}"` or `"{cdr}"` will be evaluated automatically (see
#'   examples).
#' @param collect Whether to bring the resulting table into local memory
#'   (`collect = TRUE`) as a dataframe or leave as a reference to a database table (for
#'   continued analysis using, e.g., `dbplyr`). Defaults to `FALSE.`
#' @param debug Print the query to the console; useful for debugging.
#' @param CDR The name of the "curated data repository" that will be used in any
#'   references of the form `"{CDR}"` or `"{cdr}"` in the query (see examples).
#'   Defaults to `getOption("aou.default.cdr")`, which is
#'   `Sys.getenv('WORKSPACE_CDR')` if not specified otherwise (i.e., the
#'   "mainline" CDR). On the controlled tier, specify the "base" CDR with `CDR =
#'   paste0(Sys.getenv('WORKSPACE_CDR'), "_base")`.
#' @param ... All other arguments passed to `bigrquery::bq_table_download()` if
#'   `collect = TRUE`.
#' @param con Connection to the allofus SQL database. Defaults to `getOption("aou.default.con")`,
#' which is created automatically with `aou_connect()`. Only needed if `collect = FALSE`.
#' @return A dataframe if `collect = TRUE`; a reference to a remote database table if not.
#' @export
#'
#' @examplesIf on_workbench()
#'
#' con <- aou_connect()
#'
#' # Examples based on AoU snippets
#' aou_sql("
#'   -- Compute the count of unique participants in our All of Us cohort.
#'   SELECT
#'   COUNT(DISTINCT person_id) AS total_number_of_participants
#'   FROM
#'   `{CDR}.person`
#' ", collect = TRUE)
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
#' ', collect = TRUE)
aou_sql <- function(query, collect = FALSE, debug = FALSE, ..., con = getOption("aou.default.con"), CDR = getOption("aou.default.cdr")) {
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

  # The GOOGLE_PROJECT requirement only applies to the BigQuery backend. With a
  # non-BigQuery connection (e.g. a local DuckDB database), the query runs
  # through DBI in get_query_table(), so skip the Workbench guard. Note that
  # user-supplied raw SQL must be portable to run locally (avoid BigQuery-only
  # constructs such as COUNTIF, REGEXP_CONTAINS, APPROX_QUANTILES).
  if (Sys.getenv("GOOGLE_PROJECT") == "" &&
    (is.null(con) || inherits(con, "BigQueryConnection"))) {
    cli::cli_abort(c('This function only works on the Researcher Workbench. Please ensure you have a valid Google Cloud project set up by checking {.code Sys.getenv("GOOGLE_PROJECT")}.'),
      call = NULL
    )
  }

  if (is.null(con)) {
    cli::cli_abort(c('No connection detected. Please run `allofus::aou_connect()`.'),
                   call = NULL
    )
  }

  res <- tryCatch(
    {
      get_query_table(glue::glue(query), collect = collect, con = con)
    },
    error = function(e) {
      cli::cli_abort(
        c(
          "SQL query did not result in a table. Please check to make sure SQL code is valid.",
          "To print the query, run {.code aou_sql(query, debug = TRUE)}"
        ),
        call = NULL
      )
      return(e)
    }
  )

  res
}

#' Helper function to get result of a query
#' @param q query
#' @param collect Whether to bring the resulting table into local memory
#'   (`collect = TRUE`) as a dataframe or leave as a reference to a database
#'   table (for continued analysis using, e.g., `dbplyr`). Defaults to `FALSE.`
#' @param ... Other arguments passed to bigrquery::bq_table_download
#' @param con Connection to the allofus SQL database. Defaults to
#'   `getOption("aou.default.con")`, which is created automatically with
#'   `aou_connect()`.
#' @keywords internal
#' @noRd

get_query_table <- function(q, collect = FALSE, ..., con = getOption("aou.default.con")) {
  if (is.null(con) & isFALSE(collect)) {
    cli::cli_abort(c("No connection available.",
      "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
      "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }

  # Backend-aware execution. On the All of Us Researcher Workbench the
  # connection is a BigQueryConnection and queries run through bigrquery. For
  # any other DBI backend (e.g. a local DuckDB database such as the one created
  # by the mockallofus package), execute the query through DBI instead, so the
  # same allofus code can run locally for development. See get_query_table_local().
  if (!is.null(con) && !inherits(con, "BigQueryConnection")) {
    return(get_query_table_local(q, collect = collect, con = con))
  }

  tbl_obj <- bigrquery::bq_project_query(
    Sys.getenv("GOOGLE_PROJECT"),
    query = q, temporary = TRUE
  )

  if (isTRUE(collect)) {
    return(bigrquery::bq_table_download(tbl_obj, ...))
  }

  # get the table name to return for future reference.
  tbl_name <- paste(tbl_obj$project, tbl_obj$dataset, tbl_obj$table, sep = ("."))

  # to deal with display error when printing the output in jupyter
  res <- dplyr::tbl(con, tbl_name) %>% dplyr::filter(1 > 0)

  res
}

#' Split a SQL string into statements on `;`, ignoring `;` inside string literals
#'
#' A naive `strsplit(q, ";")` breaks statements whose string literals contain a
#' semicolon (e.g. medical concept names like "Alpha-fetoprotein (AFP); serum"
#' inserted by `aou_create_temp_table()`). This walks the string, tracking
#' single-quoted literals (with `''` treated as an escaped quote), and only
#' splits at top-level semicolons. Falls back to a fast split when there are no
#' string literals (the common case for internally-generated SQL).
#' @keywords internal
#' @noRd
split_sql_statements <- function(q) {
  if (!grepl("'", q, fixed = TRUE)) {
    return(strsplit(q, ";", fixed = TRUE)[[1]])
  }
  chars <- strsplit(q, "", fixed = TRUE)[[1]]
  n <- length(chars)
  stmts <- character(0)
  start <- 1L
  in_str <- FALSE
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (in_str) {
      if (ch == "'") {
        if (i < n && chars[i + 1L] == "'") {
          i <- i + 2L # escaped quote ('')
          next
        }
        in_str <- FALSE
      }
    } else if (ch == "'") {
      in_str <- TRUE
    } else if (ch == ";") {
      stmts <- c(stmts, paste0(chars[start:(i - 1L)], collapse = ""))
      start <- i + 1L
    }
    i <- i + 1L
  }
  if (start <= n) stmts <- c(stmts, paste0(chars[start:n], collapse = ""))
  stmts
}

#' Execute a query on a non-BigQuery (local DBI) backend
#'
#' Runs the SQL that allofus would otherwise send to BigQuery on a generic DBI
#' connection (e.g. local DuckDB), applying the small set of dialect adjustments
#' needed for the BigQuery-flavored SQL allofus emits: stripping BigQuery
#' backtick identifier quoting and mapping the `FLOAT64` type name to `DOUBLE`.
#' Handles the multi-statement `CREATE TEMP TABLE ...; SELECT * FROM ...` form
#' produced by `aou_compute()` / `aou_observation_period()` / `aou_create_temp_table()`.
#' @keywords internal
#' @noRd
get_query_table_local <- function(q, collect = FALSE, con = NULL) {
  q <- gsub("`", "", q) # BigQuery backtick identifiers -> bare (valid in DuckDB)
  q <- gsub("\\bFLOAT64\\b", "DOUBLE", q) # BigQuery type name -> DuckDB

  stmts <- trimws(split_sql_statements(q))
  stmts <- stmts[nzchar(stmts)]
  last <- stmts[length(stmts)]
  prelim <- if (length(stmts) > 1) stmts[-length(stmts)] else character(0)

  unique_name <- function() paste0("aou_tmp_", paste(sample(c(letters, 0:9), 12, TRUE), collapse = ""))
  select_star_from <- function(x) {
    regmatches(x, regexec("(?i)^SELECT\\s+\\*\\s+FROM\\s+\"?([A-Za-z0-9_.]+)\"?\\s*$", x, perl = TRUE))[[1]]
  }

  # Special-case the script wrap emitted by aou_compute() and
  # aou_observation_period():  CREATE TEMP TABLE <name> AS <inner> ; SELECT * FROM <name>
  # Re-materialize <inner> under a fresh unique name and return a reference to
  # it. This mirrors BigQuery, where each query yields a new temporary table, so
  # chained calls (e.g. inside aou_survey()) don't collide on a reused fixed
  # name such as "table1".
  if (length(stmts) == 2) {
    cm <- regmatches(prelim[1], regexec(
      "(?is)^CREATE\\s+(?:TEMP(?:ORARY)?\\s+)?TABLE\\s+\"?([A-Za-z0-9_.]+)\"?\\s+AS\\s+(.*)$",
      prelim[1], perl = TRUE
    ))[[1]]
    lm <- select_star_from(last)
    if (length(cm) == 3 && length(lm) == 2 &&
      identical(gsub('"', "", cm[2]), gsub('"', "", lm[2]))) {
      inner <- cm[3]
      if (isTRUE(collect)) {
        return(DBI::dbGetQuery(con, inner))
      }
      nm <- unique_name()
      DBI::dbExecute(con, paste0("CREATE TEMP TABLE ", nm, " AS ", inner))
      return(dplyr::tbl(con, nm) %>% dplyr::filter(1 > 0))
    }
  }

  # General multi-statement path (e.g. aou_create_temp_table's CREATE/INSERT/SELECT,
  # which uses freshly randomized table names). Drop-if-exists before each CREATE
  # for idempotency, run the statements, then reference the final result.
  for (s in prelim) {
    nm <- regmatches(s, regexec(
      "(?i)\\bCREATE\\s+(?:TEMP(?:ORARY)?\\s+)?TABLE\\s+\"?([A-Za-z0-9_.]+)\"?",
      s, perl = TRUE
    ))[[1]]
    if (length(nm) == 2) {
      DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", nm[2]))
    }
    DBI::dbExecute(con, s)
  }

  if (isTRUE(collect)) {
    return(DBI::dbGetQuery(con, last))
  }
  lm <- select_star_from(last)
  if (length(lm) == 2) {
    return(dplyr::tbl(con, gsub('"', "", lm[2])) %>% dplyr::filter(1 > 0))
  }
  nm <- unique_name()
  DBI::dbExecute(con, paste0("CREATE TEMP TABLE ", nm, " AS ", last))
  dplyr::tbl(con, nm) %>% dplyr::filter(1 > 0)
}


#' List tables in the AoU Database
#'
#' @description Prints a list of all of the tables in the All of Us Big Query
#'   Database.
#'
#' @param remove_na Whether to remove tables that are not in the data
#'   dictionary. Defaults to `TRUE`
#' @param ... Not currently used
#' @param con Connection to the allofus SQL database. Defaults to
#'   `getOption("aou.default.con")`, which is created automatically with
#'   `aou_connect()`.
#'
#' @return A dataframe with the table names and the number of columns
#' @export
#'
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' aou_tables()
#'
aou_tables <- function(remove_na = TRUE, ..., con = getOption("aou.default.con")) {
  if (is.null(con)) {
    cli::cli_abort("No connection specified. Please specify a connection or run {.code aou_test_connect}() to create a connection.")
  }

  tbls <- DBI::dbListTables(con)

  tbls <- tibble::tibble(table_name = tbls) %>%
    dplyr::left_join(allofus::aou_table_info, by = "table_name") %>%
    dplyr::select("table_name", "columns") %>%
    dplyr::arrange(.data$columns)

  if (isTRUE(remove_na)) {
    tbls <- tbls %>% tidyr::drop_na("columns")
  }

  cli::cli_inform(c("i" = "Tables not referenced in the Data Dictionary are omitted. View them by setting {.code remove_na = FALSE}."))

  cli::cli_inform(c("!" = 'Warning: The ds_survey table does not include "PMI_SKIP" (903096) responses for all survey questions. Use with caution.
          The {.pkg allofus} R package authors recommend using the observation table to query complete survey data.
          For assistance querying the observation table, see {.code allofus::aou_survey()}.'))

  return(tbls)
}
