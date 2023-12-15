

#' Function to test all of us package on AllofUs Researcher Workbench using documented examples
aou_test_package <- function() {
  testthat::test_that("all examples run without error",{
    fun_names <- getNamespaceExports("allofus")
    for (fun in fun_names) {
      testthat::expect_no_error({
        invisible(
          capture.output(
            example(fun, package = "allofus", character.only = TRUE, echo = FALSE)
          )
        )
      })
    }
  })
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
#' @noRd
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
    ) %>%
      janitor::clean_names()

    # remove tables that are not in All of Us
    for (tb in DBI::dbListTables(con)[!DBI::dbListTables(con) %in%
                                      unique(tbls$relevant_omop_table)]) {
      DBI::dbRemoveTable(con, tb)
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
    cli::cli_inform(c("v" ="Connected successfully!"))
  } else {
    cli::cli_inform(c("!" = "Error: Unable to connect"))
  }

  options(aou.default.con = con)

  return(con)
}
