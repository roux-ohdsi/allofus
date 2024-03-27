
#' Create a custom tbl object from a data source
#'
#' `r lifecycle::badge('experimental')`
#' @param src The data source to create the tbl object from.
#' @param from The table to select from the data source.
#' @param ... Additional arguments to be passed to the dplyr::tbl function.
#'
#' @return A reference to a remote table with class "tbl_aou".
#'
#'
#' @examples
#' # Create a tbl object from a data source
#' con <- aou_connect()
#' person_table <- tbl(con, "person")
#'
tbl.src_aou <- function(src, from, ...) {

  tbl_aou <- dplyr::tbl(src, from, ...)
  class(tbl_aou) <- c("tbl_aou", class(tbl_aou))

  tbl_aou
}

#' Compute function for tbl_aou objects
#'
#' This function takes a tbl_aou object and computes the SQL query associated with it.
#' It returns a tbl.src_aou object representing the computed table.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param x The tbl_aou object to compute.
#' @param con The database connection to use. Defaults to the value of 'aou.default.con' option.
#'
#' @return A tbl.src_aou object representing the computed table.
#'
compute.tbl_aou <- function(x, con = getOption('aou.default.con')) {

  if (is.null(con)) {
      cli::cli_abort(c("No connection available.", i = "Provide a connection automatically by running {.code aou_connect()} before this function.",
      i = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."))
  }

  # get the query as a character vector
  q = as.character(dbplyr::db_sql_render(con, x))

  # add the create temp table text before and after
  tmp1 = 'CREATE TEMP TABLE table1 AS'
  tmp2 = 'SELECT * FROM table1'

  # combine the three vars
  out = paste0(tmp1, "\n", q, ";\n", tmp2)

  # adjustment to the table references to add {CDR}. in front
  # of the tables in the dplyr query so that the table
  # references point to the right place.
  # replaces any references to current tables from dbListTables
  without_cdr = paste0("`",DBI::dbListTables(con), "`")
  with_cdr = paste("`{CDR}",DBI::dbListTables(con),"`", sep = ".")
  names(with_cdr) = without_cdr
  out = stringr::str_replace_all(out, pattern = with_cdr)
  CDR = getOption("aou.default.cdr")
  out = stringr::str_glue(out)

  # execute the query
  tbl_obj = bigrquery::bq_project_query(
    Sys.getenv("GOOGLE_PROJECT"),
    query = out, temporary = TRUE
  )
  # get the tbale name to return for future reference.
  tbl_name = paste(tbl_obj$project, tbl_obj$dataset, tbl_obj$table, sep = ("."))

  return(tbl.src_aou(con, tbl_name))

}

#' Collects the data from a tbl_aou object
#'
#' This function collects the data from a tbl_aou object and converts it to a regular data frame.
#' Finally, it converts any columns of type `integer64` to `double` for ease of use.
#'
#' `r lifecycle::badge('experimental')`
#'
#' @param tbl_obj The tbl_aou object to collect data from
#' @param ... Additional arguments to be passed to the `collect` function
#'
#' @return A data frame with the collected data
#'
collect.tbl_aou <- function(tbl_obj, convert_int64 = TRUE, ...) {
  # use the default collect method
  class(tbl_obj) <- class(tbl_obj)[!class(tbl_obj) == "tbl_aou"]
  collected <- dplyr::collect(tbl_obj, ...)
  # and then convert to double
  if (convert_int64) {
    return(dplyr::mutate(collected, dplyr::across(dplyr::where(bit64::is.integer64), as.double)))
    cli::cli_inform(i = "{.code integer64} columns were converted to {.code double}.")
  } else {
    cli::cli_inform(i = "{.code integer64} columns were not converted.")
    return(collected)
  }
}

