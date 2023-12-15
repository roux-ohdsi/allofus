#' Join current query to another table
#'
#' @description Simple wrapper for join functions to join an existing query
#' to another table in the All of Us database.
#'
#' @details
#'
#' There are a few good reasons to use aou_join() when possible over the x_join functions from dplyr.
#' First, it reduces the code necessary to join an existing table to another table. Second,
#' it includes checks/workarounds for two sources of common errors using dbplyr:
#' it automatically appends the x_as and y_as arguments to the join call if they are not provided and
#' it changes the default suffix from .x/.y to _x/_y for cases with shared column names not specified by
#' the `by` argument which will result in a SQL error.
#'
#' @param data unexecuted SQL query from dbplyr/dplyr.
#' @param table the omop table (or other remote table in your schema) you wish to join, as a character string, or a tbl object.
#' @param type the type of join. use types available in dplyr: left, right, inner, anti, full etc.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param ... arguments passed on to the join function. e.g., by = "person_id"
#' @param by columns to join on
#' @param suffix suffix preferences to add when joining data with the same column names not specified in the by argument.
#' @param x_as optional; a string for the name of the left table
#' @param y_as optional; a string for the name of the right table
#'
#' @return Continued dbplyr query
#' @export
#' @importFrom dplyr left_join right_join inner_join anti_join full_join semi_join
#' @md
#'
#' @examplesIf on_workbench()
#'
#' con <- aou_connect()
#' obs_tbl %>%
#'   aou_join("person", type = "left", by = "person_id")
#'
aou_join <- function(data,
                     table,
                     type,
                     by,
                     suffix = c("_x", "_y"),
                     con = getOption("aou.default.con"),
                     x_as = NULL,
                     y_as = NULL,
                     ...) {
  if (is.null(con)) cli::cli_abort(c("No connection available.",
                                     "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
                                     "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."))

  if (is.character(table)) y_table <- dplyr::tbl(con, table) else y_table <- table
  shared_columns <- intersect(colnames(data), colnames(y_table))

  # convert to the new join type
  by <- dplyr::join_by(by)

 res <- get(paste(type, "join", sep = "_"))(data, y_table,
    x_as = if (missing(x_as)) {
      paste(sample(letters, 10, TRUE), collapse = "")
    } else {
      x_as
    },
    y_as = if (missing(y_as)) {
      paste(sample(letters, 10, TRUE), collapse = "")
    } else {
      y_as
    },
    by = by,
    suffix = suffix,
    ...)

 if (any(!paste0(names(data), "_x") %in% names(res)) || any(!paste0(names(y_table), "_y") %in% names(res))) {
  # if (length(shared_columns) > 0 && !all(sort(shared_columns) %in% sort(c(by$x, by$y))) && all(suffix == c("_x", "_y"))) {
     cli::cli_warn(c("There are shared column names not specified in the {.code by} argument.",
                     ">" = "These column names now end in '_x' and '_y'.",
                     "i" = "You can change these suffixes using the {.code suffix} argument but it cannot contain periods (`.`).",
                     ">" = "Consider specifing all shared columns in the {.code by} argument.",
                     ">" = "Or if these additional shared columns are `NA`, remove them prior to joining."))
 }

 return(res)


}
