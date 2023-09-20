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
#' @param table the omop table (or other remote table in your schema) you wish to join
#' @param type the type of join. use types available in dplyr: left, right, inner, anti, full etc.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param ... arguments passed on to the join function. e.g., by = "person_id"
#' @param x_as optional; a string for the name of the left table
#' @param y_as optional; a string for the name of the right table
#'
#' @return Continued dbplyr query
#' @export
#' @md
#'
#' @examples
#' allofus::aou_connect()
#' options(aou.default.con = con)
#' obs_tbl |>
#'   omop_join("person", type = "left", by = "person_id")
#'
aou_join <- function(data,
                      table,
                      type,
                      by,
                      suffix = c("_x", "_y"),
                      con = getOption("aou.default.con"),
                      #schema = NULL,
                      x_as = NULL,
                      y_as = NULL,
                      ...){

  if (is.null(con)) stop("Have you set up the connection? Provide `con` as an argument or default with `options(aou.default.con = ...)`")

  y_table = tbl(con, table)
  shared_columns = intersect(colnames(data), colnames(y_table))

  # stop("Provide `schema` as an argument or default with `options(schema.default.value = ...)`")

  # convert to the new join type
  by <- dplyr:::as_join_by(by)
  if(length(shared_columns) > 0 & !all(sort(shared_columns) %in% sort(c(by$x, by$y))) & all(suffix == c("_x", "_y"))){
    w1 = "There are shared column names not specified in the `by` argument."
    w2 = "These column names now include `_x` and `_y`."
    w3 = "You can change this suffix using the `suffix` argument but it cannot contain periods (`.`)."
    w4 = "Consider specifing all shared columns in the `by` argument."
    w5 = "Or if these additional shared columns are `NA`, remove them prior to joining."
    warning(paste(w1, w2, w3, w4, w5, sep = "\n  "))
  }

  get(paste(type, "join", sep = "_"))(data, y_table,
                                      x_as = if (missing(x_as)) {paste(sample(letters, 10, TRUE), collapse = "")} else {x_as},
                                      y_as = if (missing(y_as)) {paste(sample(letters, 10, TRUE), collapse = "")} else {y_as},
                                      by = by,
                                      suffix = suffix,
                                      ...)


}
