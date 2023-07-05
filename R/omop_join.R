#' Join current query to another omop table
#'
#' @description Simple wrapper for join functions to join an existing query
#' to another table in the omop cdm (or any other of the same source).
#'
#' @details
#' Include the following line at the top of your script after setting the connection
#' where con refers to the connection object in R.
#' options(con.default.value = con)
#'
#'
#' There are a few good reasons to use omop_join() when possible over the x_join functions from dplyr.
#' First, it reduces the code necessary to join an existing table to another table. Second,
#' it includes checks/workarounds for two sources of common errors using dplyr with DatabaseConnector:
#' it automatically appends the x_as and y_as arguments to the join call if they are not provided and
#' it changes the default suffix from .x/.y to _x/_y for cases with shared column names not specified by
#' the `by` argument which will result in a sql error.
#'
#' @param data sql query from dbplyr/dplyr.
#' @param table the omop table (or other table in your schema) you wish to join
#' @param type the type of join. use types available in dplyr: left, right, inner, anti, full etc.
#' @param con defaults to the connection you set with options()
#' @param ... arguments passed on to the join function. e.g., by = "person_id"
#' @param x_as optional; a string for the name of the left table
#' @param y_as optional; a string for the name of the right table
#'
#' @return Continued dplyr query
#' @export
#' @md
#'
#' @examples
#' allofus::aou_connect()
#' options(con.default.value = con)
#' obs_tbl |>
#'   omop_join("person", type = "left", by = "person_id")
#'
omop_join <- function(data,
                      table,
                      type,
                      by,
                      suffix = c("_x", "_y"),
                      con = getOption("con.default.value"),
                      #schema = NULL,
                      x_as = NULL,
                      y_as = NULL,
                      ...){

  if (is.null(con)) stop("Provide `con` as an argument or default with `options(con.default.value = ...)`")

  # allow for use in AoU
  # first argument assigns the schema as a default, or NULL if not set
  # second if adds a period to the schema
  # if (is.null(schema)) schema <- getOption("schema.default.value")
  # if (!is.null(schema)) {
  #   schema <- paste0(schema, ".")
  # } else if (class(con) != "BigQueryConnection"){
  #   # if(grepl("redshift", con@dbms)) {
  #   stop("Missing schema. Either provide a schema or set options(schema.default.value = cdm_schema).")
  # }


  y_table = tbl(con, table) #paste0(schema, table))
  shared_columns = intersect(colnames(data), colnames(y_table))

  # stop("Provide `schema` as an argument or default with `options(schema.default.value = ...)`")

  if(!all(sort(shared_columns) == sort(by)) & all(suffix == c("_x", "_y"))){
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
