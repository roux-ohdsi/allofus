#
#' Creates a temporary table from a local data frame or tibble
#'
#' @param df a local dataframe or tibble
#' @description
#'  Experimental function that builds a local tibble into an SQL query and
#'  generates a temporary table. Tables
#' generally will need to be small in size (<1500 rows). The table will only
#' exist for the current connection session and will need to be created again
#' in a new session.
#' @return a name to a temporary table in the database.
#' @export
#'
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' df = tibble(
#'   category = c("DELIV", "ECT", "AB", "LB", "SB", "SA"),
#'   max_term = as.integer(c(301, 84, 168, 301, 301, 139)),
#'   min_term = as.integer(c(140, 42, 42, 161, 140, 28)),
#'   retry =    as.integer(c(28, 14, 14, 28, 28, 14))
#' )
#' tmp_tbl = aou_create_temp_table(df)
#' tbl(con, tmp_tbl)
#'
#'
aou_create_temp_table <- function(df){

  # VALUES
  add_q = function(s){
    paste0("'", s, "'")
  }

  # change factor to character and add single quotes as needed to character
  df <- df %>% dplyr::mutate(
    dplyr::across(dplyr::where(is.factor), ~as.character(.x)),
    dplyr::across(dplyr::where(is.character), ~stringr::str_replace_all(.x, "\\'", "\\\\'")),
    dplyr::across(dplyr::where(is.character), ~stringr::str_replace_all(.x, '\\"', '\\\\"')),
    dplyr::across(dplyr::where(is.character), ~add_q(.x))
  )

  # column names
  cn = colnames(df)
  # column types
  ct = stringr::str_replace_all(sapply(df, class), c(
    "character" = "STRING",
    "integer" = "INT64",
    "double" = "NUMERIC",
    "factor" = "STRING",
    "Date" = "DATE")
  )
  
  df <- df %>% dplyr::mutate(
    dplyr::across(dplyr::everything(), ~replace_na(as.character(.x), "NULL"))
  )

  # add the data into the table within the
  # create temp table text
  l2 = list()
  for(i in 1:length(cn)){
    l2[[i]] = paste(cn[i], ct[i])
  }
  s1_str = paste(l2, collapse = ",\n")
  s1 = stringr::str_glue('CREATE TEMP TABLE dataset  (\n{s1_str}\n);')
  s2 = stringr::str_glue('INSERT INTO dataset ({paste(cn, collapse =", ")})')

  l = list()
  for(i in 1:nrow(df)){l[[i]] = paste0("(", paste(df[i, ], collapse = ", "), ")")}

  s3 = stringr::str_glue('VALUES{paste(l, collapse = ",\n")};')
  s4 = 'SELECT * FROM dataset'

  # put it all together
  q = paste(
    s1, s2, s3, s4
  )

  # compute the table
  tmptbl_object = bigrquery::bq_project_query(
    Sys.getenv("GOOGLE_PROJECT"),
    query = q
  )
  # get the table name to return to the user
  name = paste(tmptbl_object$project, tmptbl_object$dataset, tmptbl_object$table, sep = ("."))
  return(name)
}


#' Compute a dplyr tbl SQL query into a temp table
#'
#' @param .data result of tbl(con, "table") %>% ... query
#' @param con connection from aou_connect(). Set by default
#'
#' @description Experimental function that computes a temporary table from a tbl(con, "table")
#' dplyr chain that returns an SQL query (e.g., with show_query()). The
#' may be useful to create intermediate tables to reduce long queries.
#' It is a workaround for dplyr::compute(temporary = TRUE) which does not
#' currently work on the workbench. The table will only
#' exist for the current connection session and will need to be created again
#' in a new session.
#'
#' @return  a name to a temporary table in the database.
#' @export
#'
#' @examplesIf on_workbench()
#'
#' con <- aou_connect()
#' tmp_tbl = tbl(con, "concept") %>%
#'    select(concept_id) %>%
#'    head(10) %>%
#'    aou_compute()
#'
#' tbl(con, tmp_tbl)
#'
#'
aou_compute <- function(.data, con = getOption('aou.default.con')){

  # get the query as a character vector
  q = as.character(dbplyr::db_sql_render(con, .data))

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

  return(tbl_name)

}
