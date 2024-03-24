#
#' Creates a temporary table from a local data frame or tibble
#'
#' @param df a local dataframe or tibble
#' @param nchar_batch approximate number of characters per sql query
#' @param con connection
#' @description
#'  Experimental function that builds a local tibble into an SQL query and
#'  generates a temporary table. Tables
#' generally will need to be small in size (<1500 rows). The table will only
#' exist for the current connection session and will need to be created again
#' in a new session.
#' @return a reference to a temporary table in the database with the data from `df`
#' @export
#'
#' @examplesIf on_workbench()
#' con <- aou_connect()
#' df <- data.frame(concept_id = c(439331, 4290245, 42535816, 46269813, 
#'                  2784565, 45765502, 434112, 4128031, 435640, 45876808), 
#'                  category = c("AB", "DELIV", "DELIV", "SA", "DELIV", 
#'                  "LB", "DELIV", "DELIV", "PREG", "SA"), 
#'                  gest_value = c(NA, NA, NA, NA, NA, NA, NA, NA, 25, NA))
#' tmp_tbl = aou_create_temp_table(df)
#' 
#'
#'
aou_create_temp_table <- function(df, nchar_batch = 1000000, con = getOption("aou.default.con")) {
    if (is.null(con)) {
        cli::cli_abort(c("No connection available.", i = "Provide a connection automatically by running {.code aou_connect()} before this function.", 
                         i = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."))
    }
    add_q = function(s) {
        paste0("'", s, "'")
    }
    add_date <- function(d) {
        paste0("DATE '", as.character(d), "'")
    }
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character), 
                               dplyr::across(dplyr::where(is.character), 
                                             ~stringr::str_replace_all(.x, "\\'", "\\\\'")), 
                               dplyr::across(dplyr::where(is.character), 
                                             ~stringr::str_replace_all(.x, "\\\"", "\\\\\"")), 
                               dplyr::across(dplyr::where(is.character), add_q))
    cn = colnames(df)
    ct = stringr::str_replace_all(sapply(df, class), c(character = "STRING", 
                                                       integer64 = "INT64",
                                                       integer = "INT64", 
                                                       double = "FLOAT64", 
                                                       numeric = "FLOAT64",
                                                       factor = "STRING", 
                                                       Date = "DATE"))
    df <- df %>% dplyr::mutate(dplyr::across(dplyr::where(is.Date), ~ifelse(is.na(.x), "NULL", add_date(.x))),
                               dplyr::across(dplyr::everything(), 
                                             ~replace_na(as.character(.x), "NULL")))
    
    l2 = purrr::map2_chr(cn, ct, paste)
    s1_str = paste(l2, collapse = ",\n")

    l = map_chr(transpose(df), ~paste0("(", paste(.x, collapse = ", "), ")"))

    values <- split(l, ceiling(cumsum(map_dbl(l, nchar)) / nchar_batch))
    batches <- map_chr(values, ~stringr::str_glue("VALUES{paste(.x, collapse = \",\n\")};"))

    datasets <- do.call(paste0, replicate(10, sample(LETTERS, length(batches), TRUE), FALSE))
    n <- list()
    
    for (i in seq_along(batches)) {
        dataset <- datasets[i]
        s1 = stringr::str_glue("CREATE TEMP TABLE {dataset} (\n{s1_str}\n);")
        s2 = stringr::str_glue("INSERT INTO {dataset} ({paste(cn, collapse =\", \")})")
        s3 = batches[i]
        s4 = stringr::str_glue("SELECT * FROM {dataset};")
        q = paste(s1, s2, s3, s4)
        tmptbl_object = bigrquery::bq_project_query(Sys.getenv("GOOGLE_PROJECT"),
                                                    query = q)
        n[[i]] <- dplyr::tbl(con, paste(tmptbl_object$project, tmptbl_object$dataset, 
                        tmptbl_object$table, sep = (".")))
    }
    
    final_tbl <- reduce(n, union_all)
    
    return(final_tbl)
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

  return(dplyr::tbl(con, tbl_name))

}
