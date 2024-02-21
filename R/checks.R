# check for connection


#' Checks whether a connection is valid for functions. Internal
#'
#' @param con connection to aou database
#'
#' @return NULL
#' @keywords internal
check_connection <- function(con = getOption("aou.default.con")){
  if (is.null(con)) {
    cli::cli_abort(c("No connection available.",
                     "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
                     "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }
}

#' Checks whether a cohort is valid for functions. Internal. SQL cohorts only
#'
#' @param cohort cohort provided to function
#'
#' @return NULL
#' @keywords internal
#' @export
validate_cohort_sql_df <- function(cohort = NULL, con = getOption("aou.default.con")){

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling enrollment dates for entire All of Us cohort."))
  } else if (!"person_id" %in% colnames(cohort)) {
    # ensure person_id is a column name in cohort
    cli::cli_abort(c("{.code person_id} column not found in cohort.",
                     "i" = "Confirm that the cohort has a column named {.code person_id}"
    ))
  } else if (is.data.frame(cohort)) {
    cohort <- dplyr::tbl(con, "person") %>%
      dplyr::filter(.data$person_id %in% !!unique(cohort$person_id)) %>%
      dplyr::select("person_id")
    return(cohort)
  } else {
    cohort <- cohort %>%
      dplyr::select("person_id")
    return(cohort)
  }

}

#' Checks whether a cohort is valid for functions. Internal. SQL cohorts only
#'
#' @param cohort cohort provided to function
#'
#' @return NULL
#' @keywords internal
#' @export
validate_cohort_sql <- function(cohort = NULL){

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling data for entire All of Us cohort."))
  } else if (!"person_id" %in% colnames(cohort)) {
    # ensure person_id is a column name in cohort
    cli::cli_abort(c("{.code person_id} column not found in cohort.",
                     "i" = "Confirm that the cohort has a column named {.code person_id}"
    ))
  } else if (is.data.frame(cohort)) {
    cli::cli_abort("dataframes not yet supported")
  } else {
    cohort <- cohort %>%
      dplyr::select("person_id")
    return(cohort)
  }

}

