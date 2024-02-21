#' Allofus Enrollment Date
#'
#' @description Calculates the date of the first consent PII observation for each person in the cohort.
#'
#' @param cohort An SQL tbl with a person_id column. Does not currently work with dataframes.
#' @param con dbplyr connection to the aou bigquery database.
#' @param collect whether to collect the results into a local data frame. Default is FALSE.
#' @param ... not used
#'
#' @details Implements query suggested by allofus described here: https://support.researchallofus.org/hc/en-us/articles/13176125767188-How-to-find-participant-enrollment-data
#'
#' @return a SQL query or local data frame
#' @export
#'
#' @examplesIf on_workbench()
#'
#' con <- aou_connect()
#' cohort <- dplyr::tbl(con, "person") %>%
#'   dplyr::filter(person_id > 5000000) %>%
#'   dplyr::select(person_id)
#'
#' aou_enrollment_date(
#'   cohort
#' )
aou_enrollment_date <- function(
    cohort = NULL,
    con = getOption("aou.default.con"),
    collect = FALSE, ...){

  # check for connection
  if (is.null(con)) {
    cli::cli_abort(c("No connection available.",
                     "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
                     "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }

  # validate cohort argument
  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling enrollment dates for entire All of Us cohort."))
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
  }

  tmp = dplyr::tbl(con, "concept") %>%
    dplyr::filter(.data$concept_name == "Consent PII", .data$concept_class_id == "Module") %>%
    dplyr::inner_join(dplyr::tbl(con, "concept_ancestor"), by = c("concept_id" = "ancestor_concept_id")) %>%
    dplyr::inner_join(
      dplyr::tbl(con, "observation") %>% dplyr::select("person_id", "observation_source_concept_id", "observation_date"),
      by = c("descendant_concept_id" = "observation_source_concept_id")) %>%
    dplyr::group_by(.data$person_id) %>%
    dplyr::filter(.data$observation_date == min(.data$observation_date)) %>%
    dplyr::select("person_id", "primary_consent_date" = "observation_date")

  if(!is.null(cohort)){
    tmp =  tmp %>%
      dplyr::inner_join(cohort, by = "person_id")
  }

  if(isTRUE(collect)){
    return(collect(tmp))
  } else {
    return(tmp)
  }

}
