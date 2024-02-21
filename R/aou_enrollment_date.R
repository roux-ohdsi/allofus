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

  # check connection
  check_connection()
  # check cohort
  function_cohort = validate_cohort_sql(cohort = cohort)

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
      dplyr::inner_join(function_cohort, by = "person_id")
  }

  if(isTRUE(collect)){
    return(collect(tmp))
  } else {
    return(tmp)
  }

}
