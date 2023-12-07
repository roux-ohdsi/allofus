#' Generate an observation period table based on OMOP Conventions
#'
#' @param cohort query to a cohort or local dataframe with column "person_id"
#' @param persistence_window longest allowable time between visits for the same observation period. defaults to 548 see details
#' @param end_date_buffer number of days to add to end date. defaults to 60. see details
#' @param exclude_aou_visits whether to exclude All of Us clinical visits (not part of the participants typical EHR) from the observation period
#' @param collect whether to collect the data or keep as SQL query
#'
#' @details
#' Follows conventions described here: https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html
#'
#'
#' @return a sql query or local data frame
#' @export
#'
#' @examples
#' \dontrun{
#' cohort <-
#'   dplyr::tbl(con, "cb_search_person") %>%
#'   dplyr::filter(has_ehr_data == 1) %>%
#'   head(100) %>%
#'   dplyr::select(person_id)
#'
#' observation_periods <- aou_observation_period(cohort,
#'   persistence_window = 548,
#'   end_date_buffer = 60,
#'   collect = FALSE
#' )
#' }
#'
aou_observation_period <- function(cohort = NULL,
                                   persistence_window = 548,
                                   end_date_buffer = 60,
                                   exclude_aou_visits = FALSE,
                                   collect = FALSE) {

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Creating observation periods for entire All of Us cohort."))
    tmp <- dplyr::tbl(con, "visit_occurrence")
  } else {
    if (is.data.frame(cohort)) {
      tmp <- dplyr::tbl(con, "visit_occurrence") %>%
        dplyr::filter(.data$person_id %in% !!cohort$person_id)
    } else {
      tmp <- dplyr::tbl(con, "visit_occurrence") %>%
        dplyr::inner_join(cohort, by = "person_id")
    }
  }

  if (exclude_aou_visits) {
    tmp <- tmp %>%
      dplyr::filter(.data$visit_type_concept_id != 44818519)
  }

  obs_period <-
    tmp %>%
    dplyr::select('person_id', 'visit_start_date', 'visit_end_date') %>%
    dplyr::distinct() %>%
    dplyr::group_by(.data$person_id) %>%
    # use window order instead of arrange. because arrange == ORDER BY which is executed last in sql
    dbplyr::window_order(.data$person_id, .data$visit_start_date, .data$visit_end_date) %>%
    # get the last visit end date within each person
    dplyr::mutate(last_end_date = lag(.data$visit_end_date)) %>%
    # calc days in between visits.
    dplyr::mutate(days_between_visits = date_diff(.data$visit_start_date, .data$last_end_date, dplyr::sql("day"))) %>%
    # iterate over the observation period if days between visits is more than the persistence window
    dplyr::mutate(obs_period = cumsum(ifelse(.data$days_between_visits > persistence_window, 1, 0))) %>%
    # the first observation per person is NA , but can just be changed to 1
    dplyr::mutate(obs_period = ifelse(is.na(obs_period), 1, obs_period + 1)) %>%
    # get the minimum and maximums leftover
    dplyr::group_by(.data$person_id, .data$obs_period) %>%
    dplyr::summarize(
      observation_period_start_date = min(.data$visit_start_date),
      observation_period_end_date = max(.data$visit_end_date), .groups = "drop"
    ) %>%
    dplyr::group_by(.data$person_id) %>%
    # pad the end date
    dplyr::mutate(observation_end_date = date_add(.data$observation_period_end_date, dplyr::sql(paste0("INTERVAL ", end_date_buffer, " day")))) %>%
    dbplyr::window_order(.data$person_id, .data$obs_period)

  # collect if desired.
  if (isTRUE(collect)) {
    obs_period <- dplyr::collect(obs_period)
  }

  return(obs_period)
}
