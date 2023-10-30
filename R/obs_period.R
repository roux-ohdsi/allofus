#' Generate an observation period table based on OMOP Conventions
#'
#' @param cohort query to a cohort or local dataframe
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
#'   tbl(con, "cb_search_person") %>%
#'   filter(has_ehr_data == 1) %>%
#'   head(100) %>%
#'   select(person_id)
#'
#' observation_periods <- aou_observation_period(cohort,
#'   persistence_window = 548,
#'   end_date_buffer = 60,
#'   collect = FALSE
#' )
#' }
#'
aou_observation_period <- function(cohort,
                                   persistence_window = 548,
                                   end_date_buffer = 60,
                                   exclude_aou_visits = FALSE,
                                   collect = FALSE) {
  if (is.data.frame(cohort)) {
    tmp <- tbl(con, "visit_occurrence") %>%
      filter(person_id %in% !!cohort$person_id)
  } else {
    tmp <- tbl(con, "visit_occurrence") %>%
      inner_join(cohort, by = "person_id")
  }

  if (exclude_aou_visits) {
    tmp <- tmp %>%
      filter(visit_type_concept_id != 44818519)
  }

  obs_period <-
    tmp %>%
    select(person_id, visit_start_date, visit_end_date) %>%
    distinct() %>%
    group_by(person_id) %>%
    # use window order instead of arrange. because arrange == ORDER BY which is executed last in sql
    dbplyr::window_order(person_id, visit_start_date, visit_end_date) %>%
    # get the last visit end date within each person
    mutate(last_end_date = lag(visit_end_date)) %>%
    # calc days in between visits.
    mutate(days_between_visits = date_diff(visit_start_date, last_end_date, sql("day"))) %>%
    # iterate over the observation period if days between visits is more than the persistence window
    mutate(obs_period = cumsum(ifelse(days_between_visits > persistence_window, 1, 0))) %>%
    # the first observation per person is NA , but can just be changed to 1
    mutate(obs_period = ifelse(is.na(obs_period), 1, obs_period + 1)) %>%
    # get the minimum and maximums leftover
    group_by(person_id, obs_period) %>%
    summarize(
      observation_start_date = min(visit_start_date),
      observation_end_date = max(visit_end_date), .groups = "drop"
    ) %>%
    group_by(person_id) %>%
    # pad the end date
    mutate(observation_end_date = date_add(observation_end_date, sql(paste0("INTERVAL ", end_date_buffer, " day")))) %>%
    dbplyr::window_order(person_id, obs_period)

  # collect if desired.
  if (isTRUE(collect)) {
    obs_period <- collect(obs_period)
  }

  return(obs_period)
}
