#' Generate an observation period table based on OMOP Conventions
#'
#' @param cohort Query to a cohort or local dataframe with column "person_id". If no cohort is provided,
#' defaults to the entire All of Us cohort
#' @param persistence_window Longest allowable time between visits for the same observation period. Defaults to 548 (see details)
#' @param end_date_buffer Number of days to add to last observed date. Defaults to 60 (see details)
#' @param exclude_aou_visits Whether to exclude All of Us clinical visits (i.e., for program-specific measurements,
#' not part of the participants' typical EHR) from the observation period. Defaults to `FALSE`
#' @param con Connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param collect Whether to collect the data or keep as SQL query. Defaults to `FALSE`.
#' @param max_visit_length max visit length for all visits
#' @param max_op_visit_length max visit length for outpatient visits
#' @param max_er_visit_length max visit length for emergency room visits
#' @param ... Further arguments passed along to `collect()` if `collect = TRUE`
#'
#' @details
#' The visit_occurrence table
# is used to generate a new observation_period table based on OHDSI conventions here:
#' <https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html>. The visit_occurrence table in the
#' All of Us OMOP CDM contains dates from participant data collection visits which are excluded by default but can be included. Some of the
#' visit lengths in the visit_occurrence table are unrealistically long for EHR data. These visits can be constrained
#' by setting the `max_visit_length`, `max_op_visit_length`, and `max_er_visit_length` parameters. >99% of these visits are of the outpatient
#' visit type, so its likely these are errors in the data. However, assuming these are errors, there's no way to determine whether the
#' visit_start_date or visit_end_date are the actual visit date. Therefore, the function will exclude these visits when generating the
#' observation period table.
#'
#' Users should note that the aou_observation_period function will only generate observation periods for
#' participants who have at least one visit in the visit_occurrence table. If participant in the AllofUs research
#' program who did not include electronic health record data are included in the cohort argument, they will not be included in the
#' generated observation period table.
#'
#'
#' @return a sql query or local data frame
#' @export
#'
#' @examplesIf on_workbench()
#' cohort <- dplyr::tbl(con, "cb_search_person") %>%
#'   dplyr::filter(has_ehr_data == 1) %>%
#'   head(100) %>%
#'   dplyr::select(person_id)
#'
#' observation_periods <- aou_observation_period(cohort,
#'   persistence_window = 548,
#'   end_date_buffer = 60,
#'   collect = FALSE
#' )
#'
aou_observation_period <- function(cohort = NULL,
                                   persistence_window = 548,
                                   end_date_buffer = 60,
                                   exclude_aou_visits = TRUE,
                                   con = getOption("aou.default.con"),
                                   collect = FALSE,
                                   max_visit_length = 1095, # 3 years
                                   max_op_visit_length = 7, # 7 days
                                   max_er_visit_length = 30, # 30 days
                                   ...) {
  if (is.null(con)) {
    cli::cli_abort(c("No connection available.",
      "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
      "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }
  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Creating observation periods for entire All of Us cohort."))
    tmp <- dplyr::tbl(con, "visit_occurrence")
  } else {
    if (is.data.frame(cohort)) {
      tmp <- dplyr::tbl(con, "visit_occurrence") %>%
        dplyr::filter(.data$person_id %in% !!cohort$person_id)

      n = nrow(cohort %>% dplyr::distinct("person_id"))
    } else {
      tmp <- dplyr::tbl(con, "visit_occurrence") %>%
        dplyr::inner_join(cohort, by = "person_id")

      n = tally(cohort %>% dplyr::distinct("person_id")) %>% collect()
      n = n[[1,1]]
    }
  }

  if (exclude_aou_visits) {
    tmp <- tmp %>%
      dplyr::filter(.data$visit_type_concept_id != 44818519)
  }

  visit_concepts <- tbl(con, "concept") %>% select(concept_id, concept_name)
  op_visits <- c(9202, 581477,38004207)
  er_visits <- c(9203, 581381, 8870)

  obs_period <-
    tmp %>%
    dplyr::select("person_id", "visit_start_date", "visit_end_date", "visit_concept_id") %>%
    dplyr::distinct() %>%
    dplyr::mutate(visit_length = DATE_DIFF(.data$visit_end_date, .data$visit_start_date, dplyr::sql("day"))) %>%
    # some visits have ridiculous lengths - this is a fix for this for now.
    dplyr::filter(
      dplyr::case_when(
        visit_length > max_op_visit_length & visit_concept_id %in% op_visits ~ FALSE,
        visit_length > max_er_visit_length & visit_concept_id %in% er_visits ~ FALSE,
        visit_length > max_visit_length ~ FALSE,
        TRUE ~ TRUE
      )) %>% # filter out visits that are too long
    dplyr::group_by(.data$person_id) %>%
    # use window order instead of arrange. because arrange == ORDER BY which is executed last in sql
    dbplyr::window_order(.data$person_id, .data$visit_start_date, .data$visit_end_date) %>%
    # get the last visit end date within each person
    dplyr::mutate(last_end_date = lag(.data$visit_end_date)) %>%
    # calc days in between visits.
    dplyr::mutate(days_between_visits = DATE_DIFF(.data$visit_start_date, .data$last_end_date, dplyr::sql("day"))) %>%
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
    dplyr::mutate(observation_period_end_date = DATE_ADD(.data$observation_period_end_date, dplyr::sql(paste0("INTERVAL ", end_date_buffer, " day")))) %>%
    dbplyr::window_order(.data$person_id, .data$obs_period) %>%
    dplyr::ungroup()

  n_obs_period <- tally(obs_period %>% distinct("person_id")) %>% collect()
  n_obs_period <- n_obs_period[[1,1]]

  if(n != n_obs_period){cli::cli_inform("warning for different number of people in obs period table")}

  # collect if desired.
  if (isTRUE(collect)) {
    obs_period <- dplyr::collect(obs_period, ...)
  }



  return(obs_period)
}
