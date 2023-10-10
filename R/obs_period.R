#' Generate an observation period table based on OMOP Conventions
#'
#' @param cohort query to a cohort or local dataframe
#' @param persistence_window longest allowable time between visits for the same observation period. defaults to 548 see details
#' @param end_date_buffer number of days to add to end date. defaults to 60. see details
#' @param collect whether to collect the data or keep as SQL query
#'
#'@details
#'Follows conventions described here: https://ohdsi.github.io/CommonDataModel/ehrObsPeriods.html
#'
#'
#' @return a sql query or local data frame
#' @export
#'
#' @examples
#'
#' \dontrun{
#' cohort <-
#'   tbl(con, "cb_search_person") %>%
#'     filter(has_ehr_data == 1) %>%
#'     head(100) %>%
#'     select(person_id)
#'
#' aou_observation_period <- function(cohort,
#'                                    persistence_window = 548,
#'                                    end_date_buffer = 60,
#'                                    collect = FALSE)
#' }
#'
aou_observation_period <- function(cohort,
                                   persistence_window = 548,
                                   end_date_buffer = 60,
                                   collect = FALSE){

  if(is.data.frame(cohort)){
    tmp = tbl(con, "visit_occurrence") %>%
      filter(person_id %in% !!cohort$person_id)
  } else {
    tmp = tbl(con, "visit_occurrence") %>%
      inner_join(cohort, by = "person_id")
  }

  cat("Joined to visit_occurrence table \n")

  obs_period =
    tmp %>%
      select(person_id, visit_start_date, visit_end_date) %>%
      distinct() %>%
      group_by(person_id) %>%
      dbplyr::window_order(person_id, visit_start_date, visit_end_date) %>%
      mutate(last_end_date = lag(visit_end_date)) %>%
      mutate(days_between_visits = !!CDMConnector::datediff("last_end_date", "visit_start_date", "day")) %>%
      mutate(obs_period = cumsum(ifelse(days_between_visits > persistence_window, 1, 0))) %>%
      mutate(obs_period = ifelse(is.na(obs_period), 1, obs_period+1)) %>%
      group_by(person_id, obs_period) %>%
      summarize(observation_start_date = min(visit_start_date),
                observation_end_date = max(visit_end_date), .groups = 'drop') %>%
      group_by(person_id) %>%
      mutate(observation_end_date = !!CDMConnector::dateadd("observation_end_date", end_date_buffer, "day")) %>%
      arrange(person_id, obs_period)

  if(isTRUE(collect)){
    obs_period = collect(obs_period)
  }

  return(obs_period)
}
