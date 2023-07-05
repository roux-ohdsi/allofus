#' Get occurrences of a concepts from AoU for a given cohort
#'
#'
#' @param cohort tbl; reference to a table with a column called "person_id", and columns for start_date and end_date
#' @param concepts num; a vector of concept ids
#' @param concept_set_name chr; Name to describe the concept set, used to create an indicator variable
#' @param start_date the name of the start_date column in the cohort table (unquoted)
#' @param end_date the name of the end_date column in the cohort table (unquoted)
#' @param min_n dbl; the minimum number of occurrences per person to consider the indicator true
#' @param n dbl; count the number of occurrences per person (will not include zeros)
#' @param keep_all lgl; keep columns with information about the concept (e.g., concept name, id, etc.)
#' @param con the connection object to AoU
#' @param collect lgl; whether to collect from the database
#'
#' @return a dataframe if collect = TRUE; a remote tbl if not
#' @export
#'
#' @examples
#' \dontrun{
#' # simple example
#' tobacco <- pull_concepts(cohort, concepts = 1157, start_date = covariate_start_date,
#'  end_date = cohort_start_date, name = "tobacco")
#'
#' # starting with person table
#'people <- tbl(con, "cb_search_person") %>%
#' mutate(start_date = as.Date("1970-01-01"),
#'        end_date = as.Date("2023-05-24"))
#'
#' dat <- aou_pull_concepts(cohort = people,
#'                          concepts = c(725115, 1612146, 1613031),
#'                          start_date = start_date,
#'                          end_date = end_date, concept_set_name = "CGM",
#'                          domains = c("condition", "measurement", "observation", "procedure", "drug", "device"),
#'                          keep_all = TRUE)
#'  }
#'
aou_pull_concepts <- function(cohort,
                              concepts,
                              start_date,
                              end_date,
                              concept_set_name = "concepts",
                              domains = c("condition", "measurement", "observation", "procedure", "drug", "device"),
                              min_n = NULL,
                              n = FALSE,
                              keep_all = FALSE,
                              con = getOption("con.default.value"),
                              collect = TRUE, ...){

  if (is.null(concept_set_name)) concept_set_name <- paste0("concept_set_", concept_set_id)
  if (!is.null(min_n) & !is.numeric(min_n)) stop("Provide a number to `min_n` to restrict to observations with at least that number of rows")
  if (n && keep_all) warning("The `keep_all` argument takes precedence; all data will be returned instead of counts.")

  if (is.null(con)) stop("Provide `con` as an argument or default with `options(con.default.value = ...)`")

  all_concepts <- map(
    domains,
    ~ aou_get_concepts(cohort, concepts,
                       {{ start_date }}, {{ end_date }},
                       domain = tolower(.x))
  ) |>
    reduce(union_all) |>
    mutate(concept_set = concept_set_name) |>
    distinct()

  if (!is.null(min_n)) {
    all_concepts <- all_concepts |>
      group_by(person_id) |>
      filter(n() >= min_n) |>
      ungroup()
  }

  if (keep_all) {
    if (collect) return(collect(all_concepts))
    return(all_concepts)
  }

  if (n) {
    if (collect) return(collect(count(all_concepts, concept_set, person_id)))
    return(count(all_concepts, concept_set, person_id))
  }

  res <- all_concepts |>
    distinct(person_id) |>
    mutate(!!concept_set_name := 1)

  if (collect) return(collect(res))

  res

}

aou_get_condition_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("condition_occurrence", type = "left", by = "person_id") |>
    select(-c(
      condition_start_datetime, condition_end_date, condition_end_datetime,
      condition_type_concept_id, stop_reason, provider_id, visit_occurrence_id,
      visit_detail_id, condition_source_value, condition_source_concept_id,
      condition_status_source_value, condition_status_concept_id
    )) |>
    filter(condition_concept_id %in% concepts) |>
    filter(between(condition_start_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("condition_concept_id" = "concept_id")) |>
    select(person_id,
           date = condition_start_date, concept_id = condition_concept_id,
           concept_name, domain = domain_id
    )
}

aou_get_measurement_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("measurement", type = "left", by = "person_id") |>
    select(-c(
      measurement_datetime, measurement_time, measurement_type_concept_id, operator_concept_id,
      value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high,
      provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value,
      measurement_source_concept_id, unit_source_value, value_source_value
    )) |>
    filter(measurement_concept_id %in% concepts) |>
    filter(between(measurement_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("measurement_concept_id" = "concept_id")) |>
    select(person_id,
           date = measurement_date, concept_id = measurement_concept_id,
           concept_name, domain = domain_id
    )
}

aou_get_procedure_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("procedure_occurrence", type = "left", by = "person_id") |>
    select(-c(
      procedure_datetime, procedure_type_concept_id, modifier_concept_id,
      quantity, provider_id, visit_occurrence_id, visit_detail_id,
      procedure_source_value, procedure_source_concept_id, modifier_source_value
    )) |>
    filter(procedure_concept_id %in% concepts) |>
    filter(between(procedure_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("procedure_concept_id" = "concept_id")) |>
    select(person_id,
           date = procedure_date, concept_id = procedure_concept_id,
           concept_name, domain = domain_id
    )
}

aou_get_observation_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("observation", type = "left", by = "person_id") |>
    select(-c(
      observation_datetime, observation_type_concept_id, value_as_number,
      value_as_string, value_as_concept_id, qualifier_concept_id,
      unit_concept_id, provider_id, visit_occurrence_id, visit_detail_id,
      observation_source_value, observation_source_concept_id,
      unit_source_value, qualifier_source_value
    )) |>
    filter(observation_concept_id %in% concepts) |>
    filter(between(observation_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("observation_concept_id" = "concept_id")) |>
    select(person_id,
           date = observation_date, concept_id = observation_concept_id,
           concept_name, domain = domain_id
    )
}

aou_get_drug_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("drug_exposure", type = "left", by = "person_id") |>
    select(-c(
      drug_exposure_start_datetime, drug_exposure_end_date,
      drug_exposure_end_datetime, verbatim_end_date, drug_type_concept_id,
      stop_reason, refills, quantity, days_supply, sig, route_concept_id,
      lot_number, provider_id, visit_occurrence_id, visit_detail_id,
      drug_source_value, drug_source_concept_id, route_source_value,
      dose_unit_source_value
    )) |>
    filter(drug_concept_id %in% concepts) |>
    filter(between(drug_exposure_start_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("drug_concept_id" = "concept_id")) |>
    select(person_id,
           date = drug_exposure_start_date, concept_id = drug_concept_id,
           concept_name, domain = domain_id
    )
}

aou_get_device_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    omop_join("device_exposure", type = "left", by = "person_id") |>
    select(-c(
      device_exposure_start_datetime, device_exposure_end_date,
      device_exposure_end_datetime, device_type_concept_id,
      unique_device_id, quantity, provider_id, visit_occurrence_id,
      visit_detail_id, device_source_value, device_source_concept_id
    )) |>
    filter(device_concept_id %in% concepts) |>
    filter(between(device_exposure_start_date, {{ start_date }}, {{ end_date }})) |>
    omop_join("concept", type = "left", by = c("device_concept_id" = "concept_id")) |>
    select(person_id,
           date = device_exposure_start_date, concept_id = device_concept_id,
           concept_name, domain = domain_id
    )
}


aou_get_concepts <- function(..., domain = c("condition", "measurement", "observation", "procedure", "drug", "device")) {
  if (length(domain) != 1) stop("Provide one domain only")
  if (!domain %in% c("condition", "measurement", "observation", "procedure", "drug", "device")) {
    stop(
      '`domain` must be one of: "condition", "measurement", "observation", "procedure", "drug", "device"'
    )
  }
  get(paste("aou_get", domain, "concepts", sep = "_"))(..., combine = FALSE)
}

#' Get survey questions from AoU for a given cohort
#'
#'
#' @param cohort tbl; reference to a table with a column called "person_id"
#' @param concepts num; a vector of concept ids for questions in the survey table
#' @param collect lgl; whether to collect from the database
#' @param reshape lgl; whether to turn the long data into wide data with clean variable names

#' @return if reshape = FALSE, a dataframe or remote tbl with columns person_id, date (survey_datetime),
#'  concept_id (question_concept_id), question, answer. If reshape = TRUE, a dataframe with
#'  questions as columns. Those with multiple answers per person ("checkbox" questions) are list-columns.
#' @export
#' @examples
#' \dontrun{
#' survey_data <- aou_pull_survey_concepts(cohort, concepts = c(1157, 124839), reshape = TRUE)
#'  )}
#'
aou_pull_survey_concepts <- function(cohort, concepts, collect = TRUE, reshape = FALSE, ...) {
  dat <- cohort |>
    omop_join("ds_survey", type = "left", by = "person_id") |>
    select(person_id, question, question_concept_id, answer, answer_concept_id, survey,
           survey_datetime) |>
    filter(question_concept_id %in% concepts)


  if (reshape) {
    if (!collect) warning("survey data must be collected to be reshaped")
    # reshape the questions/answers so that there's one column for every question

    wide_lists <- dat %>%
      select(person_id, question, answer) %>%
      collect() %>%
      # since there are multiple answers for some questions (eg the 'conditions' questions), put all in a list
      pivot_wider(names_from = question, values_from = answer, values_fn = list) %>%
      janitor::clean_names()

    gt1 <- wide_lists %>%
      rowwise() %>%
      mutate(across(where(is.list), length)) %>%
      ungroup %>%
      summarise(across(-person_id, ~max(.x))) %>%
      select(where(~any(.x > 1))) %>%
      names()

    # don't need answers in a list if there is only one per question
    wide <- wide_lists %>%
      unnest(c(where(is.list), -all_of(gt1)), keep_empty = TRUE)

    return(wide)
  }
  if (!collect) return(dat)
  collect(dat)
}
