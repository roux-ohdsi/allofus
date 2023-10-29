#' Get occurrences of a concept set from AoU for a given cohort
#'
#'
#'
#' @param cohort tbl; reference to a table with a column called "person_id", and columns for start_date and end_date
#' @param concepts num; a vector of concept ids
#' @param start_date the name of the start_date column in the cohort table (unquoted)
#' @param end_date the name of the end_date column in the cohort table (unquoted)
#' @param return one of "indicator", "count", "all"; do you want to return a 1 if a person has any matching concepts and 0 if not ("indicator"),
#'  the number of matching concepts per person ("count"), or all info about the matching concepts ("all"). Defaults to "indicator"
#' @param concept_set_name chr; If return = "indicator", name for that column. Defaults to "concept_set".
#' @param min_n dbl; If return = "indicator", the minimum number of occurrences per person to consider the indicator true. Defaults to 1.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param collect lgl; whether to collect from the database
#'
#' @return a dataframe if collect = TRUE; a remote tbl if not
#' @export
#'
#' @examples
#' \dontrun{
#' # simple example
#' aspirin_users <- pull_concepts(cohort, concepts = 1191, start_date = covariate_start_date,
#'  end_date = cohort_start_date, concept_set_name = "aspirin", domain = "drug")
#'
#' # starting with person table
#'people <- tbl(con, "person") %>%
#' mutate(start_date = as.Date("1970-01-01"),
#'        end_date = as.Date("2023-05-24"))
#'
#' dat <- aou_concept_set(cohort = people,
#'                          concepts = c(725115, 1612146, 1613031),
#'                          start_date = start_date,
#'                          end_date = end_date,
#'                          concept_set_name = "CGM",
#'                          return = "all")
#'  }
#'
aou_concept_set <- function(cohort,
                              concepts,
                              start_date,
                              end_date,
                              domains = c("condition", "measurement", "observation", "procedure", "drug", "device"),
                              return = "indicator",
                              concept_set_name = "concept_set",
                              min_n = 1,
                              con = getOption("aou.default.con"),
                              collect = TRUE, ...) {

  if (is.null(con)) stop("Provide `con` as an argument or default with `options(aou.default.con = ...)`")

  all_concepts <- purrr::map(
    domains,
    ~ aou_get_concepts(cohort, concepts,
                       {{ start_date }}, {{ end_date }},
                       domain = tolower(.x))
  ) |>
    purrr::reduce(union_all) |>
    dplyr::mutate(concept_set = concept_set_name) |>
    dplyr::distinct()

  if (return == "all") {
    if (collect) return(dplyr::collect(all_concepts))
    return(all_concepts)
  } else if (return == "count") {
    if (collect) return(dplyr::collect(dplyr::count(all_concepts, concept_set, person_id)))
    return(dplyr::count(all_concepts, concept_set, person_id))
  }

  if (is.null(min_n) || !is.numeric(min_n)) stop("Provide a number to `min_n` to restrict to observations with at least that number of rows")

  if (min_n > 1) {
    all_concepts <- all_concepts |>
      dplyr::group_by(person_id) |>
      dplyr::filter(dplyr::n() >= min_n) |>
      dplyr::ungroup()
  }

  res <- all_concepts |>
    dplyr::distinct(person_id) |>
    dplyr::mutate(!!concept_set_name := 1)

  if (collect) return(dplyr::collect(res))

  res

}

#' Get concepts from the condition table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering condition occurrences
#' @param end_date The name of the column with the end date for filtering condition occurrences
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "condition"
#'
#' @examples
#' aou_get_condition_concepts(cohort, c(123, 456), start_date, end_date)

aou_get_condition_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("condition_occurrence", type = "left", by = "person_id") |>
    dplyr::select(-c(
      condition_start_datetime, condition_end_date, condition_end_datetime,
      condition_type_concept_id, stop_reason, provider_id, visit_occurrence_id,
      visit_detail_id, condition_source_value, condition_source_concept_id,
      condition_status_source_value, condition_status_concept_id
    )) |>
    dplyr::filter(condition_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(condition_start_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("condition_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = condition_start_date, concept_id = condition_concept_id,
           concept_name, domain = domain_id
    )
}

#' Get concepts from the measurement table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering measurements
#' @param end_date The name of the column with the end date for filtering measurements
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "measurement"
#'
#' @examples
#' aou_get_measurement_concepts(cohort, c(123, 456), start_date, end_date)
aou_get_measurement_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("measurement", type = "left", by = "person_id") |>
    dplyr::select(-c(
      measurement_datetime, measurement_time, measurement_type_concept_id, operator_concept_id,
      value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high,
      provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value,
      measurement_source_concept_id, unit_source_value, value_source_value
    )) |>
    dplyr::filter(measurement_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(measurement_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("measurement_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = measurement_date, concept_id = measurement_concept_id,
           concept_name, domain = domain_id
    )
}

#' Get concepts from the procedure table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering procedures
#' @param end_date The name of the column with the end date for filtering procedures
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "procedure"
#'
#' @examples
#' aou_get_procedure_concepts(cohort, c(123, 456), start_date, end_date)
aou_get_procedure_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("procedure_occurrence", type = "left", by = "person_id") |>
    dplyr::select(-c(
      procedure_datetime, procedure_type_concept_id, modifier_concept_id,
      quantity, provider_id, visit_occurrence_id, visit_detail_id,
      procedure_source_value, procedure_source_concept_id, modifier_source_value
    )) |>
    dplyr::filter(procedure_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(procedure_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("procedure_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = procedure_date, concept_id = procedure_concept_id,
           concept_name, domain = domain_id
    )
}

#' Get concepts from the observation table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering observations
#' @param end_date The name of the column with the end date for filtering observations
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "observation"
#'
#' @examples
#' aou_get_observation_concepts(cohort, c(123, 456), start_date, end_date)
aou_get_observation_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("observation", type = "left", by = "person_id") |>
    dplyr::select(-c(
      observation_datetime, observation_type_concept_id, value_as_number,
      value_as_string, value_as_concept_id, qualifier_concept_id,
      unit_concept_id, provider_id, visit_occurrence_id, visit_detail_id,
      observation_source_value, observation_source_concept_id,
      unit_source_value, qualifier_source_value
    )) |>
    dplyr::filter(observation_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(observation_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("observation_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = observation_date, concept_id = observation_concept_id,
           concept_name, domain = domain_id
    )
}

#' Get concepts from the drug exposure table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering drugs
#' @param end_date The name of the column with the end date for filtering drugs
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "drug"
#'
#' @examples
#' aou_get_drug_concepts(cohort, c(123, 456), start_date, end_date)
aou_get_drug_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("drug_exposure", type = "left", by = "person_id") |>
    dplyr::select(-c(
      drug_exposure_start_datetime, drug_exposure_end_date,
      drug_exposure_end_datetime, verbatim_end_date, drug_type_concept_id,
      stop_reason, refills, quantity, days_supply, sig, route_concept_id,
      lot_number, provider_id, visit_occurrence_id, visit_detail_id,
      drug_source_value, drug_source_concept_id, route_source_value,
      dose_unit_source_value
    )) |>
    dplyr::filter(drug_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(drug_exposure_start_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("drug_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = drug_exposure_start_date, concept_id = drug_concept_id,
           concept_name, domain = domain_id
    )
}

#' Get concepts from the device exposure table for a given cohort
#'
#' @param cohort A cohort object
#' @param concepts A vector of concept IDs to filter by
#' @param start_date The name of the column with the start date for filtering devices
#' @param end_date The name of the column with the end date for filtering devices
#' @param ... Additional arguments to pass to aou_join
#'
#' @return A data frame with columns person_id, date, concept_id, concept_name, and domain = "device"
#'
#' @examples
#' aou_get_device_concepts(cohort, c(123, 456), start_date, end_date)
aou_get_device_concepts <- function(cohort, concepts, start_date, end_date, ...) {
  cohort |>
    aou_join("device_exposure", type = "left", by = "person_id") |>
    dplyr::select(-c(
      device_exposure_start_datetime, device_exposure_end_date,
      device_exposure_end_datetime, device_type_concept_id,
      unique_device_id, quantity, provider_id, visit_occurrence_id,
      visit_detail_id, device_source_value, device_source_concept_id
    )) |>
    dplyr::filter(device_concept_id %in% concepts) |>
    dplyr::filter(dplyr::between(device_exposure_start_date, {{ start_date }}, {{ end_date }})) |>
    aou_join("concept", type = "left", by = c("device_concept_id" = "concept_id")) |>
    dplyr::select(person_id,
           date = device_exposure_start_date, concept_id = device_concept_id,
           concept_name, domain = domain_id
    )
}


#' Get concepts from a specified domain
#'
#' @param ...  Arguments passed to specific `aou_get_{}_concepts()` function
#' @param domain A character string specifying the domain to retrieve concepts from. Must be one of: "condition", "measurement", "observation", "procedure", "drug", "device"
#'
#' @return A list of concepts from the specified domain
#'
#' @examples
#' aou_get_concepts(cohort, c(123, 456), start_date, end_date, domain = "condition")
#'
aou_get_concepts <- function(..., domain = c("condition", "measurement", "observation", "procedure", "drug", "device")) {
  if (length(domain) != 1) stop("Provide one domain only")
  if (!domain %in% c("condition", "measurement", "observation", "procedure", "drug", "device")) {
    stop(
      '`domain` must be one of: "condition", "measurement", "observation", "procedure", "drug", "device"'
    )
  }
  get(paste("aou_get", domain, "concepts", sep = "_"))(..., combine = FALSE)
}
