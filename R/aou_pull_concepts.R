#' Get occurrences of a concept set from AoU for a given cohort
#'
#' @param cohort query to a cohort or local dataframe with a column called "person_id", and (possibly) columns for start_date and end_date.
#' If not provided, defaults to entire All of Us cohort.
#' @param concepts a vector of concept ids
#' @param start_date chr; the name of the start_date column in the cohort table; defaults to NULL to pull data across all dates
#' @param end_date chr; the name of the end_date column in the cohort table; defaults to NULL to pull data across all dates
#' @param domains chr; a vector of domains to search for the concepts in ("condition", "measurement", "observation", "procedure", "drug", "device", "visit"); defaults to all
#' @param output one of "indicator", "count", "all"; do you want to return a 1 if a person has any matching concepts and 0 if not ("indicator"),
#'  the number of matching concepts per person ("count"), or all info about the matching concepts ("all"). Defaults to "indicator"
#' @param concept_set_name chr; If output = "indicator" or output = "n", name for that column. Defaults to "concept_set".
#' @param min_n dbl; If output = "indicator", the minimum number of occurrences per person to consider the indicator true. Defaults to 1.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param collect lgl; whether to collect from the database
#' @param ... further arguments passed along to `collect()` if `collect = TRUE`
#'
#' @return a dataframe if collect = TRUE; a remote tbl if not
#' @export
#'
#' @examplesIf on_workbench()
#' # indicator for any aspirin at any time
#' aspirin_users <- aou_concept_set(concepts = 1191, concept_set_name = "aspirin", domains = "drug")
#'
#' # starting with person table to create a cohort
#' people <- tbl(con, "person") %>%
#'   filter(person_id < 2000000) %>%
#'   mutate(
#'     start = as.Date("2021-01-01"),
#'     end = as.Date("2023-12-31")
#'   )
#'
#' dat <- aou_concept_set(
#'   cohort = people,
#'   concepts = c(725115, 1612146, 1613031),
#'   start_date = "start",
#'   end_date = "end",
#'   concept_set_name = "CGM",
#'   output = "all"
#' )
#'
aou_concept_set <- function(cohort = NULL,
                            concepts,
                            start_date = NULL,
                            end_date = NULL,
                            domains = c(
                              "condition", "measurement", "observation",
                              "procedure", "drug", "device", "visit"
                            ),
                            output = "indicator",
                            concept_set_name = "concept_set",
                            min_n = 1,
                            con = getOption("aou.default.con"),
                            collect = FALSE, ...) {

  if (is.null(con)) cli::cli_abort(c("No connection available.",
                                     "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
                                     "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."))

  # keep track of whether we are forced to collect
  # due to start and end dates provided with cohort as dataframe
  must_collect <- FALSE

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling concepts for entire All of Us cohort."))
    if (!is.null(start_date) || !is.null(end_date)) {
      cli::cli_warn(c("No cohort provided.",
                      ">" = "Ignoring start and end date."))
    }
    tmp <- dplyr::tbl(con, "person")
  } else {
    if (is.data.frame(cohort)) {
      tmp <- dplyr::tbl(con, "person") %>%
        dplyr::filter(.data$person_id %in% !!cohort$person_id)
      if (!collect && (!is.null(start_date) || !is.null(end_date))) {
        # can't have these both because we can't join (on the dates) without collecting
        cli::cli_warn(c("Cannot have {.code collect = FALSE} and also provide start and end dates.",
                        ">" = "Changing to {.code collect = TRUE}."))
        collect <- TRUE
        must_collect <- TRUE
      }
    } else {
      tmp <- cohort
    }
  }

  if (is.null(con)) cli::cli_abort(c("No connection available.",
                                     "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
                                     "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."))

  if (!all(domains %in% c("condition", "measurement", "observation", "procedure", "drug", "device", "visit"))) {
    cli::cli_abort(
      '{.code domains} can only include "condition", "measurement", "observation", "procedure", "drug", "device", "visit".'
    )
  }

  if (is.null(start_date) || is.data.frame(cohort)) {
    tmp <- dplyr::mutate(tmp, start_date = as.Date("1970-01-01"))
    start_date <- "start_date"
  }
  if (is.null(end_date) || is.data.frame(cohort)) {
    tmp <- dplyr::mutate(tmp, end_date = as.Date("2100-01-01"))
    end_date <- "end_date"
  }

  # now no matter what there will be start_date and end_date columns
  tmp <- dplyr::mutate(tmp,
                       start_date = .data[[start_date]],
                       end_date = .data[[end_date]])

  all_concepts <- data.frame(
    domain = c(
      "condition", "measurement", "observation",
      "procedure", "drug", "device", "visit"
    ),
    tbl_name = c(
      "condition_occurrence",
      "measurement", "observation", "procedure_occurrence", "drug_exposure",
      "device_exposure", "visit_occurrence"
    ),
    date_column = c(
      "condition_start_date",
      "measurement_date", "observation_date",
      "procedure_date", "drug_exposure_start_date",
      "device_exposure_start_date", "visit_start_date"
    ),
    concept_id_column = c(
      "condition_concept_id", "measurement_concept_id",
      "observation_concept_id", "procedure_concept_id",
      "drug_concept_id", "device_concept_id", "visit_concept_id"
    )
  ) %>%
    dplyr::filter(.data$domain %in% domains) %>%
    purrr::pmap(get_domain_concepts, cohort = tmp, concepts = concepts, start_date = start_date, end_date = end_date) %>%
    purrr::reduce(dplyr::union_all) %>%
    dplyr::distinct()

  if (must_collect) {
    # collect to restrict the concepts between the given start and end dates
    all_concepts <- dplyr::collect(all_concepts) %>%
      dplyr::right_join(cohort, by = dplyr::join_by('person_id', dplyr::between('concept_date', 'start_date', 'end_date')))
    cohort_w_concepts <- all_concepts
  } else {
    cohort_w_concepts <- dplyr::right_join(all_concepts, tmp,
      by = dplyr::join_by('person_id', dplyr::between('concept_date', 'start_date', 'end_date'))
    )
  }


  if (output == "all") {
    # remove start_date and end_date columns if they were not there in the first place
    if (!"start_date" %in% colnames(cohort)) {
      cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("start_date"))
    }
    if (!"end_date" %in% colnames(cohort)) {
      cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("end_date"))
    }

    if (collect && !must_collect) {
      # if must_collect, then it's already collected
      return(dplyr::collect(all_concepts, ...))
    } else {
      return(all_concepts)
    }
  }

  if (any(grepl("^value\\_", colnames(cohort_w_concepts)))) {
    cli::cli_warn(c("Output includes data from the measurement table. Measurement values will be lost with {.code output = 'indicator'} or {.code output = 'count'}.",
                    ">" = "Consider using {.code output = 'all'} to get all data."))
  }

  counted <- cohort_w_concepts %>%
    dplyr::group_by(.data$person_id, .data[[start_date]], .data[[end_date]]) %>%
    dplyr::summarise(n = sum(ifelse(is.na(.data$concept_id), 0, 1)), .groups = "drop")

  if (output == "count") {
    counted <- counted %>%
      dplyr::rename(!!concept_set_name := 'n')

    # remove start_date and end_date columns if they were not there in the first place
    if (!"start_date" %in% colnames(cohort)) {
      cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("start_date"))
    }
    if (!"end_date" %in% colnames(cohort)) {
      cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("end_date"))
    }

    if (collect && !must_collect) {
      return(dplyr::collect(counted, ...))
    } else {
      return(counted)
    }
  }

  if (is.null(min_n) || !is.numeric(min_n)) cli::cli_abort("Provide a number to {.code min_n} to restrict to observations with at least that number of rows.")

  res <- counted %>%
    dplyr::mutate(!!concept_set_name := ifelse(.data$n >= !!min_n, 1, 0)) %>%
    dplyr::select(-'n')

  # remove start_date and end_date columns if they were not there in the first place
  if (!"start_date" %in% colnames(cohort)) {
    cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("start_date"))
  }
  if (!"end_date" %in% colnames(cohort)) {
    cohort_w_concepts <- dplyr::select(cohort_w_concepts, -dplyr::any_of("end_date"))
  }

  if (collect && !must_collect) {
    return(dplyr::collect(res, ...))
  } else {
    return(res)
  }
}
#' Retrieves domain concepts for a given cohort and time range
#'
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is set automatically if you use `aou_connect()`
#' @param cohort A data frame containing person IDs
#' @param concepts A vector of concept IDs to retrieve
#' @param start_date The start date of the time range to retrieve concepts for
#' @param end_date The end date of the time range to retrieve concepts for
#' @param tbl_name The name of the table containing the domain concepts
#' @param date_column The name of the column containing the concept dates
#' @param concept_id_column The name of the column containing the concept IDs
#' @param ... Additional arguments not currently used
#'
#' @noRd

get_domain_concepts <- function(cohort, concepts, start_date, end_date, tbl_name, date_column, concept_id_column, con = getOption("aou.default.con"), ...) {

  domain_tbl <- dplyr::tbl(con, tbl_name) %>%
    dplyr::select('person_id', concept_date = .data[[date_column]], concept_id = .data[[concept_id_column]], dplyr::starts_with("value_"))

  cohort %>%
    # suffix is needed because the cohort and domain tables have the same column names
    dplyr::left_join(domain_tbl, by = "person_id", suffix = c(tbl_name, "")) %>%
    dplyr::filter(.data$concept_id %in% concepts) %>%
    dplyr::filter(dplyr::between(.data$concept_date, .data[[start_date]], .data[[end_date]])) %>%
    dplyr::left_join(dplyr::select(dplyr::tbl(con, "concept"), 'concept_id', 'concept_name', 'domain_id'),
      by = "concept_id"
    ) %>%
    dplyr::select('person_id', 'concept_date', 'concept_id', 'concept_name', concept_domain = 'domain_id', dplyr::starts_with("value_"))
}
