#' Function to query allofus observation table for survey responses
#'
#' @description
#' Because responses to the survey questions stored in the ds_survey table do not
#' include skipped questions (i.e., missing data!), this function makes it easier to
#' query the observation table for responses to survey questions so that the skipped
#' responses are included.
#'
#' The function will return a dataframe or SQL tbl with the initial cohort table along
#' with a column for each question included in `questions` and answers for
#' each person_id in the cells. The column names (questions) can
#' be returned as the concept_code or concept_id or by providing new column names. For each question, a column with
#' the suffix "_date" is included with the date on which the question was answered.
#' When questions can have multiple answers ("checkbox"-style questions), answers
#' are returned as a comma-separated string.
#'
#' To find the desired survey questions, use the all of us data dictionary,
#' survey codebook, athena, data browser, or the allofus R package
#' modified codebook which can be found here: https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html
#' For questions regarding an individual's health history or family health history,
#' the function requires the specific concept_id (or concept_code) for individual in question,
#' whether that is "self" or another relative. Responses are returned as "Yes" if the respondent
#' answered that the individual had the condition, "No" if the respondent answered that the individual
#' did not have that condition (or omitted it when selecting from related conditions), a
#' skip response if the question was skipped, and NA if the respondent did not answer the question.
#'
#' @param cohort tbl or dataframe with a cohort that includes a column called person_id
#' @param questions either a vector of concept_ids or concept_codes for questions to return results
#' @param question_output how to name the columns. Options include as the text of the concept code ("concept_code"), as concept ids preceded by
#' "x_" ("concept_id"), or using a custom vector of column names matching the vector of `questions`. Defaults to "concept_code".
#' @param clean_answers whether to clean the answers to the survey questions. Defaults to TRUE.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is created automatically with `aou_connect()`
#' @param collect whether to return the results as a local (TRUE) or database table
#' @param ... additional arguments passed to `collect()` when `collect = TRUE`
#' @return a dataframe if collect = TRUE; a remote tbl if not
#' @export
#' @examplesIf on_workbench()
#'
#' con <- aou_connect()
#' cohort <- dplyr::tbl(con, "person") %>%
#'   dplyr::filter(person_id > 5000000) %>%
#'   dplyr::select(person_id, year_of_birth, gender_concept_id)
#'
#' aou_survey(
#'   cohort,
#'   questions = c(1585375, 1586135),
#'   question_output = "concept_code"
#' )
#' aou_survey(
#'   cohort,
#'   questions = c(1585811, 1585386),
#'   question_output = c("pregnancy", "insurance")
#' )
#' aou_survey(
#'   cohort,
#'   questions = c(1585375, 1586135, 1740719, 43529932),
#'   question_output = c("income", "birthplace", "grandpa_bowel_obstruction", "t2dm"),
#'   collect = FALSE
#' )
#'
#' aou_survey(cohort,
#'   questions = 1384452,
#'   question_output = "osteoarthritis"
#' ) %>%
#'   dplyr::count(osteoarthritis)
#'
aou_survey <- function(cohort = NULL,
                       questions,
                       question_output = "concept_code",
                       clean_answers = TRUE,
                       con = getOption("aou.default.con"),
                       collect = FALSE,
                       ...) {
  if (packageVersion("dbplyr") < "2.3.0") {
    cli::cli_abort(c(
      'Older versions of dbplyr are not supported by the {.code aou_survey()} function.',
      i = "Note: v.2.4.0 is alsonot compatible with the AllofUS Database"
      i = 'Please install either dbplyr v2.3.4 or the development version of dbplyr:',

      '# Install pak',
      'install.packages("pak")',
      '# Install dbplyr v2.3.4',
      'pak::pkg_install("tidyverse/dbplyr@v2.3.4")',
      '# Or install development version of dbplyr',
      'pak::pkg_install("tidyverse/dbplyr")',
      '# restart your R kernel'
    ), call = NULL)

  }

  # check for connection
  if (is.null(con)) {
    cli::cli_abort(c("No connection available.",
      "i" = "Provide a connection automatically by running {.code aou_connect()} before this function.",
      "i" = "You can also provide {.code con} as an argument or default with {.code options(aou.default.con = ...)}."
    ))
  }

  # check for NA values or empty strings in questions or question_output
  if(any(is.na(c(questions, question_output))) | any(c(questions, question_output) == "")){
    cli::cli_abort("NA values or empty string detected in {.code questions} or {.code question_output}")
  }

  if (length(question_output) == length(questions)) { # either gave column names or happen to have a single question
    if (length(question_output) == 1) { # gave one column name or "concept_code" or "concept_id"
      # see if the matched argument matches the actual argument
      question_output_arg <- tryCatch(match.arg(question_output, c("concept_code", "concept_id")), error = function(e) question_output)
      # if not, print a message so that the user can spot their problem
      if (!identical(question_output, question_output_arg)) {
        cli::cli_inform("Using {question_output} as column name.",
          ">" = "Did you really mean {question_output_arg}?"
        )
        question_output_arg <- question_output
      }
    } else {
      question_output_arg <- question_output
    }
    # didn't give the right number question_output and it doesn't match one of the options
  } else if (is.null(tryCatch(match.arg(question_output, c("concept_code", "concept_id")), error = function(e) NULL))) {
    cli::cli_abort(c("Length of argument {.code question_output} doesn't match {.code questions}.",
      "i" = "Provide a vector of column names the same length as {.code questions}.",
      "i" = "Alternatively, set either {.code question_output = \"concept_code\"} or {.code question_output = \"concept_id\"}"
    ))

    # gave one value for question_output and multiple values for questions
  } else {
    question_output_arg <- match.arg(question_output, c("concept_code", "concept_id"))
  }
  question_output <- if (length(question_output_arg) == 1 && question_output_arg[1] == "concept_id") "concept_id" else "value"

  if (is.null(cohort)) {
    cli::cli_warn(c("No cohort provided.", ">" = "Pulling survey data for entire All of Us cohort."))
    function_cohort <- dplyr::tbl(con, "person") %>% dplyr::select("person_id")
  } else if (!"person_id" %in% colnames(cohort)) {
    # ensure person_id is a column name in cohort

    cli::cli_abort(c("{.code person_id} column not found in cohort.",
      "i" = "Confirm that the cohort has a column named {.code person_id}"
    ))
  } else if (is.data.frame(cohort)) {
    function_cohort <- dplyr::tbl(con, "person") %>%
      dplyr::filter(.data$person_id %in% !!unique(cohort$person_id)) %>%
      dplyr::select("person_id")
  } else {
    function_cohort <- cohort %>%
      dplyr::select("person_id")
  }

  # pivot longer to inclde the rx, on_txt, and age_diagnosis columns
  aou_health_history_long <- allofus::aou_health_history %>%
    dplyr::filter(.data$relative == "self") %>%
    dplyr::rename(concept_code_overall = "concept_code") %>%
    tidyr::pivot_longer(
      cols = c(
        "concept_id_rx_meds", "concept_id_on_txt", "concept_id_age_diagnosis",
        "concept_code_rx_meds", "concept_code_on_txt", "concept_code_age_diagnosis"
      ),
      names_pattern = "(.+_.+)_(.+_.+)",
      names_to = c(".value", "question_sub"), values_to = c("concept_id_sub")
    ) %>%
    dplyr::mutate(
      concept_id_for_sub = .data$concept_id_specific,
      concept_id_specific = .data$concept_id
    ) %>%
    dplyr::distinct(
      .data$question, .data$relative, .data$condition, .data$category, .data$concept_code, .data$concept_code_overall, .data$concept_id_specific,
      .data$concept_id_for_sub
    ) %>%
    dplyr::bind_rows(allofus::aou_health_history)

  # branching logic for how to handle concept_id vs. concept_code question inputs
  if (is.character(questions)) {
    # if its a character vector, go find the matching concept_ids because thats much faster
    # and easier to search with %in%
    regular_survey_qs <- questions[questions %in% allofus::aou_codebook$concept_code]
    health_survey_qs <- questions[questions %in% aou_health_history_long$concept_code]

    # did we account for everything?
    missing_qs <- questions[!questions %in% c(regular_survey_qs, health_survey_qs)]
    if (length(missing_qs) > 0) {
      cli::cli_abort(c(paste("Concept codes", paste(missing_qs, collapse = ", "), "not found in codebook."),
        "i" = "Check spelling and confirm that concept codes appear in the codebook at {.url https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html}."
      ))
    }

    # get the concept_ids for the regular survey questions
    regular_survey_concept_ids <- allofus::aou_codebook %>%
      dplyr::filter(.data$concept_code %in% regular_survey_qs) %>%
      dplyr::pull("concept_id") %>%
      unique()

    health_survey_concept_ids <- aou_health_history_long %>%
      dplyr::filter(.data$concept_code %in% health_survey_qs) %>%
      dplyr::pull("concept_id_specific")

    # there are some concept_ids in the regular codebook that are there for reference
    # not to search -- make sure those are not included
    too_general <- regular_survey_concept_ids[regular_survey_concept_ids %in% aou_health_history_long$concept_id_overall]
    if (length(too_general) > 0) {
      too_general <- regular_survey_qs[regular_survey_concept_ids %in% aou_health_history_long$concept_id_overall]
      cli::cli_abort(c(paste("Concept code(s) ", paste0(too_general, collapse = ", "), "is/are too general."),
        "i" = "Health history codes must refer to a specific condition and person pairing.",
        "i" = "Look for a specific condition in the health history codebook at {.url https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html}."
      ))
    }

    names_for_lookup <- dplyr::tibble(concept_code = questions, cn = !!question_output_arg)
  } else {
    # if its already numeric, just look
    regular_survey_concept_ids <- questions[questions %in% allofus::aou_codebook$concept_id]
    health_survey_concept_ids <- questions[questions %in% aou_health_history_long$concept_id_specific]

    # did we account for everything?
    missing_qs <- questions[!questions %in% c(regular_survey_concept_ids, health_survey_concept_ids)]
    if (length(missing_qs) > 0) {
      # check to see if any are in the health history codebook as overall concept ids
      too_general <- missing_qs[missing_qs %in% aou_health_history_long$concept_id_overall]
      if (length(too_general) > 0) {
        cli::cli_abort(c(paste("Concept ID(s) ", paste0(too_general, collapse = ", "), "is/are too general."),
          "i" = "Health history codes must refer to a specific condition and person pairing.",
          "i" = "Look for a specific condition in the health history codebook at {.url https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html}."
        ))
      } else {
        cli::cli_abort(c(paste("Concept ids", paste(missing_qs, collapse = ", "), "not found in codebook."),
          "i" = "Confirm that concept codes appear in the codebook at {.url https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html}."
        ))
      }
    }

    names_for_lookup <- dplyr::tibble(concept_id = questions, cn = !!question_output_arg)
  }

  if (length(health_survey_concept_ids) > 0) {
    # are there any that are the meds/treatment/age questions?
    sub_questions <- aou_health_history_long %>%
      dplyr::filter(.data$concept_id_specific %in% health_survey_concept_ids) %>%
      dplyr::filter(!is.na(.data$concept_id_for_sub))

    if (nrow(sub_questions) > 0) {
      # if there are some sub questions without the parents
      if (!all(sub_questions$concept_id_for_sub %in% health_survey_concept_ids)) {
        missing_qs <- sub_questions %>%
          dplyr::filter(!.data$concept_id_for_sub %in% health_survey_concept_ids)

        new_cols <- if (question_output == "concept_id") {
          paste0("`x", missing_qs$concept_id_for_sub, "`")
        } else {
          paste0("`", missing_qs$concept_code_overall, "`")
        }

        cli::cli_inform(
          c(
            "i" = "One or more of the requested questions were only asked of people who responded that they had certain conditions. ",
            ">" = "The top-level question(s) will be added to the output to provide context about missing data as column(s) {paste0(new_cols, collapse = ', ')}."
          )
        )

        # health_survey_concept_ids <- c(health_survey_concept_ids, missing_qs$concept_id_for_sub)
        names_for_lookup <- dplyr::bind_rows(
          names_for_lookup,
          dplyr::tibble(
            concept_id = missing_qs$concept_id_for_sub,
            concept_code = missing_qs$concept_code_overall,
            cn = missing_qs$concept_code_overall,
            type = "health"
          )
        )
      }
    }
  }

  regular_survey_concept_codes <- allofus::aou_codebook %>%
    dplyr::filter(.data$concept_id %in% regular_survey_concept_ids) %>%
    dplyr::distinct(.data$concept_code, .data$concept_id) %>%
    dplyr::mutate(type = "regular")

  health_survey_concept_codes <- aou_health_history_long %>%
    dplyr::filter(.data$concept_id_specific %in% health_survey_concept_ids) %>%
    dplyr::select("concept_code", concept_id = "concept_id_specific") %>%
    dplyr::distinct(.data$concept_code, .data$concept_id) %>%
    dplyr::mutate(type = "health")

  # there are more graceful ways to merge this data but ignoring that...
  suppressMessages({
    concept_lookup <- dplyr::bind_rows(regular_survey_concept_codes, health_survey_concept_codes) %>%
      dplyr::full_join(names_for_lookup) %>%
      dplyr::group_by(.data$concept_id) %>%
      tidyr::fill(dplyr::everything(), .direction = "up") %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
  })

  all_health_survey_concept_ids <- dplyr::filter(concept_lookup, .data$type == "health") %>%
    dplyr::pull("concept_id")

  health_survey_concept_ids <- all_health_survey_concept_ids[all_health_survey_concept_ids %in% allofus::aou_health_history$concept_id_specific]
  # the sub questions for the conditions can just be treated like regular survey questions
  regular_survey_concept_ids <- c(
    regular_survey_concept_ids,
    all_health_survey_concept_ids[!all_health_survey_concept_ids %in%
      allofus::aou_health_history$concept_id_specific]
  )

  if (length(health_survey_concept_ids) > 0) {
    all_health <- purrr::map(health_survey_concept_ids, ~ {
      specific_concept_id <- .x

      # this will be what the column is called
      if (question_output_arg[1] == "concept_id") {
        condition_name <- paste0("x", specific_concept_id)
      } else if (question_output_arg[1] == "concept_code") {
        condition_name <- concept_lookup %>%
          dplyr::filter(.data$concept_id == specific_concept_id) %>%
          dplyr::pull("concept_code")
      } else {
        condition_name <- concept_lookup %>%
          dplyr::filter(.data$concept_id == specific_concept_id) %>%
          dplyr::pull("cn")
      }

      condition_date <- paste0(condition_name, "_date")


      osci_specific <- aou_health_history_long %>%
        dplyr::filter(.data$concept_id_specific == specific_concept_id) %>%
        dplyr::pull("concept_id_question") %>%
        unique()

      osci_overall <- allofus::aou_health_history %>%
        dplyr::filter(.data$concept_id_specific == specific_concept_id) %>%
        dplyr::pull("concept_id_overall") %>%
        unique()

      if (length(osci_specific) == 0) {
        cli::cli_abort(c(paste("Concept id ", specific_concept_id, "is too general."),
          "i" = "Health history codes must refer to a specific condition and person pairing.",
          "i" = "Look for a specific condition in the health history codebook at {.url https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html}."
        ))
      }
      if (length(osci_specific) == 1 & !is.na(osci_overall)) { # this is not the case if an infectious disease question
        cli::cli_inform(c("i" = "The question associated with concept id {specific_concept_id} was added to the later version of the family health history survey so earlier All of Us participants may not have answered it."))
      }

      obs <- dplyr::tbl(con, "observation") %>%
        dplyr::inner_join(dplyr::select(function_cohort, "person_id"), by = "person_id") %>%
        dplyr::filter(.data$observation_source_concept_id %in% !!c(osci_overall, osci_specific)) %>%
        dplyr::select("person_id", "observation_source_concept_id", "value_source_concept_id", "value_source_value", "observation_date") %>%
        dplyr::mutate(type = dplyr::case_when(.data$observation_source_concept_id %in% osci_specific ~ "Specific", TRUE ~ "Overall")) %>%
        tidyr::pivot_wider(
          id_cols = c("person_id", "observation_date", "type"), names_from = "value_source_value",
          values_from = "value_source_concept_id"
        ) %>%
        dplyr::group_by(.data$person_id, .data$type) %>%
        dbplyr::window_order(.data$observation_date) %>%
        tidyr::fill(-c("person_id", "observation_date", "type"), .direction = "down") %>%
        dplyr::slice_max(order_by = observation_date, n = 1, with_ties = FALSE) %>%
        dplyr::ungroup()

      if (ncol(obs) == 3) {
        return(NULL)
      } # if there are no matching concepts

      if (isTRUE(clean_answers)) {
        pnta <- "PreferNotToAnswer"
        sk <- "Skip"
        dk <- "DontKnow"
      } else {
        pnta <- "PMI_PreferNotToAnswer"
        sk <- "PMI_Skip"
        dk <- "PMI_DontKnow"
      }

      obs %>%
        dplyr::mutate(condition = dplyr::case_when(
          dplyr::if_any(-c("person_id", "observation_date", "type"), ~ .x == !!specific_concept_id) ~ "Yes",
          .data$type == "Specific" & dplyr::if_any(-c("person_id", "observation_date", "type"), ~ !.x %in% c(903079, 903096, 903087)) ~ "No",
          dplyr::if_any(-c("person_id", "observation_date", "type"), ~ .x == 903079) ~ pnta,
          dplyr::if_any(-c("person_id", "observation_date", "type"), ~ .x == 903096) ~ sk,
          dplyr::if_any(-c("person_id", "observation_date", "type"), ~ .x == 903087) ~ dk,
          TRUE ~ "No"
        )) %>%
        dplyr::select("person_id", "type", "condition", "observation_date") %>%
        dplyr::group_by(.data$person_id) %>%
        dplyr::slice_max(order_by = type, n = 1) %>%
        dplyr::ungroup() %>%
        dplyr::select("person_id", !!condition_name := "condition", !!condition_date := "observation_date")
    }) %>%
      purrr::reduce(dplyr::left_join, by = "person_id")

    if (!is.null(all_health)) {
      cohort_w_health <- dplyr::left_join(function_cohort, all_health, by = "person_id")
    } else {
      cli::cli_inform(c("x" = "No data found for health history questions."))
      cohort_w_health <- function_cohort
    }
  } else {
    cohort_w_health <- function_cohort
  }

  ## this is the part for the regular survey questions, from the original function
  if (length(regular_survey_concept_ids) > 0) {
    # temporary observation table with responses
    tmp <- dplyr::tbl(con, "observation") %>%
      dplyr::filter(.data$observation_source_concept_id %in% regular_survey_concept_ids) %>%
      # this is necessary because there may be multiple rows for a single person (hence full_join later)
      dplyr::inner_join(dplyr::select(function_cohort, "person_id"), by = "person_id")

    # for retrieving columns and pivoting
    q <- paste0("observation_source_", question_output)

    # need a prefix to fix column names if using concept_id as column names
    if (question_output == "concept_id") {
      pref <- "x"
    } else {
      pref <- ""
    }

    if (isTRUE(clean_answers)) {
      tmp <- dplyr::mutate(tmp,
        value_source_value = dplyr::case_when(
          CONTAINS_SUBSTR(.data$value_source_value, "cope_") ~ value_source_value,
          CONTAINS_SUBSTR(.data$value_source_value, "SDOH_") ~ value_source_value,
          !CONTAINS_SUBSTR(.data$value_source_value, "_") ~ value_source_value,
          TRUE ~ REGEXP_EXTRACT(.data$value_source_value, ".+_(.+_*.*)")
        )
      )
    }

    # go wide
    wide <- tmp %>%
      # numeric answers are stored in value_as_number
      dplyr::mutate(value_source_value = dplyr::coalesce(.data$value_source_value, CAST(dplyr::sql("value_as_number AS STRING")))) %>%
      # first combine all rows for a single person and question (e.g., multiple races)
      dplyr::group_by(.data$person_id, .data$observation_date, dplyr::across(dplyr::all_of(q))) %>%
      dplyr::summarise(
        value_source_value = STRING_AGG(dplyr::sql("value_source_value order by value_source_value")),
        .groups = "drop"
      ) %>%
      dplyr::select(dplyr::all_of(c("person_id", !!q, "value_source_value", "observation_date"))) %>%
      tidyr::pivot_wider(names_from = !!q, values_from = c("value_source_value", "observation_date"), names_prefix = pref)

    if (length(question_output_arg) == 1 && question_output_arg[1] %in% c("concept_code", "concept_id")) {
      wide <- wide %>%
        dplyr::rename_with(.fn = gsub, pattern = "value_source_value_|value_source_concept_id", replacement = "") %>%
        dplyr::rename_with(.fn = gsub, pattern = "observation_date_(.+)", replacement = "\\1_date")
    } else {
      # named vector to rename the columns if needed
      concept_codes <- concept_lookup %>%
        dplyr::filter(.data$concept_id %in% regular_survey_concept_ids) %>%
        dplyr::select("concept_code", "cn")
      nm <- c(
        paste0("value_source_value_", concept_codes$concept_code),
        paste0("observation_date_", concept_codes$concept_code)
      )
      names(nm) <- c(concept_codes$cn, paste0(concept_codes$cn, "_date"))
      if (!all(nm %in% tolower(colnames(wide)))) {
        cli::cli_abort(c("Wrong column names.",
          "i" = "Try one question at a time, and report this issue at {.url https://github.com/roux-ohdsi/allofus/issues}"
        ))
      }

      # change to lower, then rename
      wide <- wide %>%
        dplyr::rename_with(tolower) %>%
        dplyr::rename(dplyr::all_of(nm))
    }
    # join back to original table
    out <- dplyr::full_join(cohort_w_health, wide, by = "person_id")
  } else { # end regular survey questions
    out <- cohort_w_health
  }

  # a little reorganization
  out <- out %>%
    dplyr::relocate(dplyr::ends_with("_date") & !dplyr::any_of(colnames(cohort)), .after = dplyr::last_col())

  # collect if indicated
  if (isTRUE(collect)) {
    out <- dplyr::collect(out, ...)
  }

  return(out)
}
