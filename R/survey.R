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
#' @param question_output how to name the columns. Options include text format ("text"), as concept ids preceded by
#' "x_" ("concept_id"), or using a custom vector of column names matching the vector of `questions`. Defaults to "text".
#' @param clean_answers whether to clean the answers to the survey questions. Defaults to TRUE.
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is created automatically with `aou_connect()`
#' @param collect whether to return the results as a local (TRUE) or database table
#'
#' @importFrom dplyr filter select pull mutate rename rename_with collect tbl left_join coalesce
#' @importFrom tidyr pivot_wider all_of pivot_longer fill
#' @importFrom stringr str_replace str_remove
#' @importFrom purrr reduce map
#' @export
#' @examples
#' \dontrun{
#' con <- aou_connect()
#' cohort <- tbl(con, "person") %>%
#'   filter(person_id > 5000000) %>%
#'   select(person_id, year_of_birth, gender_concept_id)
#'
#' aou_survey(
#'   cohort,
#'   questions = c(1585375, 1586135),
#'   question_output = "text"
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
#' }
aou_survey <- function(cohort,
                       questions,
                       question_output = "text",
                       clean_answers = TRUE,
                       con = getOption("aou.default.con"),
                       collect = TRUE) {
  if (is.null(con)) stop("Please provide a connection to the database. You can do so automatically by running `aou_connect()` before this function.")


  if (length(question_output) == length(questions)) { # either gave column names or happen to have a single question
    if (length(question_output) == 1) { # gave one column name or "text" or "concept_id"
      # see if the matched argument matches the actual argument
      question_output_arg <- tryCatch(match.arg(question_output, c("text", "concept_id")), error = function(e) question_output)
      # if not, print a message so that the user can spot their problem
      if (!identical(question_output, question_output_arg)) {
        message("Using ", question_output, " as column name. Did you really mean ", question_output_arg, "?")
        question_output_arg <- question_output
      }
    } else {
      question_output_arg <- question_output
    }
    # didn't give the right number question_output and it doesn't match one of the options
  } else if (is.null(tryCatch(match.arg(question_output, c("text", "concept_id")), error = function(e) NULL))) {
    stop("Length of argument `question_output` doesn't match `questions`.")
    # gave one value for question_output and multiple values for questions
  } else {
    question_output_arg <- match.arg(question_output, c("text", "concept_id"))
  }
  question_output <- if (length(question_output_arg) == 1 && question_output_arg[1] == "concept_id") "concept_id" else "value"

  answer_output <- "value"

  # ensure person_id is a column name in cohort
  stopifnot("person_id column not found in cohort data" = "person_id" %in% colnames(cohort))

  if (is.data.frame(cohort)) {
    function_cohort <- tbl(con, "person") %>%
      filter(person_id %in% !!unique(cohort$person_id)) %>%
      select(person_id)
  } else {
    function_cohort <- cohort
  }

  # pivot longer to inclde the rx, on_txt, and age_diagnosis columns
  health_history_codebook_long <- allofus::health_history_codebook %>%
    filter(relative == "self") %>%
    rename(concept_code_overall = concept_code) %>%
    pivot_longer(
      cols = c(
        concept_id_rx_meds, concept_id_on_txt, concept_id_age_diagnosis,
        concept_code_rx_meds, concept_code_on_txt, concept_code_age_diagnosis
      ),
      names_pattern = "(.+_.+)_(.+_.+)",
      names_to = c(".value", "question_sub"), values_to = c("concept_id_sub")
    ) %>%
    mutate(
      concept_id_for_sub = concept_id_specific,
      concept_id_specific = concept_id
    ) %>%
    distinct(
      question, relative, condition, category, concept_code, concept_code_overall, concept_id_specific,
      concept_id_for_sub
    ) %>%
    bind_rows(health_history_codebook)

  # branching logic for how to handle concept_id vs. concept_code question inputs
  if (is.character(questions)) {
    # if its a character vector, go find the matching concept_ids because thats much faster
    # and easier to search with %in%
    regular_survey_qs <- questions[questions %in% allofus::aou_codebook$concept_code]
    health_survey_qs <- questions[questions %in% health_history_codebook_long$concept_code]

    # did we account for everything?
    missing_qs <- questions[!questions %in% c(regular_survey_qs, health_survey_qs)]
    if (length(missing_qs) > 0) stop("can't find all concept codes, check codebook")

    # get the concept_ids for the regular survey questions
    regular_survey_concept_ids <- allofus::aou_codebook %>%
      filter(concept_code %in% regular_survey_qs) %>%
      pull(concept_id) %>%
      unique()

    health_survey_concept_ids <- health_history_codebook_long %>%
      filter(concept_code %in% health_survey_qs) %>%
      pull(concept_id_specific)

    # there are some concept_ids in the regular codebook that are there for reference
    # not to search -- make sure those are not included
    too_general <- regular_survey_concept_ids[regular_survey_concept_ids %in% health_history_codebook_long$concept_id_overall]
    if (length(too_general) > 0) {
      too_general <- regular_survey_qs[regular_survey_concept_ids %in% health_history_codebook_long$concept_id_overall]
      stop(
        "Concept code(s) ", paste0(too_general, collapse = ", "),
        " is/are too general. Find a concept id for a specific condition and individual in the health history codebook. ",
        "See function documentation for more details."
      )
    }

    names_for_lookup <- tibble(concept_code = questions, cn = !!question_output_arg)
  } else {
    # if its already numeric, just look
    regular_survey_concept_ids <- questions[questions %in% allofus::aou_codebook$concept_id]
    health_survey_concept_ids <- questions[questions %in% health_history_codebook_long$concept_id_specific]

    # did we account for everything?
    missing_qs <- questions[!questions %in% c(regular_survey_concept_ids, health_survey_concept_ids)]
    if (length(missing_qs) > 0) {
      # check to see if any are in the health history codebook as overall concept ids
      too_general <- missing_qs[missing_qs %in% health_history_codebook_long$concept_id_overall]
      if (length(too_general) > 0) {
        stop(
          "Concept ID(s) ", paste0(too_general, collapse = ", "),
          " is/are too general. Look for a specific condition in the health history codebook.",
          "See function documentation for more details."
        )
      } else {
        stop("can't find all concept ids, check codebook")
      }
    }

    names_for_lookup <- tibble(concept_id = questions, cn = !!question_output_arg)
  }

  if (length(health_survey_concept_ids) > 0) {
    # are there any that are the meds/treatment/age questions?
    sub_questions <- health_history_codebook_long %>%
      filter(concept_id_specific %in% health_survey_concept_ids) %>%
      filter(!is.na(concept_id_for_sub))

    if (nrow(sub_questions) > 0) {
      # if there are some sub questions without the parents
      if (!all(sub_questions$concept_id_for_sub %in% health_survey_concept_ids)) {
        missing_qs <- sub_questions %>%
          filter(!concept_id_for_sub %in% health_survey_concept_ids)

        new_cols <- if (question_output == "concept_id") {
          paste0("`x", missing_qs$concept_id_for_sub, "`")
        } else {
          paste0("`", missing_qs$concept_code_overall, "`")
        }

        message(
          "One or more of the requested questions were only asked of people responded that they had certain conditions. ",
          "The top-level question(s) will be added to the output to provide context about missing data as ",
          "column(s) ", paste0(new_cols, collapse = ", "), "."
        )

        # health_survey_concept_ids <- c(health_survey_concept_ids, missing_qs$concept_id_for_sub)
        names_for_lookup <- bind_rows(
          names_for_lookup,
          tibble(
            concept_id = missing_qs$concept_id_for_sub, concept_code =
              missing_qs$concept_code_overall, cn = missing_qs$concept_code_overall,
            type = "health"
          )
        )
      }
    }
  }

  regular_survey_concept_codes <- allofus::aou_codebook %>%
    filter(concept_id %in% regular_survey_concept_ids) %>%
    distinct(concept_code, concept_id) %>%
    mutate(type = "regular")

  health_survey_concept_codes <- health_history_codebook_long %>%
    filter(concept_id_specific %in% health_survey_concept_ids) %>%
    select(concept_code, concept_id = concept_id_specific) %>%
    distinct(concept_code, concept_id) %>%
    mutate(type = "health")

  # there are more graceful ways to merge this data but ignoring that...
  suppressMessages({
    concept_lookup <- bind_rows(regular_survey_concept_codes, health_survey_concept_codes) %>%
      full_join(names_for_lookup) %>%
      group_by(concept_id) %>%
      fill(everything(), .direction = "up") %>%
      slice(1) %>%
      ungroup()
  })

  all_health_survey_concept_ids <- filter(concept_lookup, type == "health") %>%
    pull(concept_id)

  health_survey_concept_ids <- all_health_survey_concept_ids[all_health_survey_concept_ids %in% health_history_codebook$concept_id_specific]
  # the sub questions for the conditions can just be treated like regular survey questions
  regular_survey_concept_ids <- c(
    regular_survey_concept_ids,
    all_health_survey_concept_ids[!all_health_survey_concept_ids %in%
      health_history_codebook$concept_id_specific]
  )

  if (length(health_survey_concept_ids) > 0) {
    all_health <- map(health_survey_concept_ids, ~ {
      specific_concept_id <- .x

      # this will be what the column is called
      if (question_output_arg[1] == "concept_id") {
        condition_name <- paste0("x", specific_concept_id)
      } else if (question_output_arg[1] == "text") {
        condition_name <- concept_lookup %>%
          filter(concept_id == specific_concept_id) %>%
          pull(concept_code)
      } else {
        condition_name <- concept_lookup %>%
          filter(concept_id == specific_concept_id) %>%
          pull(cn)
      }

      condition_date <- paste0(condition_name, "_date")


      osci_specific <- health_history_codebook_long %>%
        filter(concept_id_specific == specific_concept_id) %>%
        pull(concept_id_question) %>%
        unique()

      osci_overall <- allofus::health_history_codebook %>%
        filter(concept_id_specific == specific_concept_id) %>%
        pull(concept_id_overall) %>%
        unique()

      if (length(osci_specific) == 0) {
        stop(
          "Concept id ", specific_concept_id,
          " is too general. Look for a specific condition in the health history codebook.",
          "See function documentation for more details."
        )
      }
      if (length(osci_specific) == 1 & !is.na(osci_overall)) { # this is not the case if an infectious disease question
        warning(
          "The question associated with concept id ",
          specific_concept_id, " was added to the later version of the",
          " family health history survey so earlier All of Us participants may not have answered it."
        )
      }

      obs <- tbl(con, "observation") %>%
        inner_join(select(function_cohort, person_id), by = "person_id") %>%
        filter(observation_source_concept_id %in% !!c(osci_overall, osci_specific)) %>%
        select(person_id, observation_source_concept_id, value_source_concept_id, value_source_value, observation_date) %>%
        mutate(type = case_when(observation_source_concept_id %in% osci_specific ~ "Specific", TRUE ~ "Overall")) %>%
        pivot_wider(
          id_cols = c(person_id, observation_date, type), names_from = value_source_value,
          values_from = value_source_concept_id
        ) %>%
        group_by(person_id, type) %>%
        dbplyr::window_order(observation_date) %>%
        fill(-c(person_id, observation_date, type), .direction = "down") %>%
        slice_max(order_by = observation_date, n = 1, with_ties = FALSE) %>%
        ungroup()

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
        mutate(condition = case_when(
          if_any(-c(person_id, observation_date, type), ~ .x == !!specific_concept_id) ~ "Yes",
          type == "Specific" & if_any(-c(person_id, observation_date, type), ~ !.x %in% c(903079, 903096, 903087)) ~ "No",
          if_any(-c(person_id, observation_date, type), ~ .x == 903079) ~ pnta,
          if_any(-c(person_id, observation_date, type), ~ .x == 903096) ~ sk,
          if_any(-c(person_id, observation_date, type), ~ .x == 903087) ~ dk,
          TRUE ~ "No"
        )) %>%
        select(person_id, type, condition, observation_date) %>%
        group_by(person_id) %>%
        slice_max(order_by = type, n = 1) %>%
        ungroup() %>%
        select(person_id, !!condition_name := condition, !!condition_date := observation_date)
    }) %>%
      reduce(left_join, by = "person_id")

    if (!is.null(all_health)) {
      cohort_w_health <- left_join(function_cohort, all_health, by = "person_id")
    } else {
      warning("No data found for health history questions.")
      cohort_w_health <- function_cohort
    }
  } else {
    cohort_w_health <- function_cohort
  }

  ## this is the part for the regular survey questions, from the original function
  if (length(regular_survey_concept_ids) > 0) {
    # temporary observation table with responses
    tmp <- tbl(con, "observation") %>%
      filter(observation_source_concept_id %in% regular_survey_concept_ids) %>%
      # this is necessary because there may be multiple rows for a single person (hence full_join later)
      inner_join(select(function_cohort, person_id), by = "person_id")

    # for retrieving columns and pivoting
    q <- paste0("observation_source_", question_output)
    a <- paste0("value_source_", answer_output)

    # need a prefix to fix column names if using concept_id as column names
    if (question_output == "concept_id") {
      pref <- "x"
    } else {
      pref <- ""
    }

    if (isTRUE(clean_answers)) {
      tmp <- mutate(tmp,
        value_source_value = case_when(
          contains_substr(value_source_value, "cope_") ~ value_source_value,
          contains_substr(value_source_value, "SDOH_") ~ value_source_value,
          !contains_substr(value_source_value, "_") ~ value_source_value,
          TRUE ~ regexp_extract(value_source_value, ".+_(.+_*.*)")
        )
      )
    }

    # go wide
    wide <- tmp %>%
      mutate(!!a := coalesce(!!rlang::ensym(a), CAST(sql("value_as_number AS STRING")))) %>%
      select(all_of(c("person_id", !!q, !!a, "observation_date"))) %>%
      mutate(observation_date = as.character(observation_date)) %>%
      pivot_wider(names_from = !!q, values_from = c(!!a, observation_date),
                  names_prefix = pref, values_fn = ~STRING_AGG(.x,  ", ")) %>%
      mutate(across(contains("_date_"), ~as.Date(substr(.x, 1L, 10L))))

    if (length(question_output_arg) == 1 && question_output_arg[1] %in% c("text", "concept_id")) {
      wide <- wide %>%
        rename_with(.fn = str_remove, pattern = "value_source_value_|value_source_concept_id") %>%
        rename_with(.fn = str_replace, pattern = "observation_date_(.+)", replacement = "\\1_date")
    } else {
      # named vector to rename the columns if needed
      concept_codes <- concept_lookup %>%
        filter(concept_id %in% regular_survey_concept_ids) %>%
        select(concept_code, cn)
      nm <- c(
        paste0("value_source_value_", concept_codes$concept_code),
        paste0("observation_date_", concept_codes$concept_code)
      )
      names(nm) <- c(concept_codes$cn, paste0(concept_codes$cn, "_date"))
      stopifnot("Wrong column names" = all(nm %in% tolower(colnames(wide))))
      # change to lower, then rename
      wide <- wide %>%
        rename_with(tolower) %>%
        rename(all_of(nm))
    }
    # join back to original table
    out <- full_join(cohort_w_health, wide, by = "person_id")
  } else { # end regular survey questions
    out <- cohort_w_health
  }

  # a little reorganization
  out <- out %>%
    relocate(ends_with("_date") & !any_of(colnames(cohort)), .after = last_col())

  # collect if indicated
  if (isTRUE(collect)) {
    out <- collect(out)
  }

  return(out)
}
