#' Function to query allofus observation table for survey responses
#'
#' @description
#' Because responses to the survey questions stored in the ds_survey table do not
#' include skipped questions (i.e., missing data!), this function makes it easier to
#' query the observation table for responses to survey questions so that the skipped
#' responses are included. At the moment, it only works for relatively straightforward
#' questions without extensive branching logic. It also does not work for some
#' questions in the personal/family/health/medical surveys that ask participants
#' to check a box for each disease.
#'
#' The function will return a dataframe or SQL tbl with the initial cohort table along
#' with a column for each question included in `questions` and answers for
#' each person_id in the cells. The column names (questions) and answers can
#' be returned as the concept_code or concept_id.
#'
#' To find the desired survey questions, use the all of us data dictionary,
#' survey codebook, athena, data browser, or the allofus R package
#' modified codebook which can be found here: https://roux-ohdsi.github.io/allofus/articles/searchable_codebook.html
#'
#' Users may want to do some post-processing. For example PMI_skip or 903096
#' could be marked as NA.
#'
#' @param cohort tbl or dataframe with a cohort that includes a column called person_id
#' @param questions either a vector of concept_ids or concept_codes for questions to return results
#' @param con connection to the allofus SQL database. Defaults to getOption("aou.default.con"), which is created automatically with `aou_connect()`
#' @param collect whether to return the results as a local (TRUE) or database table
#' @param answer_output whether to return the survey responses in their text format ("text") or as the concept id
#' for a given response ("concept_id"). Defaults to "text".
#' @param question_output how to name the columns. Options include text format ("text"), as concept ids preceded by
#' "x_" ("concept_id"), or using a custom vector of column names matching the vector of `questions`. Defaults to "text".
#'
#' @export
#' @examples
#' \dontrun{
#' con <- aou_connect()
#' cohort <- tbl(con, "person") %>%
#'   filter(person_id > 5000000) %>%
#'   select(person_id, year_of_birth, gender_concept_id)
#' aou_survey(
#'   cohort = cohort,
#'   questions = c(1585375, 1586135),
#'   answer_output = "text",
#'   question_output = "text"
#' )
#' aou_survey(cohort, questions = 1585811, question_output = "pregnancy")
#' aou_survey(cohort, questions = 1585811, question_output = "text")
#' aou_survey(cohort, questions = 1585811, question_output = "concept_id")
#' aou_survey(cohort, questions = c(1585811, 1585386), question_output = c("pregnancy", "insurance"))
#' aou_survey(cohort, questions = c(1585811, 1585386), question_output = "text")
#' aou_survey(cohort, questions = c(1585811, 1585386), question_output = "concept_id")
#' }
aou_survey <- function(cohort,
                       questions,
                       con = getOption("aou.default.con"),
                       collect = TRUE,
                       answer_output = "text",
                       question_output = "text") {
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
  question_output <- if (length(question_output_arg) == 1 & question_output_arg[1] == "concept_id") "concept_id" else "value"

  answer_output <- match.arg(answer_output, c("text", "concept_id"))
  answer_output <- ifelse(answer_output == "text", "value", answer_output)

  # ensure person_id is a column name in cohort
  stopifnot("person_id not found in cohort data" = "person_id" %in% colnames(cohort))

  if (is.data.frame(cohort)) {
    function_cohort <- tbl(con, "person") %>%
      filter(person_id %in% !!cohort$person_id) %>%
      select(person_id)
  } else {
    function_cohort <- cohort
  }

  # branching logic for how to handle concept_id vs. concept_code question inputs
  if (is.character(questions)) {
    # if its a character vector, go find the matching concept_ids because thats much faster
    # and easier to search with %in%
    concept_ids <- aou_codebook %>%
      filter(concept_code %in% questions) %>%
      pull(concept_id)
    stopifnot("no character questions found" = length(concept_ids) != 0)
    stopifnot("character question not in questions" = all(concept_ids %in% aou_codebook$concept_id))
  } else {
    # if its already numeric, just look
    # as long as we're good, then
    stopifnot("question concept id not in questions" = all(questions %in% aou_codebook$concept_id))
    concept_ids <- questions
  }

  # temporary observation table with responses
  tmp <- tbl(con, "observation") %>% filter(observation_source_concept_id %in% concept_ids)

  # for retrieving columns and pivoting
  q <- paste0("observation_source_", question_output)
  a <- paste0("value_source_", answer_output)

  # need a prefix to fix column names if using concept_id as column names
  if (question_output == "concept_id") {
    pref <- "x"
  } else {
    pref <- ""
  }

  # go wide
  wide <- tmp %>%
    mutate(!!a := coalesce(!!a, as.character(value_as_number))) %>%
    select(all_of(c("person_id", !!q, !!a, "observation_date"))) %>%
    pivot_wider(names_from = !!q, values_from = c(!!a, observation_date), names_prefix = pref)

  if (length(question_output_arg) == 1 & question_output_arg[1] %in% c("text", "concept_id")) {
    wide <- wide %>%
      rename_with(.fn = str_remove, pattern = "value_source_value_|value_source_concept_id") %>%
      rename_with(.fn = str_replace, pattern = "observation_date_(.+)", replacement = "\\1_date")
  } else {
    new_names <- setNames(names(wide), c("person_id", question_output_arg, paste0(question_output_arg, "_date")))
    wide <- wide %>% rename(all_of(new_names))
  }

  # join back to original table
  out <- left_join(function_cohort, wide, by = "person_id")

  # collect if indicated
  if (isTRUE(collect)) {
    out <- collect(out)
  }

  return(out)
}
