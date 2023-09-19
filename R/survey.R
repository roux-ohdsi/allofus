
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
#' @param con connection to the allofus SQL database.
#' @param cohort tbl or dataframe with a cohort that includes a column called person_id
#' @param questions either a vector of concept_ids or concept_codes for questions to return results
#' @param collect whether to return the results as a local (TRUE) or database table
#' @param answer_output whether to return the survey responses in their text format (value) or concept_id
#' @param question_output hether to return the survey questions (columns) in their text format (value) or concept_id
#'
#' @export
#' @examples
#' \dontrun{
#' con <- aou_connect()
#' cohort = tbl(con, "person") %>% filter(person_id > 5000000) %>% select(person_id, year_of_birth, gender_concept_id)
#' aou_survey(con = con,
#' cohort = cohort,
#' questions = c(1585375, 1586135),
#' answer_output = "value",
#' question_output = "value")
#' }
aou_survey <- function(con,
                       cohort,
                       questions,
                       collect = TRUE,
                       answer_output = c("value", "concept_id"),
                       question_output = c("value", "concept_id")){

  # ensure person_id is a column name in cohort
  stopifnot("person_id not found in cohort data" = "person_id" %in% colnames(cohort))

  if(is.data.frame(cohort)){
    function_cohort = tbl(con, "person_id") %>%
      filter(person_id %in% !!cohort$person_id)
  } else {
    function_cohort = cohort
  }

  # branching logic for how to handle concept_id vs. concept_code question inputs
  if(is.character(questions)){

    # if its a character vector, go find the matching concept_ids because thats much faster
    # and easier to search with %in%
    concept_ids = aou_codebook %>% filter(concept_code %in% questions) %>% pull(concept_id)
    stopifnot("no character questions found" = length(concept_ids) != 0)
    stopifnot("character question not in questions" = all(concept_ids %in% aou_codebook$concept_id))

  } else {

    # if its already numeric, just look
    # as long as we're good, then
    stopifnot("question concept id not in questions" = all(questions %in% aou_codebook$concept_id))
    concept_ids = questions
  }

  # temporary observation table with responses
  tmp = tbl(con, "observation") %>% filter(observation_source_concept_id %in% concept_ids)

  # for retrieving columns and pivoting
  q = paste0('observation_source_', question_output)
  a = paste0('value_source_', answer_output)

  # need a prefix to fix column names if using concept_id as column names
  if(question_output == "concept_id"){pref = "x"} else {pref = ""}

  # go wide
  wide = tmp %>%
    select(all_of(c('person_id', !!q, !!a))) %>%
    pivot_wider(names_from = !!q, values_from = !!a, names_prefix = pref)

  # join back to original table
  out = inner_join(function_cohort, wide, by = "person_id")

  # collect if indicated
  if(isTRUE(collect)){
    out = collect(out)
  }

  return(out)
}
