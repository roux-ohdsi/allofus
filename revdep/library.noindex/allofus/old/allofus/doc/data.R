## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(allofus)
#  library(tidyverse)
#  
#  svy_vars <- aou_survey(questions = c(1585838, 1586135), question_output = c("gender", "birthplace"))

## ----echo = FALSE, eval = TRUE------------------------------------------------
cli::cli_warn(c("No cohort provided.", ">" = "Pulling survey data for entire All of Us cohort."))

## -----------------------------------------------------------------------------
#  count(svy_vars, gender, birthplace)

## -----------------------------------------------------------------------------
#  cohort <- svy_vars %>%
#    filter(gender == "Woman", birthplace == "Other")
#  
#  cohort

## -----------------------------------------------------------------------------
#  t2dm <- aou_concept_set(cohort,
#    concepts = c(201826, 4193704),
#    domains = "condition", output = "indicator",
#    concept_set_name = "t2dm"
#  )

## -----------------------------------------------------------------------------
#  metformin <- aou_concept_set(cohort,
#    concepts = c(40164929, 40164897),
#    domains = "drug", output = "count",
#    start_date = "gender_date", concept_set_name = "metformin"
#  )

## -----------------------------------------------------------------------------
#  a1c <- aou_concept_set(cohort,
#    concepts = c(3004410, 3005673),
#    domains = "measurement", output = "all", start_date = "gender_date"
#  )

## -----------------------------------------------------------------------------
#  a1c

## -----------------------------------------------------------------------------
#  t2dm_self <- aou_survey(cohort, questions = 43529932, question_output = "t2dm_survey")

## -----------------------------------------------------------------------------
#  combined_data <- reduce(list(cohort, t2dm, metformin, a1c, t2dm_self),
#    aou_join,
#    type = "left"
#  )

## -----------------------------------------------------------------------------
#  final_data <- combined_data %>%
#    group_by(person_id, birthplace, t2dm, metformin, t2dm_survey) %>%
#    summarize(max_a1c = max(value_as_number, na.rm = TRUE), .groups = "drop")

## -----------------------------------------------------------------------------
#  final_data

