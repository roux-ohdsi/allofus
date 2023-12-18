library(dplyr)
con <- allofus:::aou_test_connect()
cohort <- tbl(con, "person") %>% select(person_id)

# test that the function throws an error if person_id column is not found in cohort
test_that("aou_survey throws error if person_id column not found in cohort", {
  cohort_no_person_id <- data.frame(
    year_of_birth = c(1990, 1985, 1975),
    gender_concept_id = c(1, 2, 1)
  )
  expect_error(
    aou_survey(cohort_no_person_id, c(1, 2, 3), "text", con, FALSE),
    "person_id column not found in cohort data"
  )
})

# test that the function returns the expected output when using concept_id for question_output
test_that("aou_survey returns expected output when using concept_id for question_output", {
  result <- aou_survey(cohort, 43529932, question_output = "concept_id")
})
