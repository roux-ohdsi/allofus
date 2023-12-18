# test that the function throws an error if person_id column is not found in cohort
test_that("aou_survey throws error if person_id column not found in cohort", {
  cohort_no_person_id <- data.frame(
    year_of_birth = c(1990, 1985, 1975),
    gender_concept_id = c(1, 2, 1)
  )
  expect_error(
    aou_survey(cohort_no_person_id, questions = 43529932),
    "person_id"
  )
})

# test that the function returns the expected output when using concept_id for question_output
test_that("aou_survey returns expected output question_output", {
  result_id <- aou_survey(cohort, 43529932, question_output = "concept_id")
  result_code <- aou_survey(cohort, 43529932, question_output = "concept_code")
  result <- aou_survey(cohort, 43529932, question_output = "t2dm")
  expect_equal(colnames(result_id), c("person_id", "x43529932", "x43529932_date"))
  expect_equal(colnames(result_code), c("person_id", "endocrinecondition_type2diabetes_yes", "endocrinecondition_type2diabetes_yes_date"))
  expect_equal(colnames(result), c("person_id", "t2dm", "t2dm_date"))
})
