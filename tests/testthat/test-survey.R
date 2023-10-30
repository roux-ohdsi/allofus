# create a mock cohort dataframe
cohort <- data.frame(person_id = c(1, 2, 3),
                     year_of_birth = c(1990, 1985, 1975),
                     gender_concept_id = c(1, 2, 1))

# create a mock connection to the database
con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

# create a mock observation table
DBI::dbWriteTable(con, "observation", data.frame(person_id = c(1, 1, 2, 2, 3),
                                                  concept_id = c(1, 2, 1, 3, 2),
                                                  value = c("Yes", "No", "Skip", "Yes", "No"),
                                                  observation_date = c("2021-01-01", "2021-01-02", "2021-01-03", "2021-01-04", "2021-01-05")))

# test that the function returns the expected output
test_that("aou_survey returns expected output", {
  result <- aou_survey(cohort, c(1, 2, 3), "text", con, FALSE)
  expect_equal(result$person_id, c(1, 2, 3))
  expect_equal(result$`1`, c("Yes", "Skip", NA))
  expect_equal(result$`2`, c("No", "Yes", "No"))
  expect_equal(result$`3`, c(NA, NA, "Skip"))
  expect_equal(result$`1_date`, c("2021-01-01", "2021-01-03", NA))
  expect_equal(result$`2_date`, c("2021-01-02", "2021-01-04", "2021-01-05"))
  expect_equal(result$`3_date`, c(NA, NA, "2021-01-05"))
})

# test that the function throws an error if person_id column is not found in cohort
test_that("aou_survey throws error if person_id column not found in cohort", {
  cohort_no_person_id <- data.frame(year_of_birth = c(1990, 1985, 1975),
                                    gender_concept_id = c(1, 2, 1))
  expect_error(aou_survey(cohort_no_person_id, c(1, 2, 3), "text", con, FALSE),
               "person_id column not found in cohort data")
})

# test that the function throws an error if length of question_output doesn't match questions
test_that("aou_survey throws error if length of question_output doesn't match questions", {
  expect_error(aou_survey(cohort, c(1, 2, 3), c("text", "concept_id"), con, FALSE),
               "Length of argument `question_output` doesn't match `questions`.")
})

# test that the function returns the expected output when using concept_id for question_output
test_that("aou_survey returns expected output when using concept_id for question_output", {
  result <- aou_survey(cohort, c(1, 2, 3), "concept_id", con, FALSE)
  expect_equal(result$person_id, c(1, 2, 3))
  expect_equal(result$`x_1`, c("Yes", "Skip", NA))
  expect_equal(result$`x_2`, c("No", "Yes", "No"))
  expect_equal(result$`x_3`, c(NA, NA, "Skip"))
  expect_equal(result$`x_1_date`, c("2021-01-01", "2021-01-03", NA))
  expect_equal(result$`x_2_date`, c("2021-01-02", "2021-01-04", "2021-01-05"))
  expect_equal(result$`x_3_date`, c(NA, NA, "2021-01-05"))
})