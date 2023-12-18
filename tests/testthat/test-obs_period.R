# Define a test dataset
test_data <- tibble(
  person_id = c(1, 1, 1, 2, 2, 3, 3, 3),
  visit_start_date = as.Date(c("2020-01-01", "2020-02-01", "2020-03-01", "2020-01-01", "2020-02-01", "2020-01-01", "2020-02-01", "2020-03-01")),
  visit_end_date = as.Date(c("2020-01-15", "2020-02-15", "2020-03-15", "2020-01-15", "2020-02-15", "2020-01-15", "2020-02-15", "2020-03-15")),
  visit_type_concept_id = c(44818519, 44818519, 44818519, 0, 0, 0, 0, 0)
)

# Define the test cases
test_that("aou_observation_period returns correct output", {
  # Test with default arguments
  obs_period <- aou_observation_period(test_data)
  expect_equal(nrow(obs_period), 3)
  expect_equal(obs_period$person_id, c(1, 2, 3))
  expect_equal(obs_period$observation_start_date, as.Date(c("2020-01-01", "2020-01-01", "2020-01-01")))
  expect_equal(obs_period$observation_end_date, as.Date(c("2020-03-15", "2020-02-15", "2020-03-15")))

  # Test with exclude_aou_visits = TRUE
  obs_period <- aou_observation_period(test_data, exclude_aou_visits = TRUE)
  expect_equal(nrow(obs_period), 2)
  expect_equal(obs_period$person_id, c(2, 3))
  expect_equal(obs_period$observation_start_date, as.Date(c("2020-01-01", "2020-01-01")))
  expect_equal(obs_period$observation_end_date, as.Date(c("2020-02-15", "2020-03-15")))

  # Test with collect = TRUE
  obs_period <- aou_observation_period(test_data, collect = TRUE)
  expect_is(obs_period, "data.frame")
})
