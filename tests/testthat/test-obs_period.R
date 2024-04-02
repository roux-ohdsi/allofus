test_that("aou_observation_period returns expected output", {
  skip_workbench()

  expect_warning(aou_observation_period(), "No cohort provided")

  expect_no_warning(aou_observation_period(cohort = data.frame(person_id = 2150822)))

  expect_equal(nrow(aou_observation_period(cohort = data.frame(person_id = 2011381), collect = TRUE, n = 2)), 2)
})
