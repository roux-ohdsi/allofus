# returns warning
test_that("aou_concept_set() returns warning when concept set name is not provided", {
  skip_workbench()

  expect_warning(
    aou_concept_set(concepts = 4324693, domain = "procedure", output = "count")
  )
})

# returns 13
test_that("aou_concept_set() returns counts correctly", {
  skip_workbench()

  expect_gt(
    {
      suppressWarnings(
        dplyr::tbl(con, "person") %>%
        dplyr::mutate(
          start_date = as.Date("2018-01-01"),
          end_date = as.Date("2022-01-01")
        ) %>%
        dplyr::filter(person_id < 10000000) %>%
        aou_concept_set(
          concepts = 4324693, start_date = "start_date", end_date = "end_date",
          concept_set_name = "mammogram",
          domains = "procedure", output = "count"
        ) %>%
        dplyr::count(mammogram) %>%
        dplyr::tally() %>%
        dplyr::pull(1)
      )
    },
    5
  )
})


# returns warning
# returns dataframe
test_that("aou_concept_set() returns dataframe when start/end date given, even if collect = FALSE", {
  skip_workbench()

  expect_warning(res <- data.frame(
    person_id = c(2150822L, 5252380L),
    start_date = as.Date("2019-01-01"), end_date = as.Date("2023-01-01")
  ) %>%
    aou_concept_set(
      concepts = 4324693, start_date = "start_date", end_date = "end_date",
      concept_set_name = "mammogram",
      domain = "procedure", output = "all"
    ))

  expect_s3_class(res, "data.frame")
})
