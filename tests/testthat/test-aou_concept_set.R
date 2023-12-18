# returns warning
test_that("aou_concept_set() returns warning when concept set name is not provided", {
  expect_warning(
    aou_concept_set(concepts = 4324693, domain = "procedure", output = "count")
  )
})

# returns 13
test_that("aou_concept_set() returns counts correctly", {
  expect_gt(
    {
      tbl(con, "person") %>%
        mutate(
          start_date = as.Date("2018-01-01"),
          end_date = as.Date("2022-01-01")
        ) %>%
        filter(person_id < 10000000) %>%
        aou_concept_set(
          concepts = 4324693, start_date = "start_date", end_date = "end_date",
          concept_set_name = "mammogram",
          domains = "procedure", output = "count"
        ) %>%
        count(mammogram) %>%
        tally() %>%
        pull(1)
    },
    5
  )
})


# returns warning
# returns dataframe
test_that("aou_concept_set() returns dataframe when start/end date given, even if collect = FALSE", {
  expect_warning(res <- data.frame(
    person_id = c(2150822L, 5252380L),
    start_date = as.Date("2019-01-01"), end_date = as.Date("2023-01-01")
  ) %>%
    aou_concept_set(
      concepts = 4324693, start_date = "start_date", end_date = "end_date",
      concept_set_name = "mammogram",
      domain = "procedure", output = "all"
    ))

  expect_is(res, "data.frame")
})
