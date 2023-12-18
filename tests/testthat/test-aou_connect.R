test_that("aou_connect connects to the database successfully", {
  skip_workbench()

  con <- aou_connect()
  expect_true(inherits(con, "BigQueryConnection"))
  expect_true(isTRUE(con@dataset == strsplit(getOption("aou.default.cdr"), split = "\\.")[[1]][2]))
})

# THIS FAIL!
test_that("aou_connect returns an error message when unable to connect", {
  skip_workbench()

  con <- aou_connect(CDR = "nonexistent.dataset")
  expect_false(inherits(con, "BigQueryConnection"))
})

test_that("aou_sql returns a dataframe", {
  skip_workbench()

  query <- "SELECT * FROM `{CDR}.person` LIMIT 10"
  result <- aou_sql(query)
  expect_is(result, "data.frame")
})

test_that("aou_sql correctly evaluates expressions enclosed with braces", {
  skip_workbench()

  query <- "SELECT * FROM `{CDR}.person` WHERE person_id = {person_id}"
  person_id <- 2150822
  result <- aou_sql(query)
  expect_equal(nrow(result), 1)
})

test_that("aou_sql correctly evaluates references to `CDR` when specified", {
  skip_workbench()

  query <- "SELECT COUNT(*) FROM `{CDR}.person`"
  result <- aou_sql(query, CDR = paste0(Sys.getenv("WORKSPACE_CDR"), "_base"))
  expect_true(result > 0)
})
