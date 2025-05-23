test_that("aou_connect connects to the database successfully", {
  skip_workbench()

  con <- aou_connect()
  expect_true(inherits(con, "BigQueryConnection"))
  expect_true(isTRUE(con@dataset == strsplit(getOption("aou.default.cdr"), split = "\\.")[[1]][2]))
})

test_that("aou_connect returns an error message when unable to connect", {
  skip_workbench()

  expect_error(aou_connect(CDR = "nonexistent.dataset"))
})

test_that("aou_sql returns a dataframe when collect = TRUE", {
  skip_workbench()

  query <- "SELECT * FROM `{CDR}.person` LIMIT 10"
  result <- aou_sql(query, collect = TRUE)
  expect_s3_class(result, "data.frame")
})


test_that("aou_sql correctly evaluates references to `CDR` when specified", {
  skip_workbench()

  query <- "SELECT COUNT(*) FROM `{CDR}.person`"
  result <- aou_sql(query, collect = TRUE, CDR = paste0(Sys.getenv("WORKSPACE_CDR"), "_base"))
  expect_true(result > 0)
})
