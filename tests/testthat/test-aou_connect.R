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
  result <- aou_sql(query, collect = TRUE, CDR = Sys.getenv("WORKSPACE_CDR"))
  expect_true(result > 0)
})

test_that("aou_cdr_dataset falls back to the aou.default.cdr option", {
  withr::local_options(aou.default.cdr = "other-project.other_dataset")

  ds <- aou_cdr_dataset(NULL)
  expect_equal(ds$project, "other-project")
  expect_equal(ds$dataset, "other_dataset")
})

test_that("aou_cdr_dataset errors informatively with no connection and no option", {
  withr::local_options(aou.default.cdr = "")
  expect_error(aou_cdr_dataset(NULL), "Unable to determine which CDR")
})

test_that("aou_cdr_dataset prefers the connection over the option", {
  skip_workbench()

  con <- aou_connect()
  withr::local_options(aou.default.cdr = "other-project.other_dataset")

  ds <- aou_cdr_dataset(con)
  expect_equal(ds$project, con@project)
  expect_equal(ds$dataset, con@dataset)
})

test_that("temp tables are created in the CDR's location so they stay joinable", {
  # regression test for Workbench 2.0: the CDR lives in a single region
  # (us-central1), so a query with no CDR table reference would otherwise run
  # in the US multi-region and its results table could never be referenced
  # alongside a CDR table.
  skip_workbench()

  con <- aou_connect()
  cdr_location <- bigrquery::bq_dataset_meta(
    aou_cdr_dataset(con),
    fields = "location"
  )$location

  # a query built purely from literals: nothing for BigQuery to infer from
  tbl_obj <- aou_bq_query("SELECT 201826 AS concept_id", con = con)
  tmp_location <- bigrquery::bq_dataset_meta(
    bigrquery::bq_dataset(tbl_obj$project, tbl_obj$dataset),
    fields = "location"
  )$location

  expect_equal(tmp_location, cdr_location)
})

test_that("a temp table built from local data can be joined to a CDR table", {
  skip_workbench()

  con <- aou_connect()
  ids <- dplyr::tbl(con, "person") |>
    dplyr::select("person_id") |>
    head(5) |>
    dplyr::collect()

  tt <- aou_create_temp_table(data.frame(
    person_id = as.numeric(ids$person_id),
    label = letters[seq_len(nrow(ids))]
  ))

  res <- dplyr::tbl(con, "person") |>
    dplyr::inner_join(tt, by = "person_id") |>
    dplyr::count() |>
    dplyr::collect()

  expect_equal(as.numeric(res$n), nrow(ids))
})
