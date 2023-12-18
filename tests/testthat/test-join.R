test_that("aou_join throws an error if connection is not set", {
  skip_workbench()

  # Remove the default connection
  opts <- options()

  options(aou.default.con = NULL)
  person_tbl <- tbl(con, "person")
  # Check that an error is thrown when the connection is not set
  expect_error(aou_join(person_tbl, "observation", "left", by = "person_id"), "No connection available")
  options(opts)
})

# test that there's a warning joining person and observation tables
test_that("aou_join throws a warning if joining person and observation tables without specifying all matching columns", {
  skip_workbench()

  person_tbl <- tbl(con, "person")
  expect_warning(aou_join(person_tbl, "observation", "left", by = "person_id"), "columns")
})
