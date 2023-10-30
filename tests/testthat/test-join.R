context("aou_join")

test_that("aou_join joins tables correctly", {
  # Connect to the database
  con <- allofus::aou_connect()

  # Create a test table
  test_table <- tbl(con, sql("SELECT 1 AS id, 'test' AS name"))

  # Create a test query
  test_query <- tbl(con, sql("SELECT * FROM test_table"))

  # Join the test query with the test table
  joined_query <- aou_join(test_query, "test_table", "left", by = "id")

  # Check that the joined query has the correct number of rows and columns
  expect_equal(nrow(joined_query), 1)
  expect_equal(ncol(joined_query), 2)

  # Check that the joined query has the correct values
  expect_equal(joined_query$id, 1)
  expect_equal(joined_query$name, "test")
})

test_that("aou_join throws an error if connection is not set", {
  # Create a test query
  test_query <- tbl(con, sql("SELECT * FROM test_table"))

  # Remove the default connection
  options(aou.default.con = NULL)

  # Check that an error is thrown when the connection is not set
  expect_error(aou_join(test_query, "test_table", "left", by = "id"), "Have you set up the connection?")
})