# Test aou_ls_workspace function
test_that("aou_ls_workspace function returns correct files", {
  # Create some test files
  unlink("testdata", recursive = TRUE)
  dir.create("testdata")
  file.create("testdata/test1.csv")
  file.create("testdata/test2.txt")
  file.create("testdata/test3.R")

  # Test finding all files
  expect_equal(aou_ls_workspace(path = "testdata"), c("test1.csv", "test2.txt", "test3.R"))

  # Test finding csv files
  expect_equal(aou_ls_workspace(pattern = "*.csv", path = "testdata"), "test1.csv")

  # Test finding R files
  expect_equal(aou_ls_workspace(pattern = "*.R", path = "testdata"), "test3.R")

  # Test finding non-existent files
  expect_message(aou_ls_workspace(pattern = "*.pdf", path = "testdata"), "No files found with that pattern.")

  # Clean up test files
  unlink("testdata", recursive = TRUE)
})

# NEED OTHER TESTS FOR BUCKET
