# Test aou_ls_workspace function
test_that("aou_ls_workspace function returns correct files", {
  # Create some test files
  tmp <- tempdir()
  dir.create(tmp)
  file.create(file.path(tmp, "test1.csv"))
  file.create(file.path(tmp, "test2.txt"))
  file.create(file.path(tmp, "test3.R"))

  # Test finding all files
  expect_equal(aou_ls_workspace(path = tmp), c("test1.csv", "test2.txt", "test3.R"))

  # Test finding csv files
  expect_equal(aou_ls_workspace(pattern = "*.csv", path = tmp), "test1.csv")

  # Test finding R files
  expect_equal(aou_ls_workspace(pattern = "*.R", path = tmp), "test3.R")

  # Test finding non-existent files
  expect_message(aou_ls_workspace(pattern = "*.pdf", path = tmp), "No files found with that pattern.")

  # Clean up test files
  unlink(tmp, recursive = TRUE)
})

# NEED OTHER TESTS FOR BUCKET
