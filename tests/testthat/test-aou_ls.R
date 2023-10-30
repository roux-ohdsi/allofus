# Test aou_ls_workspace function
test_that("aou_ls_workspace function returns correct files", {
  # Create some test files
  file.create("test1.csv")
  file.create("test2.txt")
  file.create("test3.R")
  
  # Test finding all files
  expect_equal(aou_ls_workspace(), c("test1.csv", "test2.txt", "test3.R"))
  
  # Test finding csv files
  expect_equal(aou_ls_workspace("*.csv"), "test1.csv")
  
  # Test finding R files
  expect_equal(aou_ls_workspace("*.R"), "test3.R")
  
  # Test finding non-existent files
  expect_output(aou_ls_workspace("*.pdf"), "No files found with that pattern.")
  
  # Clean up test files
  file.remove("test1.csv", "test2.txt", "test3.R")
})

# Test aou_ls_bucket function
test_that("aou_ls_bucket function returns correct files", {
  # Create some test files in a test bucket
  system("gsutil mb gs://test-bucket")
  system("touch test-bucket/test1.csv")
  system("touch test-bucket/test2.txt")
  system("touch test-bucket/test3.R")
  
  # Test finding all files
  expect_equal(aou_ls_bucket(bucket = "gs://test-bucket"), c("test1.csv", "test2.txt", "test3.R"))
  
  # Test finding csv files
  expect_equal(aou_ls_bucket("*.csv", bucket = "gs://test-bucket"), "test1.csv")
  
  # Test finding R files
  expect_equal(aou_ls_bucket("*.R", bucket = "gs://test-bucket"), "test3.R")
  
  # Test finding non-existent files
  expect_output(aou_ls_bucket("*.pdf", bucket = "gs://test-bucket"), "No files found with that pattern.")
  
  # Clean up test files and bucket
  system("gsutil rm -r gs://test-bucket")
})

# Test aou_bucket_to_workspace function
test_that("aou_bucket_to_workspace function retrieves files from bucket", {
  # Create some test files in a test bucket
  system("gsutil mb gs://test-bucket")
  system("touch test-bucket/test1.csv")
  system("touch test-bucket/test2.txt")
  system("touch test-bucket/test3.R")
  
  # Test retrieving a single file
  aou_bucket_to_workspace("test1.csv", bucket = "gs://test-bucket")
  expect_true(file.exists("test1.csv"))
  
  # Test retrieving multiple files
  aou_bucket_to_workspace(c("test2.txt", "test3.R"), bucket = "gs://test-bucket")
  expect_true(file.exists("test2.txt"))
  expect_true(file.exists("test3.R"))
  
  # Test retrieving non-existent files
  expect_error(aou_bucket_to_workspace("test4.csv", bucket = "gs://test-bucket"))
  
  # Clean up test files and bucket
  system("gsutil rm -r gs://test-bucket")
})

# Test aou_workspace_to_bucket function
test_that("aou_workspace_to_bucket function saves files to bucket", {
  # Create some test files in the workspace
  file.create("test1.csv")
  file.create("test2.txt")
  file.create("test3.R")
  
  # Test saving a single file
  aou_workspace_to_bucket("test1.csv", bucket = "gs://test-bucket")
  expect_true(system("gsutil ls gs://test-bucket/test1.csv", intern = TRUE) != "")
  
  # Test saving multiple files
  aou_workspace_to_bucket(c("test2.txt", "test3.R"), bucket = "gs://test-bucket")
  expect_true(system("gsutil ls gs://test-bucket/test2.txt", intern = TRUE) != "")
  expect_true(system("gsutil ls gs://test-bucket/test3.R", intern = TRUE) != "")
  
  # Test saving non-existent files
  expect_error(aou_workspace_to_bucket("test4.csv", bucket = "gs://test-bucket"))
  
  # Clean up test files and bucket
  system("gsutil rm -r gs://test-bucket")
  file.remove("test1.csv", "test2.txt", "test3.R")
})