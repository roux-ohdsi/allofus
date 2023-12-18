library(testthat)
library(dplyr)

if (on_workbench()) {
  con <- aou_connect()
}

skip_workbench <- function() {
  skip_if_not(
    on_workbench(),
    "This test is only for the workbench"
  )
}
