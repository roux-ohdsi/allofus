library(testthat)
test_that("all examples run without error", {
  fun_names <- getNamespaceExports("allofus")

  for (fun in fun_names) {
    cat("Testing examples from:", fun, "\n")
    expect_no_error({
      example(fun,
        package = "allofus", character.only = TRUE, echo = FALSE,
        run.dontrun = TRUE
      )
    })
  }
})
