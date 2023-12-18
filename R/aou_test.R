#' Function to test all of us package on AllofUs Researcher Workbench using documented examples
#' @keywords internal
#' @noRd
aou_test_examples <- function() {
  fun_names <- getNamespaceExports("allofus")
  for (fun in fun_names) {
    testthat::test_that(glue::glue("{fun} runs without error"), {
      testthat::expect_no_error({
        invisible(
          capture.output(
            example(fun, package = "allofus", character.only = TRUE, echo = FALSE)
          )
        )
      })
    })
  }
}

#' Function to test all of us package on AllofUs Researcher Workbench using testthat tests
#' @keywords internal
#' @noRd
aou_test_examples <- function() {

  gh::gh("https://api.github.com/repos/roux-ohdsi/allofus/contents/tests/testthat", ref = "cran") %>%
    purrr::walk(source)

}
