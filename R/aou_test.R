#' Function to test all of us package on All of Us Researcher Workbench using documented examples
#' @keywords internal
#' @noRd
aou_test_examples <- function() {
  fun_names <- getNamespaceExports("allofus")
  # don't test -- mi
  fun_names <- fun_names[!fun_names %in% c("aou_atlas_cohort")]
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

#' Function to test all of us package on All of Us Researcher Workbench using testthat tests
#' @keywords internal
#' @noRd
aou_test_tests <- function() {
  gh::gh("https://api.github.com/repos/roux-ohdsi/allofus/contents/tests/testthat", ref = "main") %>%
    purrr::map_chr(purrr::pluck, "download_url") %>%
    purrr::walk(source)
}
