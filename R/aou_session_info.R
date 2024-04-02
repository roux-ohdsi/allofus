#' Print session information for the AoU R environment
#'
#' @description Returns a table of information that is necessary to fully
#'   reproduce your analyses. Specifically, it includes R version, the packages
#'   loaded and their versions, and the All of Us CDR release that you are
#'   using.
#'
#' @param CDR The name of the CDR to use. Defaults to
#'   `getOption("aou.default.cdr")`
#'
#' @return A list with three elements: the platform, the AoU release, and the
#'   packages
#' @export
#'
#' @examplesIf on_workbench()
#' allofus::aou_session_info()
aou_session_info <- function(CDR = getOption("aou.default.cdr")) {
  suppressWarnings({
    # a warning about timezone is annoying and always appears on workbench
    si <- sessioninfo::session_info()
  })
  si1 <- si$platform
  si2 <- allofus::aou_sql(query = "SELECT * FROM {CDR}._cdr_metadata")
  si3 <- si$packages

  out <- list(si1, si2, si3)
  names(out) <- c("Platform", "AoU_Release", "Packages")
  return(out)
}
