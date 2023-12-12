#' Print session information for the AoU R environment
#'
#' @description
#' For the purposes of reproducibility, it is important to know the version of R,
#' the packages, and the AoU release that you are using. This function prints
#' this information to the console.
#'
#' @param CDR The name of the CDR to use. Defaults to `getOption("aou.default.cdr")`
#'
#' @return A list with three elements: the platform, the AoU release, and the packages
#' @export
#'
#' @examplesIf on_workbench()
#' allofus::aou_session_info()

aou_session_info <- function(CDR = getOption("aou.default.cdr")){
  si = sessioninfo::session_info()
  si1 = si$platform
  si2 = allofus::aou_sql(query = "SELECT * FROM {CDR}._cdr_metadata")
  si3 = si$packages

  out = list(si1, si2, si3)
  names(out) = c("Platform", "AoU_Release", "Packages")
  return(out)
}
