#' Print session information for the AoU R environment
#'
#' @param CDR The name of the CDR to use. Defaults to `getOption("aou.default.cdr")`
#'
#' @return A list with three elements: the platform, the AoU release, and the packages
#' @export
#'
#' @examples
#' \dontrun{
#'   con <- allofus::aou_connect()
#'   allofus::aou_session_info()
#' }
aou_session_info <- function(CDR = getOption("aou.default.cdr")){
  si = sessioninfo::session_info()
  si1 = si$platform
  si2 = allofus::aou_sql(query = "SELECT * FROM {CDR}._cdr_metadata")
  si3 = si$packages

  out = list(si1, si2, si3)
  names(out) = c("Platform", "AoU_Release", "Packages")
  return(out)
}
