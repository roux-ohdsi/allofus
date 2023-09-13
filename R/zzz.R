#' @keywords internal
.onLoad <- function(libname = find.package("allofus"), pkgname = "allofus") {

  op <- options()
  op.aou <- list(
    aou.default.cdr = Sys.getenv('WORKSPACE_CDR'),
    aou.default.bucket = Sys.getenv('WORKSPACE_BUCKET'),
    aou.default.con = NULL
  )
  toset <- !(names(op.aou) %in% names(op))
  if (any(toset)) options(op.aou[toset])

  invisible()
}
