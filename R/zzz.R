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

.onAttach <- function(libname = find.package("allofus"), pkgname = "allofus"){
    cli::cli_inform(c("i" = "Thank you for using the {.pkg allofus} R package!",
                    "This package is currently in beta. Please report any issues to {.url https://github.com/roux-ohdsi/allofus/issues}.",
                    "The {.pkg allofus} R package is not affiliated with or endorsed by the AllofUs Research Program. \n\n"))
}

