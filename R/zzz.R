#' @keywords internal
.onLoad <- function(libname = find.package("allofus"), pkgname = "allofus") {
  op <- options()
  op.aou <- list(
    aou.default.cdr = Sys.getenv("WORKSPACE_CDR"),
    aou.default.bucket = Sys.getenv("WORKSPACE_BUCKET"),
    aou.default.con = NULL
  )
  toset <- !(names(op.aou) %in% names(op))
  if (any(toset)) options(op.aou[toset])

  invisible()
}

greet_startup <- function() {
  msg <- paste0(
    c(
      "{cli::symbol$heart} Thank you for using the {.pkg allofus} R package! {cli::symbol$heart}",
      "{cli::symbol$warning} This package continues to be developed as All of Us grows and changes. Please report any issues to {.url https://github.com/roux-ohdsi/allofus/issues}.",
      "{cli::symbol$info} The {.pkg allofus} R package is not affiliated with or endorsed by the All of Us Research Program. \n\n"
    ),
    collapse = "\n"
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

.onAttach <- function(libname = find.package("allofus"), pkgname = "allofus") {
  greet_startup()
  if (!on_workbench()) {
    rlang::inform(cli::format_inline("{cli::symbol$warning} This package has limited functionality outside of the All of Us Researcher Workbench"),
      class = "packageStartupMessage"
    )
  }
}
