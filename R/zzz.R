#' @keywords internal
.onLoad <- function(libname = find.package("allofus"), pkgname = "allofus") {
  # if the verily env vars aren't set, try to source them from the ~/.aou-env
  # cache first (fast: no subprocess calls), then fall back to querying the
  # Workbench CLI directly (workbench 2.0, where WORKSPACE_CDR is never
  # injected as an OS env var), caching the result for next time
  if (Sys.getenv("WORKSPACE_CDR") == "") {
    cached <- read_aou_env()

    if (!is.na(cached["WORKSPACE_CDR"]) && cached["WORKSPACE_CDR"] != "") {
      to_set <- as.list(cached)
      to_set <- to_set[Sys.getenv(names(to_set)) == ""]
      if (length(to_set) > 0) do.call(Sys.setenv, to_set)
    } else {
      resolved <- tryCatch(workbench_env_vars(), error = function(e) NULL)
      if (length(resolved) > 0) {
        write_aou_env(resolved)
        resolved <- resolved[Sys.getenv(names(resolved)) == ""]
        if (length(resolved) > 0) do.call(Sys.setenv, resolved)
      }
    }
  }

  op <- options()
  op <- op[sapply(op, function(x) !identical(x, ""))]
  op.aou <- list(
    aou.default.cdr = Sys.getenv("WORKSPACE_CDR"),
    aou.default.bucket = Sys.getenv("WORKSPACE_BUCKET"),
    aou.default.con = NULL
  )
  toset <- !(names(op.aou) %in% names(op))
  if (any(toset)) {
    options(op.aou[toset])
  }
  invisible()
}

#' Read cached workspace environment variables from `~/.aou-env`
#' @return A named character vector (empty if the file doesn't exist).
#' @keywords internal
read_aou_env <- function() {
  aou_env_path <- path.expand("~/.aou-env")
  if (!file.exists(aou_env_path)) {
    return(character(0))
  }

  lines <- readLines(aou_env_path, warn = FALSE)
  lines <- lines[grepl("^\\s*export\\s+[^=]+=", lines)]
  kv <- sub("^\\s*export\\s+", "", lines)
  keys <- sub("=.*$", "", kv)
  vals <- gsub('^"|"$', "", sub("^[^=]+=", "", kv))
  names(vals) <- keys
  vals
}

#' Write workspace environment variables to the `~/.aou-env` cache
#' @description Merges `vars` into whatever is already cached (e.g., a
#'   previously-resolved `WORKSPACE_CDR`, or a bucket created in an earlier
#'   session) rather than overwriting the file, so different functions can
#'   cache different variables over time.
#' @param vars A named list or character vector of environment variables to cache.
#' @return Nothing; called for its side effect.
#' @keywords internal
write_aou_env <- function(vars) {
  vars <- vars[!vapply(vars, function(x) is.null(x) || is.na(x) || x == "", logical(1))]
  if (length(vars) == 0) {
    return(invisible(NULL))
  }

  merged <- utils::modifyList(as.list(read_aou_env()), as.list(vars))
  lines <- paste0("export ", names(merged), "=\"", unlist(merged), "\"")
  writeLines(lines, path.expand("~/.aou-env"))
  invisible(NULL)
}

#' Fetch workspace environment variables from the Workbench CLI
#' @description On workbench 2.0 (Verily-based), WORKSPACE_CDR is never
#'   injected as an OS env var the way it was on classic Terra-based
#'   workbenches, and there's no `~/.aou-env` file created automatically.
#'   `wb auth print-access-token` can't be used to hit the Workspace Manager
#'   API directly here: it returns the workspace's pet service account
#'   identity (a GCE VM instance-identity token), which the API doesn't
#'   accept, rather than the logged-in user's credential. `wb resource list`
#'   sidesteps this entirely by having the CLI handle its own authentication
#'   internally. Returns `NULL` (never errors) if any step fails, so
#'   `.onLoad()` can proceed without env vars set.
#' @return A named list with `WORKSPACE_CDR` and (if available) `GOOGLE_PROJECT`,
#'   or `NULL`.
#' @keywords internal
workbench_env_vars <- function() {
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    return(NULL)
  }

  response <- suppressWarnings(system2("wb", c("resource", "list", "--format=json"), stdout = TRUE, stderr = FALSE))
  resources <- tryCatch(
    jsonlite::fromJSON(paste(response, collapse = "\n"), simplifyVector = FALSE),
    error = function(e) NULL
  )
  if (is.null(resources)) {
    return(NULL)
  }

  # the main CDR is a referenced BigQuery dataset; workspaces can also have a
  # "prep_"-prefixed scratch dataset alongside it, which we don't want
  cdr_resource <- purrr::detect(
    resources,
    ~ .x$resourceType %in% c("BQ_DATASET", "BIGQUERY_DATASET", "BIG_QUERY_DATASET") &&
      .x$stewardshipType == "REFERENCED" &&
      !startsWith(.x$datasetId, "prep_")
  )
  if (is.null(cdr_resource)) {
    return(NULL)
  }

  to_set <- list(WORKSPACE_CDR = paste0(cdr_resource$projectId, ".", cdr_resource$datasetId))

  context_path <- path.expand("~/.workbench/context.json")
  if (file.exists(context_path)) {
    context <- tryCatch(jsonlite::fromJSON(context_path, simplifyVector = FALSE), error = function(e) NULL)
    google_project <- context$workspace$googleProjectId
    if (!is.null(google_project) && google_project != "") {
      to_set$GOOGLE_PROJECT <- google_project
    }
  }

  to_set
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
