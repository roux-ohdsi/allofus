#' List the current files in your workspace
#'
#' @description Lists all data files in the workspace or files matching a
#'   certain pattern.
#'
#' @param pattern Regular expression, such as "*.csv" or a single file name
#'   e.g., "mydata.csv". Default will find all files apart from notebooks
#'   (.ipynb, .Rmd, .qmd files).
#' @param silent Whether to omit the names of files found. Defaults to `FALSE`.
#' @param ... Other arguments passed to `list.files()`
#' @return A vector of file names
#'
#' @export
#' @examples
#' my_workspace_files <- aou_ls_workspace(silent = TRUE)
#' aou_ls_workspace("*.csv")
#' aou_ls_workspace(path = "data")
#'
aou_ls_workspace <- function(pattern = "", silent = FALSE, ...) {
  files <- list.files(pattern = pattern, ...)
  if (!grepl("ipynb", pattern)) {
    files <- files[!grepl("*.ipynb", files)]
  }
  if (!grepl("Rmd", pattern)) {
    files <- files[!grepl("*.Rmd", files)]
  }
  if (!grepl("qmd", pattern)) {
    files <- files[!grepl("*.qmd", files)]
  }
  if (length(files) == 0) {
    cli::cli_inform(c("!" = "No files found with that pattern."))
  } else {
    if (!silent) {
      for (i in seq_along(files)) {
        cat(files[i], "\n")
      }
    }
    invisible(files)
  }
}


#' List the current files in your bucket
#' @description Lists all files in the bucket or files matching a certain
#'   pattern.
#'
#' @param pattern Regular expression, such as "*.csv" or a single file name
#'   e.g., "mydata.csv". Default will find all files apart from notebooks
#'   (.ipynb files).
#' @param silent Whether to omit the names of files found. Defaults to `FALSE`.
#' @param recursive Whether to search subdirectories. Defaults to `TRUE`.
#' @param bucket Bucket to retrieve file from. Defaults to
#'   `getOption("aou.default.bucket")`, which is
#'   `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#' @param gsutil_args A string containing other arguments passed to `gsutil ls`.
#'   See <https://cloud.google.com/storage/docs/gsutil/commands/ls> for details.
#' @return A vector of file names
#'
#' @export
#' @examplesIf on_workbench()
#' # list all files, including in subdirectories
#' aou_ls_bucket()
#' # list all csv files
#' aou_ls_bucket("*.csv")
#'
aou_ls_bucket <- function(pattern = "", silent = FALSE, recursive = TRUE, bucket = getOption("aou.default.bucket"), gsutil_args = "") {
  if (recursive) {
    gsutil_args <- paste("-r", gsutil_args)
  }

  # Check if file is in the bucket
  files <- suppressWarnings(system(paste0("gsutil ls ", gsutil_args, " ", bucket, "/", pattern), intern = TRUE))

  if (length(files) == 0) {
    cli::cli_inform(c("!" = "No files found with that pattern."))
  } else {
    files <- sub(paste0(bucket, "\\/"), "", files)
    if (!silent) {
      cat(files, sep = "\n")
    }
    invisible(files)
  }
}

#' Move files from a bucket to your workspace
#'
#' @description Retrieves a file from the workspace bucket and moves it into the
#'   current persistent disk where it can be read into R, e.g., using a function
#'   like read.csv().
#'
#' @param file The name of a file in your bucket, a vector of multiple files, a
#'   directory, or a file pattern (e.g. ".csv").
#' @param directory Whether `file` refers to an entire directory you want to
#'   move.
#' @param bucket Bucket to retrieve file from. Defaults to
#'   `getOption("aou.default.bucket")`, which is
#'   `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#'
#' @details This function retrieves a file from your bucket and moves it into
#'   your workspace where it can be read into R, e.g., using a function like
#'   `write.csv()`. See
#'   <https://cloud.google.com/storage/docs/gsutil/commands/cp> for details on
#'   the underlying function.
#'
#' @return Nothing
#' @export
#' @examplesIf on_workbench()
#' # save a file to the bucket
#' tmp <- tempdir()
#' write.csv(data.frame(x = 1), file.path(tmp, "testdata.csv"))
#' aou_workspace_to_bucket(file.path(tmp, "testdata.csv"))
#' # read the file back into the workspace
#' aou_bucket_to_workspace("testdata.csv")
#' # read in to your local environment
#' read.csv("testdata.csv")
#' file.remove("testdata.csv")
#'
#'
aou_bucket_to_workspace <- function(file, directory = FALSE, bucket = getOption("aou.default.bucket")) {
  # # Copy the file from current workspace to the bucket
  bucket_files <- allofus::aou_ls_bucket(silent = TRUE)

  missing_files <- list()

  if (directory) {
    file <- paste0(file, "/:")
    gs_args <- "gsutil cp -r "
  } else {
    gs_args <- "gsutil cp "
  }

  for (i in seq_along(file)) {
    if (!(file[i] %in% bucket_files)) {
      missing_files <- append(missing_files, file[i])
    } else {
      system(paste0(gs_args, bucket, "/", file[i], " ."), intern = TRUE)
      cli::cli_inform(c("v" = "Retrieved ", file[i], " from bucket."))
    }
  }

  if (length(missing_files) > 0) {
    missing <- paste0(unlist(missing_files), collapse = ", ")
    cli::cli_inform(c("!" = paste0(missing, " not found in bucket.")))
  }
}

#' Save a file from your workspace to your bucket
#'
#' @description Moves a file saved in on the persistent disk to the workspace
#'   bucket, where it can be stored even if a compute environment is deleted.
#'
#'
#' @param file The name of a file in your bucket, a vector of multiple files, a
#'   directory, or a file pattern (e.g. ".csv"). See Details.
#' @param directory Whether `file` refers to an entire directory you want to
#'   move.
#' @param bucket Bucket to save files to. Defaults to
#'   `getOption("aou.default.bucket")`, which is
#'   `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#'
#' @details This function moves a file saved in a workspace to a bucket, where
#'   it can be retrieved even if the environment is deleted. To use, first save
#'   the desired object as a file to the workspace (e.g., `write.csv(object,
#'   "filename.csv")`) and then run this function (e.g.,
#'   `aou_workspace_to_bucket(files = "filename.csv")`). See
#'   <https://cloud.google.com/storage/docs/gsutil/commands/cp> for details on
#'   the underlying function.
#' @return Nothing
#' @export
#' @examplesIf on_workbench()
#' # create test files in a temporary directory
#' tmp <- tempdir()
#' write.csv(data.frame(x = 1), file.path(tmp, "testdata1.csv"))
#' write.csv(data.frame(y = 2), file.path(tmp, "testdata2.csv"))
#' # save a file to the bucket
#' aou_workspace_to_bucket(file.path(tmp, "testdata1.csv"))
#' # save multiple files at once
#' aou_workspace_to_bucket(c(file.path(tmp, "testdata1.csv"), file.path(tmp, "testdata2.csv")))
#' # save an entire directory
#' aou_workspace_to_bucket(tmp, directory = TRUE)
aou_workspace_to_bucket <- function(file, directory = FALSE,
                                    bucket = getOption("aou.default.bucket")) {
  if (any(stringr::str_detect(file, " "))) {
    cli::cli_abort("File names cannot contain spaces. Consider using underscores or hyphens instead.")
  }

  tmp <- tempdir()
  tmp_log <- file.path(tmp, "cp.log")
  gsutil_args <- paste("-L", tmp_log)

  if (directory) {
    gsutil_args <- paste("-r", gsutil_args)
  }

  # Copy the file from current workspace to the bucket
  for (i in seq_along(file)) {
    system(
      paste("gsutil cp", gsutil_args, file[i], bucket),
      intern = TRUE
    )
  }
  # Check which files were copied
  if (length(read.csv(tmp_log)$Destination) == 0) {
    cli::cli_inform(c("!" = "Oops! No files were copied"))
  } else {
    cli::cli_inform(c(
      "v" =
        "Saved to bucket:",
      paste(gsub(paste0(bucket, "/"), "", read.csv(tmp_log)$Destination), collapse = "\n")
    ))
  }
  invisible(file.remove(tmp_log))
}


#' Read the contents of a file in your bucket into memory.
#'
#' @description Streams the contents of a file in your bucket into memory.
#'
#' @param filename The name of the file to read in. This can either be a full bucket path,
#'   or the path to the file relative to the bucket associated with the workspace.
#' @param bucket The bucket to read the file from, if not specified in the filename.
#' @param project The Google Cloud project to use.
#' @param read_fn The function to use to read the file. Defaults to `readr::read_table`.
#' @param gsutil_args Additional arguments to pass to `gsutil cat`.
#' @param ... Other arguments passed to `read_fn`.
#' @return The contents of the file as returned by read_fn.
#'
#' @export
#' @examples on_workbench()
#' files = aou_ls_bucket("*.tsv")
#' file = files[1]
#' aou_read_from_bucket(file, read_fn=readr::read_tsv)
#'
aou_read_from_bucket <- function(filename, silent = FALSE,
                                 bucket = Sys.getenv("WORKSPACE_BUCKET"),  # change to aou.default.bucket
                                 project = Sys.getenv("GOOGLE_PROJECT"),
                                 read_fn = readr::read_table,
                                 gsutil_args = "", ...) {


    # Check whether file is a full path including a gs:// prefix.
    if (stringr::str_starts(filename, "gs://")) {
      bucket_file = filename
    } else {
      bucket_file = file.path(bucket, filename)
    }

    print(bucket_file)

    # Check if the file exists.
    files <- suppressWarnings(system(paste0("gsutil ls ", gsutil_args, " ", bucket_file), intern = TRUE))
    print(files)
    if (length(files) == 0) {
      stop("File not found in bucket.")
    }

    # If it exists, stream the contents into R.
    cmd = paste0("gsutil -u ", project, " ", gsutil_args, " cat ", bucket_file)
    print(cmd)
    system(cmd, intern=T) %>%
      I() %>%  # I forget why I had to include this.
      read_fn(...)

}
