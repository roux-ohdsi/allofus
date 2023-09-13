#' List the current files in your workspace
#'
#' @description
#' The files stored in a workspace are not easily visible from a notebook. List
#' all files in the workspace (apart from notebooks) or files matching a certain pattern.
#'
#' @param pattern Regular expression, such as "*.csv" or a single file name e.g., "mydata.csv".
#' Default will find all files apart from notebooks (.ipynb files).
#' @param silent Logical; whether to print the names of files found.
#' @param ... Other arguments passed to `list.files()`
#' @return A vector of file names
#'
#' @export
#' @examples
#' aou_ls_workspace("*.csv")
#'
aou_ls_workspace <- function(pattern = "", silent = FALSE, ...) {
  files <- list.files(pattern = pattern, ...)
  if (!grepl("ipynb", pattern)) {
    files <- files[!grepl("*.ipynb", files)]
  }
  if (length(files) == 0) {
    cat(cli::col_red("No files found with that pattern."))
  } else {
    if (!silent) {
      for (i in 1:length(files)) {
        cat(files[i], "\n")
      }
    }
    invisible(files)
  }
}


#' List the current files in your bucket
#'
#' The files stored in a bucket are not easily visible from a notebook. List
#' all files in the bucket or files matching a certain pattern.
#'
#' @param pattern Regular expression, such as "*.csv" or a single file name e.g., "mydata.csv".
#' Default will find all files apart from notebooks (.ipynb files).
#' @param silent Logical; whether to print the names of files found.
#' @param bucket Bucket to retrieve file from. Defaults to `getOption("aou.default.bucket")`,
#' which is `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#' @param gsutil_args A string containing other arguments passed to `gsutil ls`.
#' See https://cloud.google.com/storage/docs/gsutil/commands/ls for details.
#' @return A vector of file names
#'
#' @export
#' @examples
#' \dontrun{
#' aou_ls_bucket("*.csv")
#' # list all csv files in the data directory, including in subdirectories
#' aou_ls_bucket("data/*.csv", gsutil_args = "-r")
#' }
#'
aou_ls_bucket <- function(pattern = "", silent = FALSE, bucket = Sys.getenv("WORKSPACE_BUCKET"), gsutil_args = "") {
  # Check if file is in the bucket
  files <- system(paste0("gsutil ls ", gsutil_args, " ", bucket, "/", pattern), intern = TRUE)
  # stringr::str_remove(files, paste0(bucket_name, "/data/"))
  files <- gsub(".*/data/", "", files)

  if (length(files) == 0) {
    cat(cli::col_red("No files found with that pattern."))
  } else {
    if (!silent) {
      for (i in 1:length(files)) {
        cat(files[i], "\n")
      }
    }
    invisible(files)
  }
}

#' Move files from a bucket to your workspace
#'
#' @param file The name of a file in your bucket, a vector of multiple files, a directory,
#' or a file pattern (e.g. ".csv").
#' @param dir Optional directory in the workspace to save files to.
#' @param bucket Bucket to retrieve file from. Defaults to `getOption("aou.default.bucket")`,
#' which is `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#'
#' @description This function retrieves a file from your bucket and moves it
#' into your workspace where it can be read into R, e.g., using a function like write.csv().
#' See https://cloud.google.com/storage/docs/gsutil/commands/cp for details on the
#' underlying function.
#'
#' @return Nothing
#' @export
#' @examples
#' \dontrun{
#' aou_bucket_to_workspace("data1.csv")
#' read.csv("data1.csv")
#' # a file to a specific directory in the workspace
#' aou_bucket_to_workspace("data2.csv", dir = "data")
#' read.csv("data/data2.csv")
#' # all of the files in this directory
#' aou_bucket_to_workspace("data/")
#' read.csv("data/data3.csv")
#' }
#'
aou_bucket_to_workspace <- function(file, dir = "", bucket = getOption("aou.default.bucket")) {
  # # Copy the file from current workspace to the bucket
  bucket_files <- allofus::aou_ls_bucket(silent = TRUE)

  missing_files <- list()

  for (i in 1:length(file)) {
    if (!(file[i] %in% bucket_files)) {
      cat(cli::col_red("Oops! ", file[i], " not found in bucket\n"))
      missing_files <- append(missing_files, file[i])
    } else {
      system(paste0("gsutil cp ", bucket, "/", file[i], " ."), intern = TRUE)
      cat(cli::col_green("Retrieved ", file[i], " from bucket\n"))
    }
  }

  if (length(missing_files) > 0) {
    missing <- paste0(unlist(missing_files), collapse = ", ")
    stop(paste0(missing, " not found in bucket\n"))
  }
}

#' Save a file from your workspace to your bucket.
#'
#' @param file The name of a file in your bucket, a vector of multiple files, a directory,
#' or a file pattern (e.g. ".csv"). See Details.
#' @param dir Optional directory in the bucket to save files to.
#' @param bucket Bucket to save files to. Defaults to `getOption("aou.default.bucket")`,
#' which is `Sys.getenv('WORKSPACE_BUCKET')` unless specified otherwise.
#'
#' @description This function moves a file saved in a workspace
#' to a bucket, where it can be retrieved even if the environment is deleted. To use, first save the desired
#' object as a file to the workspace (e.g., write.csv(object, "filename.csv")) and then run this function
#' (e.g., aou_workspace_to_bucket(files = "filename.csv")). See https://cloud.google.com/storage/docs/gsutil/commands/cp for details on the
#' underlying function.
#' @export
#' @examples
#' \dontrun{
#' aou_workspace_to_bucket("data1.csv")
#' # a file to a specific directory in the bucket
#' aou_workspace_to_bucket("data2.csv", dir = "data")
#' # all of the files in this directory
#' aou_workspace_to_bucket("data/")
#' # multiple specific files
#' aou_workspace_to_bucket(c("data1.csv", "data2.csv"))
#' }
#'
aou_workspace_to_bucket <- function(file, dir = "", bucket_name = Sys.getenv("WORKSPACE_BUCKET")) {
  # TODO? check to see if files are there as in the bucket-to-workspace func?
  # Copy the file from current workspace to the bucket
  for (i in 1:length(file)) {
    system(paste0("gsutil cp ./", file[i], " ", bucket_name, "/", dir), intern = TRUE)
    cat(cli::col_green("Saved ", file[i], " to bucket\n"))
  }
}
