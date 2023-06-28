#' List the current files in your workspace.
#'
#' @param silent logical; whether to print the files found
#' @param pattern pattern like *.csv or a single file name e.g., mydata.csv
#'
#' @export
aou_ls_workspace <- function(pattern = "*.csv", silent = FALSE){
  files <- list.files(pattern = pattern)
  files <- files[!grepl("*.ipynb", files)]
  if(length(files)==0){
    cat(cli::col_red("No files found with that pattern."))
  } else {
    if(!silent){
      for(i in 1:length(files)){
        cat(files[i], "\n")
      }
    }
    invisible(files)
  }
}


#' List the current files in your bucket.
#'
#' @description Quick function to list files matching a pattern in your bucket
#'
#' @param pattern pattern like *.csv or a single file name e.g., mydata.csv
#' @param bucket_name name of your bucket. Recommend leaving the default
#' @param silent logical; whether to print the files found
#' @export
aou_ls_bucket <- function(pattern = "*.csv", bucket_name = Sys.getenv('WORKSPACE_BUCKET'), silent = FALSE){
  # Check if file is in the bucket
  files <- system(paste0("gsutil ls ", bucket_name, "/data/", pattern), intern = TRUE)
  #stringr::str_remove(files, paste0(bucket_name, "/data/"))
  files <- gsub(".*/data/", "", files)

  if(length(files)==0){
    cat(cli::col_red("No files found with that pattern."))
  } else {
    if(!silent){
      for(i in 1:length(files)){
        cat(files[i], "\n")
      }
    }
    invisible(files)
  }
}

