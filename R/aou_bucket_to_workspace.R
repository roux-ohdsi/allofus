#' Move files from a bucket to your workspace
#'
#' @param files The name of a file in your bucket or a vector of multiple files.
#' @param bucket_name Name of your bucket. Recommend leaving the default
#'
#' @description This step retrieves a file you have saved permanently in your bucket
#' into your workspace where you can read it into R using a function like write.csv().
#' @export
aou_bucket_to_workspace <- function(files, bucket_name = Sys.getenv('WORKSPACE_BUCKET')){
  # # Copy the file from current workspace to the bucket
  bucket_files = allofus::aou_ls_bucket(silent = TRUE)

  missing_files = list()

  for (i in 1:length(files)) {
    if(!(files[i] %in% bucket_files)){
      cat(cli::col_red("Oops! ", files[i], " not found in bucket\n"))
      missing_files = append(missing_files, files[i])
    } else {
      system(paste0("gsutil cp ", bucket_name, "/data/", files[i], " ."), intern = TRUE)
      cat(cli::col_green("Retrieved ", files[i], " from bucket\n"))
    }
  }

  if(length(missing_files)>0){
    missing = paste0(unlist(missing_files), collapse = ", ")
    stop(paste0(missing, " not found in bucket\n"))
  }

}

#' Save a file from your workspace to your bucket.
#'
#' @param files name of file to save
#' @param bucket_name name of your bucket. Recommend leaving the default
#'
#' @description This step permanently saves a file you have saved in your workspace
#' to your bucket where you can always retrieve it. To use, first you need to save the desired
#' r object as a file (e.g., write.csv(object, filename.csv)) and then run this function
#' (e.g., aou_workspace_to_bucket(files = "filename.csv")).
#' @export
aou_workspace_to_bucket <- function(files, bucket_name = Sys.getenv('WORKSPACE_BUCKET')){
  # Copy the file from current workspace to the bucket
  for(i in 1:length(files)){
    system(paste0("gsutil cp ./", files[i], " ", bucket_name, "/data/"), intern = TRUE)
    cat(cli::col_green("Saved ", files[i], " to bucket\n"))
  }

}
