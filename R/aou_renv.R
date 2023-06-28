#' Save renv files to bucket
#'
#' @export
aou_save_renv <- function(){
  files = c("renv.lock", ".Rprofile", "renv/activate.R")
  allofus::aou_workspace_to_bucket(files)
}

#' Save renv files to bucket
#'
#' @export
aou_load_renv <- function(){
  files = c("renv.lock", ".Rprofile", "renv/activate.R")
  allofus::aou_bucket_to_workspace(files)
}
