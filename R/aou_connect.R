# AoU helpers

#' Connect to big query database in All of Us
#'
#' @param bucket_name variable name for your bucket. Recommend leaving the default.
#'
#' @description To use, simply run the function without any arguments. It will
#' print a message if you connect successfully. It will also assign your bucket
#' to an object in your R environment.
#' @export
aou_connect <- function(bucket_name = "bucket"){
  dataset <- strsplit(Sys.getenv('WORKSPACE_CDR'), split = '\\.')[[1]]
  release <- dataset[2]
  prefix <- dataset[1]

  connection <- DBI::dbConnect(
    bigrquery::bigquery(),
    billing = Sys.getenv('GOOGLE_PROJECT'),
    project = prefix,
    dataset = release
  )


  assign(bucket_name, Sys.getenv('WORKSPACE_BUCKET'), envir = .GlobalEnv)
  options(con.default.value = connection)

  if(isTRUE(connection@dataset == release)){
    cat(cli::col_green("Connected successfully!"),
        cli::col_blue("Use ", bucket_name,
                      "` to retrieve the name of your bucket"), sep = "\n")
  } else {
    cat(cli::col_red("Error: Unable to connect"))
  }

  return(connection)
}
