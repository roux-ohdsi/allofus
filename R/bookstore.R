
#' Function to Install and Load R Packages
#'
#' @param package_names a character vector of package names from CRAN or github
#' @param description The bookstore() function will check to see if packages provided are
#' already installed on the local machine. If not installed, it will look for the package on CRAN
#' and install it if found. If it doesn't find the package on CRAN, it'll ask for the owner/repository_name
#' from github to install the package from github. this input should be provided without quotes. For example,
#' respond with roux-ohdsi/ohdsilab NOT "roux-ohdsi/ohdsilab".
#' @export
#' @examples
#' \dontrun{
#' bookstore(c("aouFI", "CohortGenerator", "tidyr"))
#' }
bookstore <- function(package_names, quietly = TRUE){

  remaining_packages <- package_names[!(package_names %in% installed.packages()[,"Package"])]

  cran_packages <- remaining_packages[remaining_packages %in% available.packages()[,"Package"]]
  github_install <- remaining_packages[!(remaining_packages %in% available.packages()[,"Package"])]

  github_packages = list()

  if(length(github_install)){
    for(i in 1:length(github_install)){

      cat("Package", github_install[i], "not found in CRAN. \n")
      cat("Enter the Github user and repository for this package (e.g., roux-ohdsi/ohdsilab) \n")
      github_packages[[i]] <- readline(paste(github_install[i], "owner/repository: "))

    }

    github_packages <- unlist(github_packages)

  }

  if(length(cran_packages>0)){
    cat("CRAN Packages to install:", length(cran_packages), "\n")
  } else {
    cat("All CRAN packages already installed", "\n")
  }

  if(length(github_packages>0)){
    cat("Github Packages to install:", length(github_packages), "\n")
  } else {
    cat("All Github packages already installed", "\n")
  }

  if(length(cran_packages)){
    install.packages(cran_packages);
  }

  if(length(github_packages)){
    remotes::install_github(github_packages);
  }

  for(package_name in package_names){
    if(isTRUE(quietly)){
      suppressPackageStartupMessages(library(package_name, character.only=TRUE, quietly=TRUE))
    } else {
      library(package_name,character.only=TRUE)
    }
    cat("Loaded", package_name, "\n")
  }

}
