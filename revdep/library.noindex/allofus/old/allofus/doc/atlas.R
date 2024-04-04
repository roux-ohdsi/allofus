## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  eval = FALSE,
  comment = "#>"
)

## ----install------------------------------------------------------------------
#  install.packages("pak")
#  pak::pak("ohdsi/ROhdsiWebApi")

## ----load---------------------------------------------------------------------
#  library(allofus)
#  library(ROhdsiWebApi)

## ----ohdsiweb-----------------------------------------------------------------
#  cd <- ROhdsiWebApi::getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
#  cd_sql <- ROhdsiWebApi::getCohortSql(cd, "https://atlas-demo.ohdsi.org/WebAPI")

## ----cohort-------------------------------------------------------------------
#  cohort <- aou_atlas_cohort(
#    cohort_definition = cd,
#    cohort_sql = cd_sql
#  )

## -----------------------------------------------------------------------------
#  head(cohort)

## -----------------------------------------------------------------------------
#  dplyr::tbl(con, "person") %>%
#    dplyr::filter(person_id %in% !!cohort$person_id)

