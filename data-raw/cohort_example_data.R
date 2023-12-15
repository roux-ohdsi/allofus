# library(ROhdsiWebApi)
#
# # generate a simple stroke cohort
# # see https://atlas-demo.ohdsi.org/#/cohortdefinition/1788061
# cd <- getCohortDefinition(1788061, "https://atlas-demo.ohdsi.org/WebAPI")
# cd_sql <- getCohortSql(cd, "https://atlas-demo.ohdsi.org/WebAPI")
#
# aou_cohort_example <- list(cd = cd, cd_sql = cd_sql)
#
# usethis::use_data(aou_cohort_example, overwrite = TRUE)
