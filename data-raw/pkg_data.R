## code to prepare `pkg_data` dataset goes here

library(shiny)
library(googlesheets4)
library(tidyverse)
library(ohdsilab)
library(keyring)
library(DatabaseConnector)
library(fuzzyjoin)
gs4_deauth()

# # url to data dictionary
# url = "https://docs.google.com/spreadsheets/d/1S-aHOjBSVlhuAtOhzra1Z91vIiN5t3NNU7SgZN893Nk/edit?pli=1#gid=1720082551"
 url = "https://docs.google.com/spreadsheets/d/1Ey8MScRYZ9QyS4izVYScISLMb62QEhSM-ErbG27dNtw/edit#gid=1832128489"

 # # get sheet names
 googlesheets4::sheet_names(url)
# # save all the sheets in a list for later use
dat <- list(
  basics = googlesheets4::read_sheet(url, sheet = "Basics"),
  lifestyle = googlesheets4::read_sheet(url, sheet = "Lifestyle"),
  overall_health = googlesheets4::read_sheet(url, sheet = "Overall Health"),
  family_health_history = googlesheets4::read_sheet(url, sheet = "Family Health History"),
  personal_medical_history = googlesheets4::read_sheet(url, sheet = "Personal Medical History"),
  personal_and_family_health_history = googlesheets4::read_sheet(url, sheet = "Personal and Family Health History"),
  social_determinants_of_health = googlesheets4::read_sheet(url, sheet = "Social Determinants of Health"),
  cope = googlesheets4::read_sheet(url, sheet = "COPE "),
  minute_survey_on_covid19_vaccines = googlesheets4::read_sheet(url, sheet = "Minute Survey on COVID-19 Vaccines", skip = 3)
)

# PPI Vocab
con <- ohdsilab_connect(username = key_get("db_username"), password = key_get("db_password"))
cdm_schema = getOption("schema.default.value")
write_schema = getOption("write_schema.default.value")

ppi_vocab <- tbl(con, inDatabaseSchema(cdm_schema, "concept")) |>
  filter(vocabulary_id == "PPI") |>
  collect()

ppi_vocab = ppi_vocab |>
  mutate(concept_code = tolower(concept_code))


join_to_ppi <- function(.x){
  full_join(.x, ppi_vocab, join_by(`Item Concept` == concept_code)) |>
    filter(`Field Type` != "descriptive") |>
    select(concept_code = `Item Concept`, concept_id, concept_name,  concept_class_id,
           form_name = `Form Name`, field_type = `Field Type`, field_label = `Field Label`,
           choices = contains("Choices"), standard_concept, valid_start_date, valid_end_date)
}

map(dat, join_to_ppi) -> joined
aou_codebook = bind_rows(joined)

usethis::use_data(aou_codebook, overwrite = TRUE)
