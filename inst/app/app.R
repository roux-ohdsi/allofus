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
# url = "https://docs.google.com/spreadsheets/d/1Ey8MScRYZ9QyS4izVYScISLMb62QEhSM-ErbG27dNtw/edit#gid=1832128489"
# # get sheet names
# googlesheets4::sheet_names(url)
# # save all the sheets in a list for later use
# dat <- list(
#   basics = googlesheets4::read_sheet(url, sheet = "Basics"),
#   lifestyle = googlesheets4::read_sheet(url, sheet = "Lifestyle"),
#   overall_health = googlesheets4::read_sheet(url, sheet = "Overall Health"),
#   family_health_history = googlesheets4::read_sheet(url, sheet = "Family Health History"),
#   personal_medical_history = googlesheets4::read_sheet(url, sheet = "Personal Medical History"),
#   personal_and_family_health_history = googlesheets4::read_sheet(url, sheet = "Personal and Family Health History"),
#   social_determinants_of_health = googlesheets4::read_sheet(url, sheet = "Social Determinants of Health"),
#   cope = googlesheets4::read_sheet(url, sheet = "COPE "),
#   minute_survey_on_covid19_vaccines = googlesheets4::read_sheet(url, sheet = "Minute Survey on COVID-19 Vaccines", skip = 3)
# )
# # save so we don't hvae to keep running this
#  saveRDS(dat, file = here::here("inst", "app", "survey_dat2.rds"))
# con <- ohdsilab_connect(username = key_get("db_username"), password = key_get("db_password"))
# cdm_schema = getOption("schema.default.value")
# write_schema = getOption("write_schema.default.value")
#
# ppi_vocab <- tbl(con, inDatabaseSchema(cdm_schema, "concept")) |>
#   filter(vocabulary_id == "PPI") |>
#   collect()
#
# saveRDS(ppi_vocab, file = here::here("inst", "app", "ppi_vocab.rds"))
names(codebook)
# read it back in as needed
codebook = readRDS(here::here("inst", "app", "survey_dat2.rds"))
ppi_vocab = readRDS(here::here("inst", "app", "ppi_vocab.rds")) |>
  mutate(
    # concept_code = str_replace(concept_code,
    #                                 pattern = "ActiveDuty_AvtiveDutyServeStatus",
    #                                 replacement = "ActiveDuty_ActiveDutyServeStatus"),
         concept_code = tolower(concept_code))


codebook[["basics"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
 filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> basics_missing

codebook[["lifestyle"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> lifestyle_missing

codebook[["overall_health"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> overall_missing

codebook[["family_health_history"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> fmh_missing

codebook[["personal_medical_history"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> pmh_missing

codebook[["personal_and_family_health_history"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> pfmh_missing

codebook[["social_determinants_of_health"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> sdh_missing

codebook[["cope"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> cope_missing

codebook[["minute_survey_on_covid19_vaccines"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(is.na(concept_name), `Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) -> minute_missing

bind_rows(basics_missing,
          lifestyle_missing,
          overall_missing,
          fmh_missing,
          pmh_missing,
          sdh_missing,
          cope_missing,
          minute_missing) |> drop_na() -> missing

write.csv(missing, here::here("inst", "app", "missing-concept_code.csv"))


test = codebook[["family_health_history"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code))

missing_j = missing |> rename(concept_code = `Item Concept`)
fuzzyjoin::stringdist_join(
  missing_j, ppi_vocab,
  by = "concept_code",
  ignore_case = TRUE,
  distance_col = "dist",
  max_dist = 10
) |>
  select(concept_code.x, concept_code.y, `Form Name`, dist) -> fj

codebook[["basics"]] |>
  left_join(ppi_vocab, join_by(`Item Concept` == concept_code)) |>
  filter(`Field Type` != "descriptive") |>
  select(`Item Concept`, `Form Name`, `Field Label`) |>
  left_join(ppi_vocab, by = c(`Item Concept` = "concept_code")) -> basics
