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
  healthcare_access = googlesheets4::read_sheet(url, sheet = "Healthcare Access and Utilization"),
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

form_names <- data.frame(form_name = c("covid19_participant_experience_cope_survey",
                                       "december_covid19_participant_experience_cope_survey", "family_health_history",
                                       "february_covid19_participant_experience_cope_survey", "july_covid19_participant_experience_cope_survey",
                                       "june_covid19_participant_experience_cope_survey", "lifestyle",
                                       "november_covid19_participant_experience_cope_surve", "overall_health",
                                       "personal_and_family_health_history", "personal_medical_history",
                                       "social_determinants_of_health_english", "summer_minute_survey_on_covid19_vaccines",
                                       "the_basics", "winter_minute_survey_on_covid19_vaccines",
                                       "healthcare_access_and_utilization"),
                         new_name = c("COPE",
                                      "COPE (Dec)", "Family Health History", "COPE (Feb)", "COPE (July)",
                                      "COPE (June)", "Lifestyle", "COPE (Nov)", "Overall Health", "Personal and Family Health History",
                                      "Personal Medical History", "Social Determinants of Health",
                                      "Summer Minute Survey on COVID-19 Vaccines", "The Basics",
                                      "Winter Minute Survey on COVID-19 Vaccines", "Healthcare Access and Utilization"),
                         link = c("https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2019/02/Family_Medical_History.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2019/02/Lifestyle.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/COPE_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2019/02/Overall_Health.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2023/PaFHH_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2019/02/Personal_Medical_History.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/surveys/SDOH_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2023/New_Year_Minute_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/faq/Basics_Survey_ENG_23.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2023/New_Year_Minute_Survey_English.pdf",
                                  "https://www.researchallofus.org/wp-content/themes/research-hub-wordpress-theme/media/2019/02/Health_Care_Access.pdf"
                                  ))

aou_codebook = bind_rows(joined) |>
  filter(concept_class_id == "Question") |>
  left_join(form_names) |>
  select(-form_name) |>
  rename(form_name = new_name)

usethis::use_data(aou_codebook, overwrite = TRUE)
