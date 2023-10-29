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

aou_codebook_w_health_history = bind_rows(joined) |>
  filter(concept_class_id == "Question") |>
  left_join(form_names) |>
  select(-form_name) |>
  rename(form_name = new_name) |>
  mutate(concept_code = ifelse(concept_code == "circulatorycondition_otherheartorbloodcondition_ye",
                               "circulatorycondition_otherheartorbloodcondition_yes", concept_code))

aou_codebook <- aou_codebook_w_health_history %>%
  filter(!(str_detect(form_name, "History") &
             (str_detect(concept_code, "currently$") |
                str_detect(concept_code, "\\_rx") |
                str_detect(concept_code, "prescribedmeds") |
                str_detect(concept_code, "\\_yes$") |
                str_detect(concept_code, "howoldwereyou") |
                str_detect(concept_code, "freetextb"))))

usethis::use_data(aou_codebook, overwrite = TRUE)

specific <- tbl(con, inDatabaseSchema(cdm_schema, "concept")) %>%
  filter(vocabulary_id == "PPI") %>%
  filter(concept_name %like% "Including yourself, who in your family has%") %>%
  filter(concept_class_id == "Answer") %>%
  collect() %>%
  separate(concept_name, into = c("question", "answer"), sep = " - ") %>%
  filter(answer %in% c("Self", "Father", "Mother", "Sibling", "Son", "Daughter", "Grandparent")) %>%
  mutate(condition = str_remove(question, "Including yourself, who in your family has had "),
         condition = str_remove(condition, "\\?"),
         condition = str_to_lower(condition),
         condition = str_remove(condition, "^an*\\s"),
         answer = str_to_lower(answer),
         concept_code = str_to_lower(concept_code)) %>%
  select(question, answer, concept_id, condition, concept_code)

overall <- tbl(con, inDatabaseSchema(cdm_schema, "concept")) %>%
  filter(vocabulary_id == "PPI") %>%
  filter(concept_name %like% "Have you or anyone in your family ever been diagnosed%") %>%
  filter(concept_class_id == "Answer") %>%
  left_join(tbl(con, inDatabaseSchema(cdm_schema, "concept_relationship")), by = c("concept_id" = "concept_id_1"),
            suffix = c("_x", "_y")) %>%
  filter(relationship_id %in% c("Answer of (PPI)", "Has PPI parent code")) %>%
  distinct(concept_name, concept_id_2, concept_code) %>%
  collect() %>%
  separate(concept_name, into = c("question", "answer"), sep = " - ") %>%
  mutate(category = str_remove(question, "Have you or anyone in your family ever been diagnosed with the following "),
         category = str_remove(category, "\\? Think only of the people you are related to by blood\\."),
         category = ifelse(category == "conditions", "other conditions", category),
         answer = str_to_lower(answer),
         answer = str_squish(answer),
         answer = case_when(
           answer == "fractured/broken any bones in the last 5 years" ~ "fractured/broken bones in the last five years",
           answer == "other bone" ~ "other bone, joint, or muscle condition(s)",
           answer == "spine" ~ "spine, muscle, or bone disorders (non-cancer)",
           answer == "skin condition (e.g., eczema, psoriasis)" ~ "skin condition(s) (e.g., eczema, psoriasis)",
           answer == "atrial fibrillation (or a-fib) or atrial flutter (or a-flutter)." ~ "atrial fibrillation (or a-fib) or atrial flutter (or a-flutter)",
           answer == "chronic lung disease (copd, emphysema or bronchitis)" ~ "chronic lung disease (copd, emphysema, or bronchitis)",
           answer == "other condition" ~ "other condition(s)",
           answer == "other brain or nervous system condition" ~ "other brain or nervous system condition(s)",
           answer == "other kidney condition" ~ "other kidney condition(s)",
           answer == "nearsighted" ~ "nearsightedness",
           answer == "other lung condition" ~ "other lung condition(s)",
           answer == "other hearing or eye condition" ~ "other hearing or eye condition(s)",
           answer == "farsighted" ~ "farsightedness",
           answer == "other cancer" ~ "other cancer(s)",
           answer == "other mental health or substance use condition" ~ "other mental or substance use condition",
           answer == "lou gehrig's disease (amyotrophic lateral sclerosis or als)" ~ "lou gehrig's disease (amyotrophic lateral sclerosis)",
           answer == "coronary artery/coronary heart disease (includes angina)" ~ "coronary artery/coronary heart disease",
           answer == "blindness" ~ "blindness, all causes",
           answer == "head and neck (this includes cancers of the mouth, sinuses, nose, or throat.)" ~ "head and neck cancer (this includes cancers of the mouth, sinuses, nose, or throat. this does not include brain cancer.)",
           TRUE ~ answer
         )) %>%
  rename(concept_id = concept_id_2) %>%
  filter(!(concept_id == 43528634 & !str_detect(concept_code, "Digestive"))) %>%
  filter(!(concept_id == 43528896)) %>%
  select(-concept_code, -question)

conditions_table <- specific %>%
  left_join(overall, by = c("condition" = "answer"), suffix = c("_specific", "_overall")) %>%
  rename(relative = answer)

# conditions_table is the table for sharing
# people can input concept_id_specific or concept_code

concepts <- tbl(con, inDatabaseSchema(cdm_schema, "concept")) %>%
  select(concept_id, concept_name, concept_class_id)

question_relationships <- tbl(con, inDatabaseSchema(cdm_schema, "concept_relationship")) %>%
  filter(concept_id_1 %in% !!conditions_table$concept_id_specific,
         relationship_id %in% c("Answer of (PPI)", "Has PPI parent code")) %>%
  distinct(concept_id_1, concept_id_2) %>%
  rename(concept_id_answer = concept_id_1, concept_id_question = concept_id_2)

all_overall_concept_id <- question_relationships %>%
  inner_join(tbl(con, inDatabaseSchema(cdm_schema, "concept_relationship")),
             by = join_by(concept_id_question == concept_id_1), suffix = c("_x", "_y")) %>%
  filter(relationship_id == "Has PPI parent code") %>%
  select(-contains("valid"), -relationship_id) %>%
  rename(concept_id_parent = concept_id_2) |>
  left_join(concepts, by = join_by(concept_id_parent == concept_id)) %>%
  rename(concept_name_parent = concept_name, concept_class_id_parent = concept_class_id) %>%
  left_join(concepts, by = join_by(concept_id_question == concept_id), suffix = c("", "_question")) %>%
  left_join(concepts, by = join_by(concept_id_answer == concept_id), suffix = c("", "_answer")) %>%
  mutate(concept_class_id_parent = ifelse(concept_class_id_parent == "Answer", "Answer", "Other")) %>%
  distinct(concept_class_id_parent, concept_id_parent, concept_id_answer, concept_id_question) %>%
  collect()

# this is the codebook for the health history questions
codebook_only_health_history <- filter(aou_codebook_w_health_history, str_detect(form_name, "History")) %>%
  filter(str_detect(concept_code, "currently$") |
           str_detect(concept_code, "\\_rx") |
           str_detect(concept_code, "prescribedmeds") |
           str_detect(concept_code, "\\_yes$") |
           str_detect(concept_code, "howoldwereyou"))

sub_conditions_questions <- codebook_only_health_history %>%
  filter(str_detect(concept_code, "currently$") |
           str_detect(concept_code, "\\_rx") |
           str_detect(concept_code, "prescribedmeds") |
           str_detect(concept_code, "howoldwereyou"))


test <- tbl(con, inDatabaseSchema(cdm_schema, "concept_relationship")) %>%
  filter(concept_id_1 %in% !! sub_conditions_questions$concept_id) %>%
  filter(relationship_id == "Has PPI parent code") %>%
  select(-contains("valid"), -relationship_id) %>%
  collect()


conditions_table <- conditions_table %>%
  left_join(test, by = join_by(concept_id_specific == concept_id_2)) %>%
  left_join(select(sub_conditions_questions, concept_id, concept_code),
            by = join_by(concept_id_1 == concept_id), suffix = c("", "_sub")) %>%
  rename(concept_id_sub = concept_id_1) %>%
  mutate(question_sub = case_when(
    str_detect(concept_code_sub, "currently$") ~ "on_txt",
    str_detect(concept_code_sub, "\\_rx") ~ "rx_meds",
    str_detect(concept_code_sub, "prescribedmeds") ~ "rx_meds",
    str_detect(concept_code_sub, "howoldwereyou") ~ "age_diagnosis"
  )) %>%
  pivot_wider(names_from = question_sub, values_from = c(concept_id_sub, concept_code_sub),
              names_sep = "_", values_fn = first) %>%
  select(-ends_with("NA")) %>%
  rename_with(~str_remove(.x, "_sub"))


# if there is no concept_id_parent for which the concept_class_id_parent == "Answer",
# then they didn't ask it about the whole family in the first set of surveys
## these are the ones that don't have a parent question
health_history_codebook <- conditions_table %>%
  left_join(all_overall_concept_id,
                      by = join_by(concept_id_specific == concept_id_answer)) %>%
  distinct(question, relative, condition, category, concept_code, concept_id_specific,
           concept_id_overall, concept_id_question, concept_id_rx_meds, concept_id_on_txt,
           concept_id_age_diagnosis, concept_code_rx_meds, concept_code_on_txt, concept_code_age_diagnosis) %>%
  mutate(concept_code = str_replace(concept_code, "conditions", "condition"),
         concept_code = ifelse(relative == "self", paste0(concept_code, "_yes"), concept_code)) %>%
  left_join(codebook_only_health_history, by = "concept_code", relationship = "many-to-many") %>%
  select(-c(concept_id, concept_name, concept_class_id, field_type, field_label,
            choices, standard_concept, valid_start_date, valid_end_date, link))

usethis::use_data(health_history_codebook, overwrite = TRUE)
