## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  library(allofus)
#  con <- aou_connect()

## -----------------------------------------------------------------------------
#  aou_tables()

## ----echo = FALSE, eval = TRUE------------------------------------------------
library(tibble)
structure(list(table_name = c(
  "concept_ancestor", "cb_criteria_ancestor",
  "attribute_definition", "cdm_source", "concept_class", "concept",
  "concept_synonym", "cb_criteria_relationship", "concept_relationship",
  "condition_occurrence", "activity_summary", "heart_rate_minute_level",
  "steps_intraday", "device_exposure", "fact_relationship", "domain",
  "drug_strength", "drug_exposure", "cb_criteria_attribute", "ds_linking",
  "ds_data_dictionary", "cb_criteria_menu", "cb_criteria", "cb_survey_attribute",
  "measurement", "observation", "observation_period", "ds_condition_occurrence",
  "cb_review_survey", "cb_review_all_events", "ds_activity_summary",
  "ds_heart_rate_summary", "heart_rate_summary", "ds_heart_rate_minute_level",
  "ds_steps_intraday", "death", "ds_device", "ds_drug_exposure",
  "cb_search_all_events", "cb_search_person", "ds_person", "person",
  "ds_measurement", "ds_observation", "ds_zip_code_socioeconomic",
  "ds_procedure_occurrence", "ds_sleep_level", "sleep_daily_summary",
  "sleep_level", "ds_survey", "ds_visit_occurrence", "procedure_occurrence",
  "relationship", "specimen", "condition_occurrence_ext", "device_exposure_ext",
  "drug_exposure_ext", "measurement_ext", "observation_ext", "procedure_occurrence_ext",
  "visit_occurrence_ext", "person_ext", "survey_conduct", "survey_conduct_ext",
  "cb_survey_version", "visit_detail", "visit_occurrence", "vocabulary"
), columns = c(
  "ancestor_concept_id, descendant_concept_id, min_levels_of_separation, max_levels_of_separation",
  "ancestor_id, descendant_id", "attribute_definition_id, attribute_name, attribute_description, attribute_type_concept_id, attribute_syntax",
  "cdm_source_name, cdm_source_abbreviation, cdm_holder, source_description, source_documentation_reference, cdm_etl_reference, source_release_date, cdm_release_date, cdm_version, vocabulary_version",
  "concept_class_id, concept_class_name, concept_class_concept_id",
  "concept_id, concept_name, domain_id, vocabulary_id, concept_class_id, standard_concept, concept_code, valid_start_date, valid_end_date, invalid_reason",
  "concept_id, concept_synonym_name, language_concept_id", "concept_id_1, concept_id_2",
  "concept_id_1, concept_id_2, relationship_id, valid_start_date, valid_end_date, invalid_reason",
  "condition_occurrence_id, person_id, condition_concept_id, condition_start_date, condition_start_datetime, condition_end_date, condition_end_datetime, condition_type_concept_id, stop_reason, provider_id, visit_occurrence_id, condition_source_value, condition_source_concept_id, condition_status_source_value, condition_status_concept_id",
  "date, activity_calories, calories_bmr, calories_out, elevation, fairly_active_minutes, floors, lightly_active_minutes, marginal_calories, sedentary_minutes, steps, very_active_minutes, person_id",
  "datetime, person_id, heart_rate_value", "datetime, steps, person_id",
  "device_exposure_id, person_id, device_concept_id, device_exposure_start_date, device_exposure_start_datetime, device_exposure_end_date, device_exposure_end_datetime, device_type_concept_id, unique_device_id, quantity, provider_id, visit_occurrence_id, visit_detail_id, device_source_value, device_source_concept_id",
  "domain_concept_id_1, fact_id_1, domain_concept_id_2, fact_id_2, relationship_concept_id",
  "domain_id, domain_name, domain_concept_id", "drug_concept_id, ingredient_concept_id, amount_value, amount_unit_concept_id, numerator_value, numerator_unit_concept_id, denominator_value, denominator_unit_concept_id, box_size, valid_start_date, valid_end_date, invalid_reason",
  "drug_exposure_id, person_id, drug_concept_id, drug_exposure_start_date, drug_exposure_start_datetime, drug_exposure_end_date, drug_exposure_end_datetime, verbatim_end_date, drug_type_concept_id, stop_reason, refills, quantity, days_supply, sig, route_concept_id, lot_number, provider_id, visit_occurrence_id, visit_detail_id, drug_source_value, drug_source_concept_id, route_source_value, dose_unit_source_value",
  "id, concept_id, value_as_concept_id, concept_name, type, est_count",
  "id, denormalized_name, omop_sql, join_value, domain", "id, field_name, relevant_omop_table, description, field type, omop_cdm_standard_or_custom_field, data_provenance, source_ppi_module, domain",
  "id, parent_id, category, domain_id, type, name, is_group, sort_order",
  "id, parent_id, domain_id, is_standard, type, subtype, concept_id, code, name, value, est_count, is_group, is_selectable, has_attribute, has_hierarchy, has_ancestor_data, path, synonyms, rollup_count, item_count, full_text, display_synonyms",
  "id, question_concept_id, answer_concept_id, survey_version_concept_id, item_count",
  "measurement_id, person_id, measurement_concept_id, measurement_date, measurement_datetime, measurement_type_concept_id, operator_concept_id, value_as_number, value_as_concept_id, unit_concept_id, range_low, range_high, provider_id, visit_occurrence_id, visit_detail_id, measurement_source_value, measurement_source_concept_id, unit_source_value, value_source_value",
  "observation_id, person_id, observation_concept_id, observation_date, observation_datetime, observation_type_concept_id, value_as_number, value_as_string, value_as_concept_id, qualifier_concept_id, unit_concept_id, provider_id, visit_occurrence_id, visit_detail_id, observation_source_value, observation_source_concept_id, value_source_concept_id, unit_source_value, qualifier_source_value, value_source_value",
  "observation_period_id, person_id, observation_period_start_date, observation_period_end_date, period_type_concept_id",
  "person_id, condition_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, condition_start_datetime, condition_end_datetime, condition_type_concept_id, condition_type_concept_name, stop_reason, visit_occurrence_id, visit_occurrence_concept_name, condition_source_value, condition_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, condition_status_source_value, condition_status_concept_id, condition_status_concept_name",
  "person_id, data_id, start_datetime, survey, question, answer",
  "person_id, data_id, start_datetime, visit_type, standard_code, standard_vocabulary, standard_name, standard_concept_id, source_code, source_vocabulary, source_name, source_concept_id, route, dose, strength, value_as_number, unit, ref_range, domain, age_at_event, num_mentions, first_mention, last_mention",
  "person_id, date, activity_calories, calories_bmr, calories_out, elevation, fairly_active_minutes, floors, lightly_active_minutes, marginal_calories, sedentary_minutes, steps, very_active_minutes",
  "person_id, date, zone_name, min_heart_rate, max_heart_rate, minute_in_zone, calorie_count",
  "person_id, date, zone_name, min_heart_rate, max_heart_rate, minute_in_zone, calorie_count",
  "person_id, datetime, heart_rate_value", "person_id, datetime, steps",
  "person_id, death_date, death_datetime, death_type_concept_id, cause_concept_id, cause_source_value, cause_source_concept_id",
  "person_id, device_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, device_exposure_start_datetime, device_exposure_end_datetime, device_type_concept_id, device_type_concept_name, visit_occurrence_id, visit_occurrence_concept_name, device_source_value, device_source_concept_id, source_concept_name, source_concept_code, source_vocabulary",
  "person_id, drug_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, drug_exposure_start_datetime, drug_exposure_end_datetime, verbatim_end_date, drug_type_concept_id, drug_type_concept_name, stop_reason, refills, quantity, days_supply, sig, route_concept_id, route_concept_name, lot_number, visit_occurrence_id, visit_occurrence_concept_name, drug_source_value, drug_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, route_source_value, dose_unit_source_value",
  "person_id, entry_date, is_standard, concept_id, domain, age_at_event, visit_concept_id, visit_occurrence_id, value_as_number, value_as_concept_id, entry_datetime, value_source_concept_id, systolic, diastolic, survey_version_concept_id, survey_concept_id, cati_concept_id",
  "person_id, gender, sex_at_ birth, race, ethnicity, dob, age_at_consent, age_at_cdr, has_ehr_data, has_ppi_survey_data, has_physical_measurement_data, is_deceased, has_fitbit, has_fitbit_activity_summary, has_fitbit_heart_rate_summary, has_fitbit_heart_rate_level, has_fitbit_steps_intraday, has_fitbit_sleep_daily_summary, has_fitbit_sleep_level, has_whole_genome_variant, has_array_data, has_lr_whole_genome_variant, has_structural_variant_data, state_of_residence",
  "person_id, gender_concept_id, gender, date_of_birth, race_concept_id, race, ethnicity_concept_id, ethnicity, sex_at_birth_concept_id, sex_at_birth",
  "person_id, gender_concept_id, year_of_birth, month_of_birth, day_of_birth, birth_datetime, race_concept_id, ethnicity_concept_id, location_id, provider_id, care_site_id, person_source_value, gender_source_value, gender_source_concept_id, race_source_value, race_source_concept_id, ethnicity_source_value, ethnicity_source_concept_id, sex_at_birth_concept_id, sex_at_birth_source_value, sex_at_birth_source_concept_id",
  "person_id, measurement_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, measurement_datetime, measurement_type_concept_id, measurement_type_concept_name, operator_concept_id, operator_concept_name, value_as_number, value_as_concept_id, value_as_concept_name, unit_concept_id, unit_concept_name, range_low, range_high, visit_occurrence_id, visit_occurrence_concept_name, measurement_source_value, measurement_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, unit_source_value, value_source_value",
  "person_id, observation_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, observation_datetime, observation_type_concept_id, observation_type_concept_name, value_as_number, value_as_string, value_as_concept_id, value_as_concept_name, qualifier_concept_id, qualifier_concept_name, unit_concept_id, unit_concept_name, visit_occurrence_id, visit_occurrence_concept_name, observation_source_value, observation_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, unit_source_value, qualifier_source_value, value_source_concept_id, value_source_value, questionnaire_response_id",
  "person_id, observation_datetime, zip3_as_string, fraction_assisted_income, fraction_high_school_edu, median_income, fraction_no_health_ins, fraction_poverty, fraction_vacant_housing, deprivation_index, acs",
  "person_id, procedure_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, procedure_datetime, procedure_type_concept_id, procedure_type_concept_name, modifier_concept_id, modifier_concept_name, quantity, visit_occurrence_id, visit_occurrence_concept_name, procedure_source_value, procedure_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, modifier_source_value, person_id, procedure_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, procedure_datetime, procedure_type_concept_id, procedure_type_concept_name, modifier_concept_id, modifier_concept_name, quantity, visit_occurrence_id, visit_occurrence_concept_name, procedure_source_value, procedure_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, qualifier_source_value",
  "person_id, sleep_date, is_main_sleep, level, start_datetime, duration_in_min",
  "person_id, sleep_date, is_main_sleep, minute_in_bed, minute_asleep, minute_after_wakeup, minute_awake, minute_restless, minute_deep, minute_light, minute_rem, minute_wake",
  "person_id, sleep_date, start_datetime, is_main_sleep, level, duration_in_min",
  "person_id, survey_datetime, survey, question_concept_id, question, answer_concept_id, answer, survey_version_concept_id, survey_version_name",
  "person_id, visit_concept_id, standard_concept_name, standard_concept_code, standard_vocabulary, visit_start_datetime, visit_end_datetime, visit_type_concept_id, visit_type_concept_name, visit_source_value, visit_source_concept_id, source_concept_name, source_concept_code, source_vocabulary, admitting_source_concept_id, admitting_source_concept_name, admitting_source_value, discharge_to_concept_id, discharge_to_concept_name, discharge_to_source_value",
  "procedure_occurrence_id, person_id, procedure_concept_id, procedure_date, procedure_datetime, procedure_type_concept_id, modifier_concept_id, quantity, provider_id, visit_occurrence_id, procedure_source_value, procedure_source_concept_id, qualifier_source_value",
  "relationship_id, relationship_name, is_hierarchical, defines_ancestry, reverse_relationship_id, relationship_concept_id",
  "specimen_id, person_id, specimen_concept_id, specimen_type_concept_id, specimen_date, specimen_datetime, quantity, unit_concept_id, anatomic_site_concept_id, disease_status_concept_id, specimen_source_id, specimen_source_value, unit_source_value, anatomic_site_source_value, disease_status_source_value",
  "src_id, condition_occurrence_id", "src_id, device_exposure_id",
  "src_id, drug_exposure_id", "src_id, measurement_id", "src_id, observation_id",
  "src_id, procedure_occurrence_id", "src_id, visit_occurrence_id",
  "state_of_residence_concept_id, state_of_residence_source_value",
  "survey_conduct_id, person_id, survey_concept_id, survey_start_date, survey_start_datetime, survey_end_date, survey_end_datetime, survey_source_value, provider_id, respondent_type_concept_id, timing_concept_id, respondent_type_source_value, timing_source_value, survey_source_concept_id, survey_source_identifier, assisted_concept_id, assisted_source_value, collection_method_concept_id, collection_method_source_value, validated_survey_concept_id, validated_survey_source_value, survey_version_number, visit_occurrence_id, response_visit_occurrence_id",
  "survey_conduct_id, src_id, language", "survey_version_concept_id, survey_concept_id, display_name, display_order",
  "visit_detail_id, person_id, visit_detail_concept_id, visit_detail_start_date, visit_detail_start_datetime, visit_detail_end_date, visit_detail_end_datetime, visit_detail_type_concept_id, provider_id, care_site_id, visit_detail_source_value, visit_detail_source_concept_id, admitting_source_value, admitting_source_concept_id, discharge_to_source_value, discharge_to_concept_id, preceding_visit_detail_id, visit_detail_parent_id, visit_occurrence_id",
  "visit_occurrence_id, person_id, visit_concept_id, visit_start_date, visit_start_datetime, visit_end_date, visit_end_datetime, visit_type_concept_id, provider_id, care_site_id, visit_source_value, visit_source_concept_id, admitting_source_concept_id, admitting_source_value, discharge_to_concept_id, discharge_to_source_value, preceding_visit_occurrence_id",
  "vocabulary_id, vocabulary_name, vocabulary_reference, vocabulary_version, vocabulary_concept_id"
)), class = c("tbl_df", "tbl", "data.frame"), row.names = c(
  NA,
  -68L
))

## -----------------------------------------------------------------------------
#  person_tbl <- tbl(con, "person")

## -----------------------------------------------------------------------------
#  person_tbl

## -----------------------------------------------------------------------------
#  young_women <- person_tbl %>%
#    filter(gender_concept_id == 45878463, year_of_birth > 1980)

## -----------------------------------------------------------------------------
#  young_women

## -----------------------------------------------------------------------------
#  tally(young_women)

## -----------------------------------------------------------------------------
#  young_women %>% collect()

## -----------------------------------------------------------------------------
#  tally(young_women) %>% collect()

## -----------------------------------------------------------------------------
#  person_data <- person_tbl %>% collect()
#  person_data

## -----------------------------------------------------------------------------
#  concept_tbl <- tbl(con, "concept") %>%
#    select(concept_id, concept_name)
#  concept_tbl

## -----------------------------------------------------------------------------
#  genders_in_aou <- person_tbl %>%
#    count(gender_concept_id) %>%
#    left_join(concept_tbl, by = join_by(gender_concept_id == concept_id))
#  genders_in_aou

## -----------------------------------------------------------------------------
#  obs <- person_tbl %>%
#    aou_join("observation", type = "inner", by = "person_id")

## -----------------------------------------------------------------------------
#  obs %>%
#    select(ends_with("_x"), ends_with("_y")) %>%
#    colnames()

## ----echo = FALSE, eval = TRUE------------------------------------------------
c("provider_id_x", "provider_id_y")

## -----------------------------------------------------------------------------
#  obs <- person_tbl %>%
#    select(-provider_id) %>%
#    aou_join("observation", type = "inner", by = "person_id")

## -----------------------------------------------------------------------------
#  # instead of aou_join(cohort, "activity_summary", type = "left", by = "person_id")
#  activity_data <- tbl(con, "activity_summary") %>%
#    filter(person_id %in% !!cohort$person_id) %>%
#    collect() %>%
#    right_join(cohort, by = "person_id")

## -----------------------------------------------------------------------------
#  obs %>%
#    show_query()

## -----------------------------------------------------------------------------
#  aou_sql("SELECT COUNT(*) AS n FROM {CDR}.person")

## -----------------------------------------------------------------------------
#  aou_sql("
#  SELECT count(*) AS `n`
#  FROM (
#    SELECT `person`.*
#    FROM {CDR}.`person`
#    WHERE (`gender_concept_id` = 45878463.0) AND (`year_of_birth` > 1980.0)
#  )
#  ")

