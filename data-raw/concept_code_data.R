library(dplyr)
library(tidyr)
library(stringr)

# these are the codes not replaced even when clean_answers = TRUE
aou_concept_codes <- aou_codebook |>
  # filter(form_name == "Social Determinants of Health" | stringr::str_detec(form_name, "COPE")) |>
  distinct(choices) |>
  separate_longer_delim(choices, "|") |>
  separate(choices, into = c("code", "answer"), sep = ",", extra = "merge") |>
  mutate(across(everything(), str_squish)) |>
  distinct(code, answer) |>
  filter(!is.na(answer)) |>
  filter(str_detect(code, "cope_") | str_detect(code, "SDOH_") | !str_detect(code, "_"))


usethis::use_data(aou_concept_codes, overwrite = TRUE)
