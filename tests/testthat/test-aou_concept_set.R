# returns warning
aou_concept_set(concepts = 4324693, concept_set_name = "mammogram") %>%
  count(mammogram)

tbl(con, "person") %>%
  mutate(
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2022-01-01")
  ) %>%
  filter(person_id < 10000000) %>%
  aou_concept_set(
    concepts = 4324693, start_date = "start_date", end_date = "end_date",
    concept_set_name = "mammogram",
    domain = "procedure", output = "count"
  ) %>%
  count(mammogram)

# returns warning
data.frame(
  person_id = c(2150822L, 5252380L),
  start_date = as.Date("2019-01-01"), end_date = as.Date("2023-01-01")
) %>%
  aou_concept_set(
    concepts = 4324693, start_date = "start_date", end_date = "end_date",
    concept_set_name = "mammogram",
    domain = "procedure", output = "all"
  )
