library(allofus)
library(tidyverse)
library(testthat)
con <- aou_connect()

aou_concept_set <- purrr::possibly(aou_concept_set, otherwise = NA)
collect <- purrr::possibly(collect, otherwise = NA)
arrange <- purrr::possibly(arrange, otherwise = NA)

eligible <- tbl(con, "person") %>%
  head(10000) %>%
  select(person_id) %>%
  mutate(
    first_eligible_date = as.Date("2018-01-01"),
    end_study_date = as.Date("2023-01-01")
  ) %>%
  collect()

eligible2 <- tbl(con, "person") %>%
  select(person_id) %>%
  mutate(
    first_eligible_date = as.Date("2018-01-01"),
    end_study_date = as.Date("2023-01-01")
  ) %>%
  head(10000)

CGM_obs <- aou_concept_set(
  eligible,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
  collect = TRUE
) %>% arrange(person_id, concept_id, concept_date)


CGM_obs2 <- aou_concept_set(
  eligible2,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
  collect = TRUE
) %>% arrange(person_id, concept_id, concept_date)

CGM_obs3 <- aou_concept_set(
  eligible2,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
  collect = FALSE
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

CGM_obs4 <- aou_concept_set(
  eligible,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
  collect = FALSE
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

CGM_obs5 <- aou_concept_set(
  eligible,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

CGM_obs6 <- aou_concept_set(
  eligible2,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "first_eligible_date",
  end_date = "end_study_date",
  output = "all",
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

eligible3 <- tbl(con, "person") %>%
  head(10000) %>%
  select(person_id) %>%
  mutate(
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2023-01-01")
  ) %>%
  collect()

CGM_obs7 <- aou_concept_set(
  eligible3,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

CGM_obs8 <- aou_concept_set(
  eligible3,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
  collect = TRUE
) %>% arrange(person_id, concept_id, concept_date)

CGM_obs9 <- aou_concept_set(
  eligible3,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
  collect = FALSE
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

eligible4 <- tbl(con, "person") %>%
  select(person_id) %>%
  mutate(
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2023-01-01")
  ) %>%
  head(10000)

CGM_obs10 <- aou_concept_set(
  eligible4,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
)%>% collect() %>% arrange(person_id, concept_id, concept_date)

CGM_obs11 <- aou_concept_set(
  eligible4,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
  collect = TRUE
) %>% arrange(person_id, concept_id, concept_date)

CGM_obs12 <- aou_concept_set(
  eligible4,
  concepts = c(
    725115,
    2314092,
    2314093
  ),
  concept_set_name = "CGM_obs",
  domains = "procedure",
  start_date = "start_date",
  end_date = "end_date",
  output = "all",
  collect = FALSE
) %>% collect() %>% arrange(person_id, concept_id, concept_date)

out = list(
  CGM_obs,
  CGM_obs2,
  CGM_obs3,
  CGM_obs4,
  CGM_obs5,
  CGM_obs6,
  CGM_obs7,
  CGM_obs8,
  CGM_obs9,
  CGM_obs10,
  CGM_obs11,
  CGM_obs12
)

# test that they're not all NA

testthat::test_that("aou_survey returns all dataframes", {
  testthat::expect_true(
    all(!is.na(out))
  )
})

# row_counts <- map_int(out2, nrow)
#
# all_identical <- all(map_lgl(out2[-1], identical, out2[1]))
# identical(out2[1], out2[3])

# waldo::compare(CGM_obs5, CGM_obs6)

testthat::test_that("All dataframes in 'out' list are equivalent", {
  expect_true(is.list(out))
  expect_gt(length(out), 1)

  all_same <- all(map_lgl(out[-1], identical, out[[1]]))
  expect_true(all_same, "Not all dataframes in the list are identical")
})

# compare_sequential <- map(
#   seq_len(length(out)-1),
#   function(i) {
#     list(
#       comparison = paste("df", i, "vs df", i+1),
#       result = waldo::compare(out[[i]], out[[i+1]], x_arg = paste0("df", i), y_arg = paste0("df", i+1), )
#     )
#   }
# )
#
# walk(compare_sequential, function(x) {
#   cat("\n", x$comparison, ":\n")
#   print(x$result)
# })


