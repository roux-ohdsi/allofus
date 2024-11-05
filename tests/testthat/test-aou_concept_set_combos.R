testthat::test_that("All dataframes in 'out' list are equivalent", {

skip_workbench()
con <- aou_connect()

if (!rlang::is_interactive()) {
  aou_concept_set <- purrr::possibly(aou_concept_set, otherwise = NA)
  collect <- purrr::possibly(dplyr::collect, otherwise = NA)
  arrange <- purrr::possibly(dplyr::arrange, otherwise = NA)
}

eligible <- dplyr::tbl(con, "person") %>%
  head(10000) %>%
  dplyr::select(person_id) %>%
  dplyr::mutate(
    first_eligible_date = as.Date("2018-01-01"),
    end_study_date = as.Date("2023-01-01")
  ) %>%
  collect()

eligible2 <- dplyr::tbl(con, "person") %>%
  dplyr::select(person_id) %>%
  dplyr::mutate(
    first_eligible_date = as.Date("2018-01-01"),
    end_study_date = as.Date("2023-01-01")
  ) %>%
  head(10000)

eligible3 <- dplyr::tbl(con, "person") %>%
  head(10000) %>%
  dplyr::select(person_id) %>%
  dplyr::mutate(
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2023-01-01")
  ) %>%
  collect()

eligible4 <- dplyr::tbl(con, "person") %>%
  dplyr::select(person_id) %>%
  dplyr::mutate(
    start_date = as.Date("2018-01-01"),
    end_date = as.Date("2023-01-01")
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

suppressWarnings({
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
})

suppressWarnings({
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
})

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

suppressWarnings({
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
})

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

suppressWarnings({
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
})

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


out_equivalent <- purrr::map(out, \(dat) dplyr::select(dat, person_id, concept_date, concept_id, concept_name, concept_domain))

  testthat::expect_true(
    all(!is.na(out)),
    "Not all dataframes are equal"
  )

  testthat::expect_equal(
    purrr::map_dbl(out, ncol),
    rep(7, length(out))
  )

  expect_true(is.list(out_equivalent))
  expect_gt(length(out_equivalent), 1)

  all_same <- all(purrr::map_lgl(out_equivalent[-1], identical, out_equivalent[[1]]))
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


