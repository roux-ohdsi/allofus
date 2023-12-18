library(dplyr)
con <- allofus:::aou_test_connect()

# test case 1: return indicator with default concept set name and min_n
result1 <- aou_concept_set(
  cohort = tbl(con, "observation_period"),
  concepts = c(4024958, 4323208, 433644, 378001),
  start_date = observation_period_start_date,
  end_date = observation_period_end_date,
  domains = c("condition", "measurement", "observation"),
  return = "indicator"
)
expect_equal(nrow(result1), 2690)

# test case 2: only return indicator if min_n > 10
result2 <- aou_concept_set(
  cohort = tbl(con, "observation_period"),
  concepts = c(4024958, 4323208, 433644, 378001),
  start_date = observation_period_start_date,
  end_date = observation_period_end_date,
  domains = c("condition", "measurement", "observation"),
  return = "indicator",
  min_n = 10
)
expect_equal(nrow(result2), 1497)

# test case 3: return count with custom concept set name and min_n
result3 <- aou_concept_set(
  cohort = tbl(con, "observation_period"),
  concepts = c(4024958, 4323208, 433644, 378001),
  start_date = observation_period_start_date,
  end_date = observation_period_end_date,
  domains = c("condition", "measurement", "observation"),
  return = "count",
  concept_set_name = "my_concept_set"
)
expect_equal(min(result3$n), 1)
expect_equal(max(result3$n), 155)
expect_equal(
  result3[result3$n >= 10, "person_id"],
  result2[, "person_id"]
)
# should have column "concept_set" with value "my_concept_set"
expect_equal(unique(result3$concept_set), "my_concept_set")

# test case 4: return all with custom concept set name
result4 <- aou_concept_set(
  cohort = tbl(con, "observation_period"),
  concepts = c(4024958, 4323208, 433644, 378001),
  start_date = observation_period_start_date,
  end_date = observation_period_end_date,
  domains = c("condition", "measurement", "observation"),
  return = "all",
  concept_set_name = "my_concept_set"
)

# should have columns date, concept_id, concept_name, domain, concept_set
expect_equal(colnames(result4), c("person_id", "date", "concept_id", "concept_name", "domain", "concept_set"))
# should have many more rows
expect_equal(nrow(result4), 45012)
