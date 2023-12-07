


# read in the allofus controlled tier data dictionary from
# google sheets at https://docs.google.com/spreadsheets/d/1XLVq84LLd0VZMioF2sPwyiaPw3EFp5c8o1CTWGPH-Yc/edit#gid=1815943286
library(googlesheets4)
library(dplyr)
library(tidyr)
library(tibble)

gs4_deauth()

df <- googlesheets4::read_sheet(
    ss = "https://docs.google.com/spreadsheets/d/1XLVq84LLd0VZMioF2sPwyiaPw3EFp5c8o1CTWGPH-Yc/edit#gid=1815943286",
    sheet = "OMOP-Compatible Tables"
) %>%
  dplyr::select(table_name = 1, column = 2) %>%
  dplyr::distinct() %>%
  tibble::add_row(table_name = "observation", column = "value_source_value") %>%
  dplyr::summarize(columns = paste(column, collapse = ", "), .by = table_name) %>%
  dplyr::mutate(recommended_for_research = "yes (omop compatible table)")

df2 <- googlesheets4::read_sheet(
  ss = "https://docs.google.com/spreadsheets/d/1XLVq84LLd0VZMioF2sPwyiaPw3EFp5c8o1CTWGPH-Yc/edit#gid=1815943286",
  sheet = "AoU Workbench UI Tool Tables"
) %>%
  dplyr::select(table_name = 1, column = 3, recommended_for_research = 2) %>%
  dplyr::summarize(columns = paste(column, collapse = ", "),
            recommended_for_research = paste(unique(recommended_for_research), collapse = ", "), .by = table_name)

df3 <- dplyr::bind_rows(df, df2)
aou_table_info <- df3
usethis::use_data(aou_table_info, overwrite = TRUE)
