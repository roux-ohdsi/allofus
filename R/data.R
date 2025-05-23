#' All of Us Modified Codebook
#'
#' @description A data frame with rows from the publicly available All of Us
#'   Survey Codebook mapped to the All of Us PPI Vocabulary available on Athena.
#'   A small number of rows did not match between the codebook and the Athena
#'   PPI Vocabulary.
#'
#' @details Questions relating to specific conditions are not included as part
#'   of this table. They are instead available in the `aou_health_history`
#'   table.
#'
#' \itemize{
#'    \item{\href{https://docs.google.com/spreadsheets/d/1Ey8MScRYZ9QyS4izVYScISLMb62QEhSM-ErbG27dNtw/edit#gid=1832128489}{All of Us codebook}}
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/pkg_data.R}{Code to generate table}}
#' }
#'
#' @format `aou_codebook` A data frame with 702 rows and 11 columns:
#' \describe{
#'   \item{concept_code}{chr; Concept code from AOU codebook}
#'   \item{concept_id}{int; mapped concept_id from PPI vocabulary}
#'   \item{concept_name}{chr; Formatted text name of concept}
#'   \item{concept_class_id}{chr; type of survey item - question or answer}
#'   \item{form_name}{int; name of survey}
#'   \item{field_type}{chr; type of question (radio, text, checkbox etc.)}
#'   \item{field_label}{chr; The actual text of the question or answer}
#'   \item{choices}{int; choices for question if radio or checkbox}
#'   \item{standard_concept}{chr; Whether concept_id is a standard omop concept}
#'   \item{valid_start_Date}{chr; start date for concept}
#'   \item{valid_end_Date}{int; end date for concept}
#'   \item{link}{chr; link to survey pdf}
#' }
"aou_codebook"


#' All of Us Health History Codebook
#'
#' This table consists of rows of the codebook pertaining to the health history
#' questions. In early All of Us surveys, these questions were asked separately
#' about the respondent and the respondent's family. In the current version, the
#' questions are asked on the same survey. The nested nature of these questions
#' makes them challenging to deal with. It can also be accessed in R using
#' `allofus::aou_health_history`.
#' \itemize{
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/pkg_data.R}{Code to generate table}}
#' }
#'
#' @format `aou_health_history` A data frame with 1685  rows and 9 columns:
#' \describe{
#'   \item{question}{chr; Question asked on survey}
#'   \item{relative}{chr; Person to whom the answer pertains}
#'   \item{condition}{chr; Formatted text name of concept}
#'   \item{category}{chr; Type of health condition}
#'   \item{concept_code}{chr; Concept code from AOU codebook}
#'   \item{concept_id_specific}{int; Concept id for the answer}
#'   \item{concept_id_overall}{int; Concept id for the condition overall}
#'   \item{concept_id_question}{int; Concept id for the overarching question}
#'   \item{form_name}{chr; Survey name}
#' }
"aou_health_history"

#' Table of tables, columns, and use for researchers from the CT data dictionary
#'
#' @description A data from with rows of the All of Us codebook pertaining to
#'   the health history questions. In early All of Us surveys, these questions
#'   were asked separately about the respondent and the respondent's family. In
#'   the current version, the questions are asked on the same survey. The nested
#'   nature of these questions can make them challenging to extract and analyze.
#'
#' \itemize{
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/more_data.R}{Code to generate table}}
#' }
#'
#' @format `aou_table_info`
#' \describe{
#'   \item{table_name}{chr; name of the table}
#'   \item{columns}{chr; columns in the table}
#'   \item{recommended_for_research}{chr; whether the table is recomended for research}
#' }
"aou_table_info"

#' Example cohort definition and SQL query from ATLAS
#'
#' @description A data frame containing tables and columns in the All of Us
#'   Controlled Tier Dataset v7 CDR Data Dictionary (C2022Q4R9) found here:
#'   https://docs.google.com/spreadsheets/d/1XLVq84LLd0VZMioF2sPwyiaPw3EFp5c8o1CTWGPH-Yc/edit#gid=1815943286.
#'   Note that the column 'value_source_value' has been manually added as it is
#'   missing from the data dictionary.
#' @keywords internal
#' @format `aou_cohort_example`
#' \describe{
#'   \item{cd}{Cohort definition object}
#'   \item{cd_sql}{SQL query for cohort definition}
#' }
"aou_cohort_example"


#' Concept codes and survey answers
#'
#' @description
#' A data frame containing concept codes (`code`) and text responses (`answer`) for
#' the SDOH and COPE surveys.
#'
#' @format aou_concept_codes
#' \describe{
#'   \item{code}{response from the observation table}
#'   \item{answer}{Text responses}
#' }
"aou_concept_codes"
