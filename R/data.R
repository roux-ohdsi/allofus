#' All of Us Modified Codebook
#'
#' This table consists of mapped rows from the
#' publicly available AllofUs Survey Codebook
#' to the AllofUs PPI Vocabulary available on Athena . A small number of rows did not
#' match between the codebook and the Athena PPI Vocabulary. It can also be accessed in R using `allofus::aou_codebook`.
#'
#' Questions relating to specific conditions are not included as part of this table.
#' They are instead available in the `health_history_codebook` table.
#' \itemize{
#'    \item{\href{https://docs.google.com/spreadsheets/d/1Ey8MScRYZ9QyS4izVYScISLMb62QEhSM-ErbG27dNtw/edit#gid=1832128489}{AllofUs codebook}}
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/pkg_data.R}{Code to generate table}}
#' }
#'
#' @format `aou_codebook`
#' A data frame with 702 rows and 11 columns:
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
#' This table consists of rows of the codebook pertaining to the health history questions. In early All of Us surveys,
#' these questions were asked separately about the respondent and the respondent's family. In the current version,
#' the questions are asked on the same survey. The nested nature of these questions makes them challenging to deal with.
#' It can also be accessed in R using `allofus::health_history_codebook`.
#' \itemize{
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/pkg_data.R}{Code to generate table}}
#' }
#'
#' @format `health_history_codebook`
#' A data frame with 1685  rows and 9 columns:
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
"health_history_codebook"

#' Table of tables, columns, and use for researchers from the CT data dictionary
#' \itemize{
#'    \item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/more_data.R}{Code to generate table}}
#' }
#'
#' @format `health_history_codebook`
#' A data frame with 1685  rows and 9 columns:
#' \describe{
#'   \item{table_name}{chr; name of the table}
#'   \item{columns}{chr; columns in the table}
#'   \item{recommended_for_research}{chr; whether the table is recomended for research}
#' }
"aou_table_info"


