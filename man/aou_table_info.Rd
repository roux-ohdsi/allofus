% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{aou_table_info}
\alias{aou_table_info}
\title{Table of tables, columns, and use for researchers from the CT data dictionary}
\format{
\code{aou_table_info}
\describe{
\item{table_name}{chr; name of the table}
\item{columns}{chr; columns in the table}
\item{recommended_for_research}{chr; whether the table is recomended for research}
}
}
\usage{
aou_table_info
}
\description{
A data from with rows of the All of Us codebook pertaining to
the health history questions. In early All of Us surveys, these questions
were asked separately about the respondent and the respondent's family. In
the current version, the questions are asked on the same survey. The nested
nature of these questions can make them challenging to extract and analyze.

\itemize{
\item{\href{https://github.com/roux-ohdsi/allofus/blob/main/data-raw/more_data.R}{Code to generate table}}
}
}
\keyword{datasets}
