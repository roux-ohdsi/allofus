library(shiny)
library(DT)

ui <- fluidPage(

  DT::DTOutput("t")

)

server <- function(input, output, session) {

  output$t <- DT::renderDT({

   cols = c("concept_code", "concept_id", "concept_name", "concept_class_id",
            "form_name", "field_type", "fild_label", "choices")

    DT::datatable(allofus::aou_codebook[,cols],
                  rownames = FALSE,
                  filter = 'top',
                  plugins = "ellipsis",
                  options = list(
                    pageLength = 20,
                    scrollX = TRUE,
                    scrollCollapse = TRUE,
                    columnDefs = list(list(
                      targets = c(6,7),
                      render = DT::JS("$.fn.dataTable.render.ellipsis( 20, false )")
                    ))
                  )) |>
                    DT::formatStyle(table = _, columns = cols, fontSize = '80%')
  })

}

shinyApp(ui, server)
