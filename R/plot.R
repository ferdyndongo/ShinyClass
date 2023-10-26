plotUi <- function(id){
  shiny::plotOutput(outputId = shiny::NS(id, "plot"), width = "100%", height = "800px")
}

#' Server module rendering multiple plot containers for each level of a categoricalvariable.
#' If two or more categorical variable are selected, the first one will be chosen.
#' @param id identifier of the module
#' @param data dataframe
multiplotOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    catVar <- shiny::reactive({
      if(base::all(input$catVar=="")){
        "noClass"
      }else if(base::all(input$catVar!="")){
        unique(data()[[input$catVar[1]]])
      }
    })

    output$multiPlot <- shiny::renderUI({
      shiny::req(catVar(), input$catVar)
      if(length(input$catVar)==1 | length(input$catVar)==2){
        purrr::map(catVar(), shiny::plotOutput)
      }
    })

    return(shiny::reactive(catVar()))
  })
}
