parallel_coordinate_plot <- function(data, numVar=NULL, catVar=NULL, var=NULL){
  if(!is.null(var)){
    data <- data[data[which(colnames(data)==catVar[1])]==var,]
  }
  if(!(is.null(catVar) | is.null(numVar)) && length(catVar)==1){
    if(all(catVar=="") && all(numVar=="")){
      GGally::ggparcoord(data = data, columns = numericIndex(data)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }else if(all(catVar=="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data = data, columns = which(colnames(data) %in% numVar)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar=="")){
      GGally::ggparcoord(data, columns = numericIndex(data), groupColumn = catVar, title = paste("~", catVar)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data, columns = which(colnames(data) %in% numVar), groupColumn = catVar,
                         title = paste("~", catVar)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
  }else if(!(is.null(catVar) | is.null(numVar) | is.null(var)) && length(catVar)==2){
    if(all(catVar!="") && all(numVar=="")){
      GGally::ggparcoord(data, columns = numericIndex(data), groupColumn = catVar[2],
                         title = paste(var, "~", catVar[2])) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }else if(all(catVar!="") && all(numVar!="") && length(numVar)>1 ){
      GGally::ggparcoord(data, columns = which(colnames(data) %in% numVar), groupColumn = catVar[2],
                         title = paste(var, "~", catVar[2])) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
    }
  }
}


#' User Interface containers for parallel coordinate plots with inputs and outputs
#' @param id module identifier
parcoordUi <- function(id){
  shiny::tagList(
    shiny::plotOutput(outputId = shiny::NS(id,"uniParcoord"), width = "100%", height = "800px"),
    shiny::uiOutput(outputId = shiny::NS(id, "multiPlot"))
  )
}

uniParcoordServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    uniParcoord <- shiny::reactive({
      if(length(input$catVar)==1){
        parallel_coordinate_plot(data(), input$numVar, input$catVar)
      }
    })
    output$uniParcoord <- shiny::renderPlot({
      shiny::req(uniParcoord())
      uniParcoord() + ggplot2::geom_text(mapping = ggplot2::aes(x = uniParcoord()$data$variable,
                                              y = round(uniParcoord()$data$value,2),label=uniParcoord()$data$.ID))
    })

  })
}

#' Server module rendering a parallel plot coordinate. It can be linked with parcoordUi by id.
#' @param id identifier of the module
#' @param data dataframe

multiplotOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    catVar <- shiny::reactive({
      if(base::all(input$catVar=="")){
        "noClass"
      }else if(base::all(input$catVar!="")){
        if(any(is.na(unique(data()[[input$catVar[1]]])))){
          shiny::showNotification(paste0("ERROR !!!: missing values in variable ", input$catVar),duration = NULL,closeButton = TRUE,type = "error")
          unique(data()[[input$catVar[1]]])[!is.na(unique(data()[[input$catVar[1]]]))]
        }else if(all(!is.na(unique(data()[[input$catVar[1]]])))){
          unique(data()[[input$catVar[1]]])
        }

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
