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
    shiny::uiOutput(outputId = shiny::NS(id,"uniParcoord"), width = "100%", height = "800px"),
    shiny::uiOutput(outputId = shiny::NS(id, "multiPlot"), width = "100%", height = "800px")
  )
}

uniParcoordServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$uniParcoord <- shiny::renderUI({
      if(length(input$catVar)==1){
        shiny::renderPlot({
          parallel_coordinate_plot(data(), input$numVar, input$catVar)
        },res = 96)
      }
    })
  })
}
