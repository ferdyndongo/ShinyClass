#' User Interface for boxplot with inputs and outputs
#' @param id module identifier
boxplotUi <- function(id){
  shiny::uiOutput(shiny::NS(id, "boxplot"))
}

numeric_plotOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    numVar <- shiny::reactive({
      if(base::all(input$numVar!="")){
        input$numVar
      }else if(base::all(input$numVar=="")){
        names(numericIndex(data()))
      }
    })

    output$boxplot <- shiny::renderUI({
      purrr::map(numVar(), shiny::plotOutput)
    })

    return(shiny::reactive(numVar()))
  })
}

box_plot <- function(data, numVar=NULL, catVar=NULL){
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="")){
      # graphics::boxplot(data[[numVar]],horizontal=TRUE,main=numVar)
      ggplot2::ggplot(data = data,mapping = ggplot2::aes(x = get(numVar))) + ggplot2::geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
        ggplot2::xlab(numVar)
    }else if(all(catVar!="") && length(catVar)==1){
      # graphics::boxplot(formula(paste(numVar,"~",catVar)),data=data,horizontal=TRUE,las=1,main=paste(numVar,"~",catVar))
      ggplot2::ggplot(data = data,mapping = ggplot2::aes(x = get(numVar),y = get(catVar))) + ggplot2::geom_boxplot(outlier.colour="red", outlier.shape=8,outlier.size=4) +
        ggplot2::xlab(numVar) + ggplot2::ylab(catVar)
    }
  }
}


#' Server module rendering a boxplot. It can be linked with plotUi by id.
#' @param id module identifier
#' @param data dataframe
boxplotServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    output$plot <- shiny::renderPlot({
      if(!is.null(data())){
        box_plot(data = data(), numVar = input$numVar, catVar = input$catVar)
      }
    },res = 96)
  })
}
