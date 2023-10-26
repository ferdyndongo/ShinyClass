box_plot <- function(data, numVar=NULL, catVar=NULL){
  if(!(is.null(catVar) | is.null(numVar))){
    if(all(catVar=="") & all(numVar!="")){
      if(length(numVar)==1){
        graphics::boxplot(data[[numVar]],horizontal=TRUE,title=paste("~", numVar))
      }else if(length(numVar)>1){
        graphics::boxplot(data %>% dplyr::select(dplyr::all_of(numVar)) %>% scale(),horizontal = TRUE)
      }
    }else if(all(catVar!="") & all(numVar!="")){
      if(length(catVar)==1){
        graphics::par(mfrow=c(length(numVar),1))
        for(var in numVar){
          graphics::boxplot(formula(paste(var,"~",catVar)),data=data,horizontal=TRUE,las=1)
        }
      }
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
