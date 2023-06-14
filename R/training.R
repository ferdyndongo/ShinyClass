#' Ui container for the fitted classifier model
#' @param id module object identifier
trainingUi <- function(id){
  shiny::tagList(
    shiny::verbatimTextOutput(shiny::NS(id,"finalModel")),
    shiny::verbatimTextOutput(shiny::NS(id,"object"))
  )
}

#' Server function writing the fitted classifier model into the rfTrainingUi
#' @param id module identifier
#' @param object a fitted model object given by knnServer, rfServer etc....
trainingOutput <- function(id, object){
  shiny::moduleServer(id, function(input, output, session){

    output$finalModel <- shiny::renderPrint({
      if(!is.null(object()) & inherits(object(),"train.formula")){
        object()$finalModel
      }
    })

    output$object <- shiny::renderPrint({
      if(!is.null(object()) & inherits(object(),"train.formula")){
        caret::print.train(object())
      }
    })
  })
}
