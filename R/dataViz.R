#' Ui where the data are overviewed by dataVizOutput function.
#'
#' @param id module identifier
dataVizUi <- function(id){
  shiny::tagList(
    shiny::h3("Data overview"),
    DT::dataTableOutput(shiny::NS(id,"data")),
    shiny::verbatimTextOutput(shiny::NS(id,"model"))
    )
}

#' Show an overview of data in dataVizUi.
#'
#' @param id module identifier
#' @param data data to overview for example output of the module created by fileUi and fileServer.
dataVizOutput <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {

    output$data <- DT::renderDataTable({
      if(!is.null(data())){
        if(inherits(data(),"data.frame") &&
           apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
          data()[1:3]
        }else if(inherits(data(),"data.frame")){
          data()
        }
      }
    })

    output$model <- shiny::renderPrint({
      if(!is.null(data())){
        if(inherits(data(),"data.frame") &&
           apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
          raw_index <- apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
          model <- unserialize(data()[[raw_index]][[1]])
          caret::print.train(model)
        }else if(inherits(data(), "train.formula")){
          data()
        }
      }
    })

  })
}
