#' Ui for classification model test on unseen data
#' @param id module object identifier
#' @return a shiny tagList object with different Ui for output visualization
classPredTestUi <- function(id){
  shiny::tagList(
    shiny::h3("Misclassified records"),
    DT::dataTableOutput(shiny::NS(id,"misdata")),
    shiny::h3("TEST METRICS"),
    shiny::verbatimTextOutput(shiny::NS(id,"confMatrix"))
    )
}

#' Write the chosen classification model Test output into classPredTestServer Ui
#' @param id module object identifier
#' @param data1 data read from application
#' @param data2 data read from application
#' @param pred  predicted data from classPredServer
#' @importFrom rlang .data
classPredTestServer <- function(id, data1, data2, pred){
  shiny::moduleServer(id, function(input, output, session){

    # look for the classification variable in the reference model
    varClass <- shiny::reactive({
      if(inherits(data1(),"train.formula")){
        dimnames(attr(x = data1()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data2(),"train.formula")){
        dimnames(attr(x = data2()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data2(),"data.frame") && apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data2()[[raw_index]][[1]]))
        dimnames(attr(x = raw_model()$terms,which = "factors"))[[1]][1]
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        raw_model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        dimnames(attr(x = raw_model()$terms,which = "factors"))[[1]][1]
      }

    })


    output$confMatrix <- shiny::renderPrint({
      if(! (is.null(pred()) || is.null(varClass())) ){
        if("data.frame" %in% class(pred())){
          if(!is.null(pred()[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred())){
              caret::confusionMatrix(pred()[[paste0("predicted_",varClass())]],
                                     factor(pred()[[varClass()]]),
                                     mode="everything")
            }
          }
        }else if("list" %in% class(pred())){
          if(!is.null(pred()$sample[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred()$sample)){
              caret::confusionMatrix(pred()$sample[[paste0("predicted_",varClass())]],
                                     factor(pred()$sample[[varClass()]]),
                                     mode="everything")
            }
          }
        }
      }
    })

    output$misdata <- DT::renderDataTable({
      if(! (is.null(pred()) || is.null(varClass())) ){
        if("data.frame" %in% class(pred())){
          if(!is.null(pred()[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred())){
              pred() %>% dplyr::filter(pred()[[varClass()]]!=pred()[[paste0("predicted_",varClass())]])
            }
          }
        }else if("list" %in% class(pred())){
          if(!is.null(pred()$sample[[paste0("predicted_",varClass())]])){
            if(varClass() %in% colnames(pred()$sample)){
              pred()$sample %>% dplyr::filter(pred()$sample[[varClass()]]!=pred()$sample[[paste0("predicted_",varClass())]])
            }
          }
        }
      }
    })

  })
}

