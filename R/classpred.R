#' Predict a sample of unseen data given fitted classification model
#' @param model a given classification model
#' @param sample a sample to predict wit respect to the given classificatin model
classPrediction <- function(model, sample){

  if(inherits(model,"data.frame") && apply(X = model,MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
    raw_index <- apply(X = model,MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
    model <- unserialize(model[[raw_index]][[1]])
  }

  if(inherits(model, "train.formula") &&
     (inherits(sample,"data.frame") && apply(X = sample,MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all())){
    na_index <- which(sample %>% dplyr::select(model$coefnames) %>% apply(MARGIN = 1,FUN = anyNA))
    varClass <- dimnames(attr(x = model$terms,which = "factors"))[[1]][1]
    if(!purrr::is_empty(na_index)){
      na_sample <- sample %>% dplyr::slice(na_index)
      sample <- sample %>% dplyr::slice(-na_index)
    }
    prob <- caret::predict.train(object = model, newdata = sample,type = "prob")
    if(varClass %in% colnames(sample)){
      sample[[paste0("predicted_",varClass)]] <- caret::predict.train(object = model, newdata = sample)
    }else{
      sample[[varClass]] <- caret::predict.train(object = model, newdata = sample)
    }

    if(purrr::is_empty(na_index)){
      return(sample)
    }else{
      return(list(sample=sample,na_sample=na_sample))
    }

  }
}


#' Ui container for predictions
#' @param id module identifier
classPredUi <- function(id){
  shiny::tagList(
    shiny::h3("Predicted class"),
    DT::dataTableOutput(shiny::NS(id,"dataPred")),
    downloadUi("source"),
    shiny::h3("Not Predicted"),
    DT::dataTableOutput(shiny::NS(id,"na_sample"))
  )
}

#' function server writing predictions into the classPredUi Ui
#' @param id module identifier
#' @param pred dataframe with predicted sample or list with predicted sample and unpredicted sample
classPredOutput <- function(id, pred){
  shiny::moduleServer(id, function(input,output,session){

    output$dataPred <- DT::renderDataTable({
      if("data.frame" %in% class(pred())){
        pred()
      }else if("list" %in% class(pred())){
        pred()$sample
      }
    })
    output$na_sample <- DT::renderDataTable({
      if("list" %in% class(pred())){
        pred()$na_sample
      }
    })

  })
}

#' compute the prediction of classes for the given model and sample in input
#' @param id  module identifier
#' @param data1 sample or model loaded in input
#' @param data2 sample or model loaded in input
classPredServer <- function(id, data1, data2){
  shiny::moduleServer(id,function(input, output, session){

    shiny::reactive({
      shiny::req(data1(), data2())
      if(inherits(data1(),"train.formula")){# data1 is a saved .RDS model
        classPrediction(data1(), data2())
      }else if(inherits(data1(),"data.frame") &&
               apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){# data is saved model in a sqlite database datatable
        raw_index <- apply(X = data1(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        model <- shiny::reactive(unserialize(data1()[[raw_index]][[1]]))
        classPrediction(model(), data2())
      }else{# otherwise data1 is a dataframe to test/predict
        if(inherits(data2(),"data.frame") && apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
          raw_index <- apply(X = data2(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
          model <- shiny::reactive(unserialize(data2()[[raw_index]][[1]]))
          classPrediction(model(), data1())
        }else if(inherits(data2(),"train.formula")){
          classPrediction(data2(), data1())
        }
      }
    })

  })
}

