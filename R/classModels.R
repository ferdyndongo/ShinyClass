#' Fit and train classification models using R the caret package.
#' @param data a dataset with explanatory variables and response variable
#' @param catVar the response variable
#' @param model string specifiying which classification model will be used.
#' @return a trained and fitted classification model
#' @importFrom stats formula na.omit
fit_class <- function(data, catVar, model){
  if(!( is.null(data) | is.null(catVar) | is.null(model) )){
    if(catVar %in% colnames(data)){
      data <- data %>% dplyr::select(dplyr::all_of(c(numericDataset(data) %>% colnames(),catVar)))
      ctrl_fit <- caret::trainControl(method = "repeatedcv",number = 10,repeats = 5,p = 0.75)

      tryCatch({
        set.seed(1234)
        caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit)
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        set.seed(1234)
        caret::train(formula(paste(catVar,"~ .")), data, method=model, trControl=ctrl_fit, na.action=na.omit)
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        return(NULL)
      })

    }
  }
}


#' Ui selectInput for some available classification models in CARET package
#' @param id module identifier
classModelUi <- function(id){
  shiny::selectInput(shiny::NS(id,"caretModel"),label = "Choose a classification model",
                     choices = c("", "rf", "gbm", "xgbTree"),
                     # choices = c("", names(lapply(X = caret::getModelInfo(),FUN = function(x) any(x[["type"]]=="Classification")))),
                     selected = NULL,selectize = FALSE)
}

#' compute classification models contained in caret R package.
#' @param id module identifier
#' @param data training data used by models
classModelServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){
    shiny::reactive({
      if(!is.null(data())){
        shiny::req(input$catVar, input$caretModel)
        id <- shiny::showNotification("MODEL TRAINGING IN PROGRESS ...", duration = NULL, closeButton = FALSE)
        base::on.exit(shiny::removeNotification(id), add = TRUE)
        fitted_model <- fit_class(data = data(), catVar = input$catVar, model=input$caretModel)
        colnames(fitted_model$trainingData)[which(colnames(fitted_model$trainingData)==".outcome")] <- input$catVar
        fitted_model
      }
    })
  })
}
