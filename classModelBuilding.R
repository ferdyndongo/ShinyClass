options(shiny.maxRequestSize = 900 * 1024^2)
pkgload::load_all(".")
ui <- shinydashboard::dashboardPage(title = "CLASSIFICATION MODEL TRAINING",
                                    header=shinydashboard::dashboardHeader(title = "TRAINING/TEST",
                                                                           disable = FALSE,
                                                                           notificationUi("source")
                                    ),
                                    sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target")),
                                    body=shinydashboard::dashboardBody(dataVizUi("overview"),shiny::uiOutput("models"),
                                                                       shiny::uiOutput("TrainTest")
                                    )
)

server <- function(input, output, session){

  rawdata <- fileServer(id = "source")
  data <- shiny::reactive({
    if(!is.null(rawdata())){
      if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        rawdata()
      }else{
        shiny::showNotification(paste0("ERROR !!! :"," UPLOADED DATA ARE NOT DATASET!!!"),duration = NULL,closeButton = TRUE,type = "error")
        return(NULL)
      }
    }
  })
  # data <- fileServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("overview",data)
  })

  output$target <- shiny::renderUI({
    shiny::req(data())
    catVarUi("source")
  })
  shiny::observeEvent(data(),{
    shiny::freezeReactiveValue(input,"source-catVar")
    catVarServer("source",data)
  })

  output$models <- shiny::renderUI({
    shiny::req(data(), input$`source-catVar`)
    classModelUi("source")
  })

  model_available <- shiny::reactive({
    shiny::req(data(), input$`source-catVar`, input$`source-caretModel`)
    if(all(caret::getModelInfo()[[input$`source-caretModel`]]$library %in% installed.packages())){
      TRUE
    }else{
      not_installed_index <- which(!(caret::getModelInfo()[[input$`source-caretModel`]]$library %in% installed.packages()))
      not_installed_library <- caret::getModelInfo()[[input$`source-caretModel`]]$library[not_installed_index]
      msg <- paste0("Required packages are missing: ", paste0(not_installed_library,collapse = ", "))
      shiny::showNotification(msg,duration = NULL,closeButton = TRUE,type = "error")
      FALSE
    }
  })

  shiny::observeEvent(model_available(),{
    if(model_available()){
      result <- TrainTestModelServer("source", data)
      output$TrainTest <- shiny::renderUI({
        shiny::req(data(), input$`source-catVar`, input$`source-caretModel`,result)
        TrainTestModelUi("source")
      })
      TrainTestModelOutput("source", result)
      filedownServer("source",result$model)
    }
  })

  # result <- TrainTestModelServer("source", data)
  # shiny::observeEvent(result,{
  #   output$TrainTest <- shiny::renderUI({
  #     shiny::req(data(), input$`source-catVar`, input$`source-caretModel`,result)
  #     TrainTestModelUi("source")
  #   })
  #   TrainTestModelOutput("source", result)
  #   filedownServer("source",result$model)
  # })

}

shinyApp(ui, server)
