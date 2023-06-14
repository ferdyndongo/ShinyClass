options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- shinydashboard::dashboardPage(title = "CLASSIFICATION MODEL TRAINING",
                    header=shinydashboard::dashboardHeader(title = "CLASS TRAINING",
                                           disable = FALSE,
                                           notificationUi("source")
                    ),
                    sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target")),
                    body=shinydashboard::dashboardBody(dataVizUi("overview"), shiny::uiOutput("models"), shiny::uiOutput("train")
                    )
)

server <- function(input, output, session){

  data <- fileServer(id = "source")
  shiny::observeEvent(data(),{
    dataVizOutput("overview",data)
  })

  output$target <- shiny::renderUI({
    shiny::req(data())
    catVarUi("source")
  })
  shiny::observeEvent(data(),input$catVar,{
    shiny::freezeReactiveValue(input,"source-catVar")
    catVarServer("source",data)
  })

  output$models <- shiny::renderUI({
    shiny::req(data(), input$`source-catVar`)
    classModelUi("source")
  })

  fitted_model <- classModelServer("source", data)


  output$train <- shiny::renderUI({
    shiny::req(fitted_model())
    shiny::tagList(
      trainingUi("source"),
      reportUi("source"),
      downloadUi("source")
    )
  })
  trainingOutput("source",fitted_model)
  reportServer(id = "source",report_script = "classReport")
  filedownServer("source",fitted_model)
}

shinyApp(ui, server)
