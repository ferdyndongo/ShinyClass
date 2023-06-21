options(shiny.reactlog = TRUE)
options(shiny.maxRequestSize = 200 * 1024^2)
options(shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- shinydashboard::dashboardPage(title = "Model Test/Predict",
                    header=shinydashboard::dashboardHeader(title = "MODEL TEST/PREDICT",titleWidth = 350,
                                           disable = FALSE,
                                           notificationUi("source")
                    ),
                    sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 350),
                    body=shinydashboard::dashboardBody(shiny::uiOutput("source2"), shiny::uiOutput("classPred"))
)

server <- function(input, output, session){

  # import model or test sample and visualize it ####
  data1 <- fileServer(id = "source")
  output$source2 <- shiny::renderUI({
    shiny::req(data1())
    fileUi("source2")
  })
  data2 <- fileServer("source2")
  pred <- classPredServer("source",data1,data2)

  shiny::observeEvent(pred(),{
    output$classPred <- shiny::renderUI({
      shiny::req(pred())
      shiny::tagList(
        classPredUi("source"),
        classPredTestUi("source")
      )
    })
    if("data.frame" %in% class(pred())){
      filedownServer("source",pred)
    }else if("list" %in% class(pred())){
      filedownServer("source",shiny::reactive(pred()$sample))
    }
    classPredOutput("source",pred)
    classPredTestServer("source", data1, data2, pred)
  })

}

shinyApp(ui, server)
