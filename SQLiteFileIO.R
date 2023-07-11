options(shiny.maxRequestSize = 800 * 1024^2)

ui <- shinydashboard::dashboardPage(
  header=shinydashboard::dashboardHeader(title = "Data Collection System",
                                         titleWidth = 250,
                                         disable = FALSE,
                                         notificationUi("source")
  ),
  sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 250),
  body=shinydashboard::dashboardBody(shiny::uiOutput("IO"))
)

server <- function(input, output, session){

  data <- fileServer("source")

  output$IO <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
      shiny::fluidRow(shinydashboard::box(downloadUi("source")),
                      shinydashboard::box(SQLiteFileUi("source"),
                                          writeToDBUi("source"))
      )
    )
  })
  dataVizOutput(id = "source", data)
  filedownServer("source",data)
  SQLiteFileServer("source")
  writeToSQLiteFile("source",data)
}

shinyApp(ui, server)
