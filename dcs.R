options(shiny.maxRequestSize = 800 * 1024^2,shiny.reactlog = TRUE)
pkgload::load_all(".")

ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader( # dashboardHeader ####
                                            title = "Data Collection System", titleWidth = 250,
                                            disable = FALSE ,
                                            notificationUi("source")),
sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 250),
body=shinydashboard::dashboardBody(shiny::uiOutput("IO")),
)

server <- function(input, output, session){

  data <- fileServer("source")

  output$IO <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
      sqliteDSNUi("source"),
      writeToDBUi("source")
    )
  })

  dataVizOutput(id = "source", data)
  sqliteDSNServer("source")
  writeToSQLiteDSN("source", data)

}

shinyApp(ui, server)
