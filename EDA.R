options(shiny.maxRequestSize = 200 * 1024^2,width = 160)
pkgload::load_all(".")


ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader(title = "DATA EXPLORATION",
                                                                           disable = FALSE,
                                                                           notificationUi("source")
                                                                           ),
sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),shiny::uiOutput("target")),
body=shinydashboard::dashboardBody(shiny::uiOutput("rawOutput"))
)

server <- function(input, output, session){

  data <- fileServer(id = "source")

  output$rawOutput <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("raw"),
      catVarUi("raw"),
      edaUi("raw"),
      reportUi("raw")
    )
  })
  dataVizOutput("raw",data)
  edaOutput("raw",data)
  catVarServer("raw",data)
  reportServer("raw", "EDA")

}
shinyApp(ui, server)
