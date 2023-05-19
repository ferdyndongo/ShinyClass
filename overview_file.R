options(shiny.maxRequestSize = 800 * 1024^2,shiny.reactlog = TRUE,shiny.maxRequestSize = 800 * 1024^2)
pkgload::load_all(".")

ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader(
  title = "Data Overview", titleWidth = 250,
  disable = FALSE),
  sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 250),
  body=shinydashboard::dashboardBody(shiny::uiOutput("IO")),
)

server <- function(input, output, session){

  data <- fileServer("source")

  output$IO <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
    )
  })
  dataVizOutput(id = "source", data)
}

shinyApp(ui, server)
