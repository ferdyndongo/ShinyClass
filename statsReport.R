options(shiny.maxRequestSize = 400 * 1024^2, width = 160)

ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader( # dashboardHeader ####
                                                                            title = "STATS REPORT",#titleWidth = 250,
                                                                            disable = FALSE,
                                                                            ShinyClass:::notificationUi("source")
),
sidebar=shinydashboard::dashboardSidebar(ShinyClass:::fileUi(id = "source"), shiny::uiOutput("target")),
body=shinydashboard::dashboardBody(shiny::uiOutput("rawOutput"))
)

server <- function(input, output, session){

  data <- ShinyClass:::fileServer(id = "source")

  output$target <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      ShinyClass:::catVarUi("source"),
      ShinyClass:::catVarValueUi("source")
    )
  })
  output$rawOutput <- shiny::renderUI({
    shiny::req(data())
    ShinyClass:::statsReportUi("source")
  })
  ShinyClass:::statsReportOutput("source",data)
  ShinyClass:::catVarServer("source",data)
  ShinyClass:::catVarValueServer("source",data)

}
shinyApp(ui, server)
