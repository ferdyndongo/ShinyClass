options(shiny.maxRequestSize = 800 * 1024^2,width = 160,max.print = 99999)
pkgload::load_all(".")

ui <- shinydashboard::dashboardPage(header=shinydashboard::dashboardHeader(title = "DATA PRE-PROCESSING",
                                                                           titleWidth = 280,
                                                                           disable = FALSE,
                                                                           notificationUi("source")),
sidebar=shinydashboard::dashboardSidebar(fileUi(id = "source"),width = 280),
body=shinydashboard::dashboardBody(shiny::uiOutput("dataViz"))
)

server <- function(input, output, session){

  rawdata <- fileServer(id = "source")
  data <- shiny::reactive({
    if(!is.null(rawdata())){
      if(inherits(rawdata(),"data.frame") && apply(X = rawdata(),MARGIN = 2,function(col){!is.raw(col[[1]])}) %>% all()){
        rawdata() %>% data_preprocessing()
      }
    }
  })

  output$dataViz <- shiny::renderUI({
    shiny::req(data())
    shiny::tagList(
      dataVizUi("source"),
      catVarUi("source"),
      edaUi("source"),
      sqliteDSNUi("source"),
      writeToDBUi("source")
    )
  })
  dataVizOutput("source",data)
  catVarServer("source",data)
  edaOutput("source",data)
  sqliteDSNServer("source")
  writeToSQLiteDSN("source", data)
}
shinyApp(ui, server)
