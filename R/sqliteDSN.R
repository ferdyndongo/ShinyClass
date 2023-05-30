#' Ui reading DSN with available database connection
#'
#' @param id module identifier
sqliteDSNUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"dsn"), "Select a Data Source Name",
                       choices = c("",odbc::odbcListDataSources()$name),selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"dbtable"), "Select a Table", choices = NULL, selected = NULL,selectize = FALSE)
  )
}

#' Function server for sqliteDSNUi. Read and load sqlite database table list through DSN
#'
#' @param id module identifier
sqliteDSNServer <- function(id){
  shiny::moduleServer(id,function(input,output,session){

    available_DSN <- shiny::reactive({
      shiny::req(input$dsn)
      DBI::dbCanConnect(drv = odbc::odbc(), dsn=input$dsn, timeout=10)
    })

    dbcon <- shiny::reactive({
      shiny::req(input$dsn)
      if(available_DSN()){
        DBI::dbConnect(drv = odbc::odbc(), dsn=input$dsn, timeout=10)
      }
    })

    output$warning <- shinydashboard::renderMenu({
      if(!available_DSN()){
        shiny::showNotification(attributes(available_DSN())$reason,duration = NULL,closeButton = TRUE,type = "error")
        shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger", icon = shiny::icon("database"),
                                     shinydashboard::notificationItem(text = stringr::str_c(input$dsn, "not found on file .odbc.ini", sep = " "),
                                                                      icon = shiny::icon("database"), status = "danger")
        )
      }else{
        if(purrr::is_empty(table_list())){
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dsn,"is a empty database",sep = " "),
                                                                        icon=shiny::icon("table"), status = "warning")
          )
        }else{
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c(input$dbtable,"database table",sep = " "),
                                                                        icon=shiny::icon("table"), status = "success")
          )
        }
      }
    })

    table_list <- shiny::reactive({
      shiny::req(dbcon())
      DBI::dbListTables(conn = dbcon())
    })

    shiny::observeEvent(table_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",table_list()),selected = NULL)
      odbc::dbDisconnect(dbcon())
    })
  })

}
