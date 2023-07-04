postgresUi <- function(id){
  shiny::tagList(
    shiny::selectInput(shiny::NS(id,"server"),
                       "Select a Server",
                       choices = c("","localhost","172.17.0.1"),
                       selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"dsn"), "Select a Data Source Name",
                       choices = c("",odbc::odbcListDataSources()$name),selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"schema"), "Select a Schema", choices = NULL,selected = NULL,selectize = FALSE),
    shiny::selectInput(shiny::NS(id,"dbtable"), "Select a Table", choices = NULL, selected = NULL,selectize = FALSE)
  )
}

postgresServer <- function(id){
  shiny::moduleServer(id,function(input,output,session){

    available_DSN <- shiny::reactive({
      shiny::req(input$dsn, input$server)
      DBI::dbCanConnect(drv = odbc::odbc(), dsn=input$dsn, server=input$server, uid="postgres", timeout=10)
    })

    output$warning <- shinydashboard::renderMenu({
      if(!available_DSN()){
        shiny::showNotification(attributes(available_DSN())$reason,duration = NULL,closeButton = TRUE,type = "error")
        shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger", icon = shiny::icon("database"),
                                     shinydashboard::notificationItem(text = stringr::str_c(input$dsn, "not available on", input$server,sep = " "),
                                                                      icon = shiny::icon("database"), status = "danger")
        )
      }else{
        tryCatch({
          odbc::dbGetQuery(
            conn = DBI::dbConnect(drv = odbc::odbc(), dsn=input$dsn, server=input$server, uid="postgres", timeout=10),
            statement = "SELECT schema_name FROM information_schema.schemata")
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "success", icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c("connected to", input$dsn,sep = " "),
                                                                        icon=shiny::icon("plug-circle-check"), status = "success"
                                       )
          )

        },warning=function(w){
          shiny::showNotification(w$message, duration = NULL, closeButton = TRUE, type = "warning")
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c("NO SCHEMA FOUND IN", class(dbcon()), "connection", input$dsn, sep = " "),
                                                                        icon=shiny::icon("plug-circle-check"), status = "warning"
                                       )
          )
        },error=function(e){
          shiny::showNotification(e$message, duration = NULL, closeButton = TRUE, type = "error")
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("database"),
                                       shinydashboard::notificationItem(text = stringr::str_c("NO SCHEMA FOUND IN", class(dbcon()), "connection", input$dsn, sep = " "),
                                                                        icon=shiny::icon("plug-circle-check"), status = "danger"
                                                                        )
                                       )
        })
      }
    })

    dbcon <- shiny::reactive({
      shiny::req(input$dsn, input$server)
      if(available_DSN()){
        DBI::dbConnect(drv = odbc::odbc(), dsn=input$dsn, server=input$server, uid="postgres", timeout=10)
      }
    })

    schema_list <- shiny::reactive({
      shiny::req(dbcon())
      if(inherits(dbcon(),"PostgreSQL")){
        odbc::dbGetQuery(conn = dbcon(),statement = "SELECT schema_name FROM information_schema.schemata")
      }
    })

    shiny::observeEvent(schema_list(),{
      shiny::freezeReactiveValue(input,"schema")
      shiny::updateSelectInput(inputId = "schema", choices = c("",schema_list()),selected = NULL)
    })

    table_list <- shiny::reactive({
      shiny::req(dbcon(),input$schema)
      DBI::dbListTables(conn = dbcon(),schema_name=input$schema)
    })

    shiny::observeEvent(table_list(),{
      shiny::freezeReactiveValue(input,"dbtable")
      shiny::updateSelectInput(inputId = "dbtable", choices = c("",table_list()),selected = NULL)
    })

    shiny::reactive({
      shiny::req(input$schema,input$dbtable)
      tryCatch({
        dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
      },warning=function(w){
        shiny::showNotification(w$message,duration = NULL,closeButton = TRUE,type = "warning")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications",
                                       shinydashboard::notificationItem(text = w$message,
                                                                        icon = shiny::icon("warning"),
                                                                        status = "warning"),
                                       # .list=lapply(X = w,FUN = notificationItem)
          )
        })
      },error=function(e){
        shiny::showNotification(e$message,duration = NULL,closeButton = TRUE,type = "error")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications",
                                       shinydashboard::notificationItem(text = e$message,
                                                                        icon = shiny::icon("warning"),
                                                                        status = "danger")
          )
        })
      })
      # dplyr::tbl(src =dbcon(), dbplyr::in_schema(schema=input$schema, table = input$dbtable)) %>% dplyr::collect()
    })
  })
}
