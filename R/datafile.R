#' Ui for file give to fileServer function.
#'
#' @param id module identifier
fileUi <- function(id){
  shiny::tagList(
    shiny::fileInput(shiny::NS(id,"file_load"),label = NULL,
    accept = c(".mdb", ".xls", ".xlsx", ".csv", ".tsv", ".txt", ".RDS")
                     ),
    shiny::uiOutput(shiny::NS(id,"file_tables")
                    )
    )
}

#' Server function for fileUi.
#'
#' @param id module identifier
fileServer <- function(id){
  shiny::moduleServer(id,function(input, output, session){

    table_list <- shiny::reactive({
      shiny::req(input$file_load$name, input$file_load$datapath)
      if(tools::file_ext(input$file_load$name) %in% c("xls","xlsx","mdb")){
        sheet_list(input$file_load$name, input$file_load$datapath)
      }
    })

    output$file_tables <- shiny::renderUI({
      shiny::req(table_list())
      shiny::selectInput(shiny::NS(id,"sheet"),label = "Select sheet",
                         choices = c("",table_list()),selected = NULL,selectize = FALSE)
    })
    shiny::reactive({
      if(!is.null(table_list())){
        shiny::req(input$file_load, input$sheet)
        if(input$sheet %in% table_list()){
          tryCatch({
            load_file(input$file_load$name, input$file_load$datapath, input$sheet)
          },warning=function(w){
            shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = shinydashboard::notificationItem))
            })
          },error=function(e){
            shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications",#warn$message
                                           shinydashboard::notificationItem(text = e$message,
                                                                            icon = shiny::icon("warning"),
                                                                            status = "danger")
              )
            })
          })
          # load_file(input$file_load$name, input$file_load$datapath, input$sheet)
        }
      }else{
        shiny::req(input$file_load$name, input$file_load$datapath)
        load_file(input$file_load$name, input$file_load$datapath)
      }

    })
  })
}
