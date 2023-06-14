#' Ui for actionButton which writes into the database from an uploaded raw data file
#' @param id module identifier
writeToDBUi <- function(id){
  shiny::tagList(
    shiny::actionButton(shiny::NS(id, "submit"),label = "upload to DB")
  )
}

#' Function server in order to write into sqlite database when the writeToDBUi button is clicked.
#' Overwrite a table is not considered, we only implement updating and creating table.
#' @param id module identifier
#' @param dat data to be written into the database
writeToSQLiteDSN <- function(id, dat){
  shiny::moduleServer(id, function(input, output, server){

    data <- shiny::reactive({
      shiny::req(dat())
      if(inherits(dat(), "data.frame")){
        dat()
      }else if(inherits(dat(), "train.formula")){
        rawdatamodel <- serialize(object = dat(),connection = NULL)
        data.frame(type=dat()$modelType,method=dat()$method,
                   date=stringr::str_replace_all(paste(Sys.Date()),"-","_"),
                   rawdatamodel=I(list(rawdatamodel)))
      }
    })

    shiny::observeEvent(input$submit,{
      shiny::req(input$submit)
      if(input$dsn!=""){
        if(input$dbtable==""){
          shiny::req(input$dsn, input$file_load)
          ext <- tools::file_ext(input$file_load$name)
          tablename <- base::switch(ext,
                                    RDS = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                    csv = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                    txt = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                    tsv = stringr::str_remove_all(strsplit(x = input$file_load$name,split = ".",fixed = TRUE)[[1]][1],"[ ]"),
                                    xls = stringr::str_remove_all(input$sheet,"[ ]"),
                                    xlsx = stringr::str_remove_all(input$sheet,"[ ]"),
                                    mdb = stringr::str_remove_all(input$sheet,"[ ]"),
                                    db = stringr::str_remove_all(input$sheet,"[ ]"),
                                    s3db = stringr::str_remove_all(input$sheet,"[ ]")
          )
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          tryCatch({
            file_to_sqlite_dsn(input$dsn, data(), tablename, overwrite=FALSE, append=FALSE)
          },warning=function(w){
            shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                           .list=lapply(X = w,FUN = shinydashboard::notificationItem))
            })
          },error=function(e){
            shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", badgeStatus = "danger",icon = shiny::icon("upload"),
                                           .list=lapply(X = e,FUN = shinydashboard::notificationItem)
                                           # shinydashboard::notificationItem(text = e$message,
                                           #                                  icon = shiny::icon("upload"),
                                           #                                  status = "danger")
              )
            })
          })
        }else{
          shiny::req(input$dsn, input$dbtable)
          tablename <- stringr::str_remove_all(input$dbtable,"[ ]")
          id <- shiny::showNotification("uploading to db ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          tryCatch({
            file_to_sqlite_dsn(input$dsn,data(),tablename,overwrite=FALSE,append=TRUE)
          },warning=function(w){
            shiny::showNotification(w,duration = NULL,closeButton = TRUE,type = "warning")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                           .list=lapply(X = w,FUN = shinydashboard::notificationItem))
            })
          },error=function(e){
            shiny::showNotification(e,duration = NULL,closeButton = TRUE,type = "error")
            output$warning <- shinydashboard::renderMenu({
              shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                           shinydashboard::notificationItem(text = e$message,
                                                                            icon = shiny::icon("upload"),
                                                                            status = "danger")
              )
            })
          })

        }
      }else{
        shiny::showNotification("select the dsn where the data table will be written",duration = NULL,closeButton = TRUE,type = "error")
        output$warning <- shinydashboard::renderMenu({
          shinydashboard::dropdownMenu(type="notifications", badgeStatus = "warning",icon = shiny::icon("upload"),
                                       shinydashboard::notificationItem(text = "The DSN input can't be empty",
                                                                        icon = shiny::icon("upload"),
                                                                        status = "danger")
          )
        })
      }


    })
  })
}
