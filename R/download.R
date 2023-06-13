#' Ui for the downloadButton in the download module
#'
#' @param id module identifier
downloadUi <- function(id){
  shiny::downloadButton(shiny::NS(id, "download"))
}

#' Function server which downloads R objects load from datafile module (i.e fileUi & fileServer) when the downloadUi is clicked
#' @param id module identifier
#' @param data the R object to be saved in a binary file when the button is clicked
filedownServer <- function(id, data){
  shiny::moduleServer(id, function(input, output, session){

    Robject <- shiny::reactive({
      shiny::req(data())
      if(inherits(data(), "train.formula")){
        data()
      }else if(inherits(data(),"data.frame") && dim(data())[1]>0 &&
               apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% any()){
        raw_index <- apply(X = data(),MARGIN = 2,function(col){is.raw(col[[1]])}) %>% which()
        unserialize(data()[[raw_index]][[1]])
      }else{
        data()
      }
    })

    output$download <- shiny::downloadHandler(
      filename = function(){
        if(inherits(Robject(),"train.formula")){
          if(!is.null(input$caretModel)){
            if(!is.null(input$sheet)){
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$sheet,sep = "_"), ".RDS")
            }else{
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$file_load$name,sep = "_"), ".RDS")
            }
          }else{
            if(!is.null(input$sheet)){
              paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".RDS")
            }else{
              paste0(paste(input$file_load$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".RDS")
            }
          }
        }else if(inherits(Robject(),"data.frame")){
          if(!is.null(input$caretModel)){
            if(!is.null(input$sheet)){
              shiny::req(input$caretModel,input$sheet)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$sheet,sep = "_"), ".xls")
            }else{
              shiny::req(input$caretModel,input$file_load$name)
              paste0(paste(input$caretModel,stringr::str_replace_all(Sys.Date(),"-","_"),input$file_load$name,sep = "_"), ".xls")
            }
          }else{
            if(!is.null(input$sheet)){
              paste0(paste(input$sheet,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
            }else{
              paste0(paste(input$file_load$name,stringr::str_replace_all(Sys.Date(),"-","_"),sep = "_"), ".xls")
            }
          }
        }
      },
      content = function(file){
        if(inherits(Robject(),"train.formula")){
          id <- shiny::showNotification("Saving model ...", duration = NULL, closeButton = FALSE)
          on.exit(shiny::removeNotification(id), add = TRUE)
          base::saveRDS(Robject(),file)
        }else if(inherits(Robject(),"data.frame")){
          id <- shiny::showNotification("Downloading database table ...", duration = NULL, closeButton = FALSE)
          base::on.exit(shiny::removeNotification(id), add = TRUE)
          if(!is.null(input$sheet)){
            shiny::req(input$sheet)
            WriteXLS::WriteXLS(Robject(),file,input$sheet)
          }else{
            WriteXLS::WriteXLS(Robject(),file,input$file_load$name)
          }
        }
      }
    )
  })
}
