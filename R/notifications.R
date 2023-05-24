#' Ui for error or warning notifications
#'
#' @param id module identifier
notificationUi <- function(id){
  shinydashboard::dropdownMenuOutput(shiny::NS(id,"warning"))
}

#' Server function throwing error or warning to notificationUi.
#'
#' @param id module identifier
#' @param expr expression to be evaluated
notificationServer <- function(id,expr){
  shiny::moduleServer(id,function(input,output,session){

    tryCatch({
      expr
    },warning=function(w){
      output$warning <- shinydashboard::renderMenu({
        shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = w,FUN = notificationItem))
      })
    },error=function(e){
      output$warning <- shinydashboard::renderMenu({
        shinydashboard::dropdownMenu(type="notifications", .list=lapply(X = e,FUN = notificationItem))
      })
    })

  })
}
