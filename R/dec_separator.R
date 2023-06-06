#' Check if the point is the decimal separator
#' @param name string character
is_point_decimal_separator <- function(name){stringr::str_detect(name,"\\.")}

#' Check if there is any comma used as decimal separator in a column vector
#' @param name string character
any_comma_decimal_separator <- function(name){any(stringr::str_detect(name,","),na.rm = TRUE)}

#' Replace the comma used as decimal separator with the point
#' @param data dataframe
replace_decimal_separator <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    if(!purrr::is_empty(cols)){
      for(col in cols){
        data[[col]] <- stringr::str_replace(data[[col]], ",",".")
      }
    }
  }
  return(data)
}

#' Detect columns with comma as decimal decimal separator
#' @param data dataframe
wrong_decimal_separator_data <- function(data){
  if(any(apply(data,2,any_comma_decimal_separator))){
    cols <- apply(data,2,any_comma_decimal_separator) %>% which()
    b <- base::data.frame()
    for(col in names(cols)){
      a <- data %>% dplyr::filter(stringr::str_detect(data[[col]],","))
      b <- b %>%  dplyr::bind_rows(a)
    }
    return(b)
  }
}
