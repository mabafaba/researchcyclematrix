




#' save the hours worked in the rcm
#' @param file.id the file id
#' @param hours the number of hours worked on the file id
#' @return the file id
rcm_set_hours_worked <- function (file.id,hours){
  if(!is.na(hours)){
  if(assertthat::is.number(hours))
    hours<-as.numeric(hours)
  }else{
    hours<-"NA"
  }

  rcm_change_value(file.id, column = "BB", value = hours)
  invisible(file.id)
}
