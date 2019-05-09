#' set the submission date in the gdrive RCM
#' @param file.id the file id of the item to change
#' @param date the date to set it to. Must be of class "POSIXct".
#' @return returns nothing important
rcm_set_submission_date<-function(file.id,date){
  # stop if input no good:
  if(!("POSIXct" %in% class(date))){
    stop("date must be in POSIXct format")
  }

  if(is.null(file.id) | is.null(date)){stop("inputs can't be NULL")}

  date_text <- format(date, format="%d-%b-%y")
  if(length(date_text)!=1){stop("date not converted to single date text")}

  rcm_change_value(file.id,column = "R",value = date_text)

}

#' check if a date string is in the format that matches the research cycle matrix standard for dates
is_date_format_in_rcm_style<-function(date_string){
  rcm_date_pattern<-"^[0-9]*-[A-z][A-z][A-z]-[0-9][0-9]$"
  return(all(grepl(rcm_date_pattern,date_string)))
}


