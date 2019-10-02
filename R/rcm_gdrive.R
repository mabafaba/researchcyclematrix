



#' get link to google drive row for a file.id
#'
gdrive_hyperlink_row<-function(file.id){
  # if(length(file.id)>1){warning("can only link to 1 gdrive row at a time. Using first.")}
  rowid<-get_gdrive_row(file.id)
  href<-paste0("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/edit#gid=1202281367&range=",
               rowid)
  return(href)
}

#' which row on google drive matches a file.id?
#' @param file.id the file id name of the item
#' @param rcm you can provide a custom rcm to use for row identification.
#' Not recommended (except for debugging purposes)
#' this will return the wrong row if the this is not a raw rcm (from rcm_download(raw=T)) or if the RCM online changed since it was downloaded
#' @return (estimated) google drive row index of file.id
get_gdrive_row<-function(file.id,rcm=NULL){
  if(is.null(rcm)){
    rcm<-rcm_download(raw=T)
  }
  local_row<-match(file.id,rcm[[grep("File.ID_Name",names(rcm))]])
  index<-rcm$index[local_row[1]]
}


#' which row on google drive matches a file.id?
#' @param file.id the file id name of the item
#' @param rcm you can provide a custom rcm to use for row identification.
#' Not recommended (except for debugging purposes)
#' this will return the wrong row if the this is not a raw rcm (from rcm_download(raw=T)) or if the RCM online changed since it was downloaded
#' @return (estimated) google drive row index of file.id
get_gdrive_relatedFiles<-function(file.id,rcm=NULL){
  if(is.null(rcm)){
    rcm<-rcm_download(raw=T)
  }
  local_row<-match(file.id,rcm[[grep("File.ID_Name",names(rcm))]])
  relatedFiles<-rcm$Related.files[local_row[1]]
}

#' which row on google drive matches a file.id?
#' @param file.id the file id name of the item
#' @param rcm you can provide a custom rcm to use for row identification.
#' Not recommended (except for debugging purposes)
#' this will return the wrong row if the this is not a raw rcm (from rcm_download(raw=T)) or if the RCM online changed since it was downloaded
#' @return (estimated) google drive row index of file.id
get_gdrive_fileType<-function(file.id,rcm=NULL){
  if(is.null(rcm)){
    rcm<-rcm_download(raw=T)
  }
  local_row<-match(file.id,rcm[[grep("File.ID_Name",names(rcm))]])
  fileType<-rcm$File.type[local_row[1]]
}


#' get google drive row links matching items in a RCM
rcm_gdrive_links<-function(rcm){
  links<-to_shiny_link(gdrive_hyperlink_row(rcm$file.id),rcm$file.id)
}



#' set an item's status to "with HQ" on google drive
#' @param file.id the items file id name as a string
#' @param fp the name of the focal point at HQ handling the item (always overwrites)
#' @export
rcm_set_to_withHQ<-function (file.id, fp, date = NULL)
{
  message(paste0("setting to 'with HQ': ",file.id))
  rcm_change_value(file.id, column = "V", value = "with HQ (api_state)")
  if(is.null(date)){
    rcm_set_withHQ_date(file.id)}else{
      rcm_set_withHQ_date(file.id,date)
    }

  rcm_set_HQ_focal_point(file.id = file.id, name = fp)
}




#' set an item's status to "with Field" on google drive
#' @param file.id the items file id name as a string
#' @param comment (optional) add a comment
#' @param fp the name of the focal point at HQ handling the item (always overwrites)
#' @export
rcm_set_to_withField<-function (file.id ,comment = NULL, fp)
{
  message(paste0("setting to 'with Field': ",file.id))
  rcm_change_value(file.id, column = "V", value = "with Field (api_state)")
  rcm_set_withfield_date(file.id)
  rcm_set_HQ_focal_point(file.id = file.id, name = fp)

  if(!is.null(comment)){
    rcm_comment(file.id,comment)
  }
}






#' set an item's status to "validated" on google drive
#' @param file.id the items file id name as a string
#' @param hours_worked time spent on the validation in hours; must be numeric or NA
#' @param comment (optional) add a comment
#' @param DDR.received logical; did we receive the data deletion report? defaults to FALSE
#' @param fp the name of the focal point at HQ handling the item (always overwrites)
#' @export
rcm_set_to_validated<-function(file.id,hours_worked,comment=NULL, DDR.received = FALSE,fp){
  if(!(is.numeric(hours_worked)|is.na(hours_worked))){stop("hours worked must be a number or NA")}
  if(DDR.received != TRUE & DDR.received != FALSE ){stop("invalid value for DDR.received. Should be TRUE or FALSE.")  }
  message(paste0("setting to 'validated': ",file.id))
  rcm_change_value(file.id,column = "V",value = "validated (api_state)")

  #Data Deletion Report only for raws concerning dataset
  file.type <- get_gdrive_fileType(file.id)
  if( grepl("DATA", toupper(file.type)) == TRUE){
    if(DDR.received == TRUE){
      rcm_change_value(file.id, column = "AM", value = "TRUE") # I is the column for related files
    }
    else{
      rcm_change_value(file.id, column = "AM", value = "FALSE") # I is the column for related files
    }
  }


  rcm_set_validation_date(file.id)
  rcm_set_hours_worked(file.id,hours_worked)
  rcm_set_HQ_focal_point(file.id = file.id, name = fp)

  rcm_comment(file.id,comment,overwrite = T)

}


#' looking at this function now, it's something really insane. Thank god it's not in use anywhere. Keeping it here to prank the digital archeologists of the future
#'
#'
#'
#'  #' add a value on google drive based on the file.id
#' rcm_add_value<-function(file.id,column,value){
#'   if(is.numeric(column)){
#'     if(column>26){stop('only columns <26 can be addressed with numeric index')}
#'     column<-LETTERS[column]
#'   }
#'   row<-get_gdrive_row(file.id)
#'   value_in_cell <- get_gdrive_relatedFiles(file.id)
#'   # print(file.id)
#'   # print(row)
#'   # print(value_in_cell)
#'   new_value <- paste(value_in_cell, value, sep=" / ")
#'   g_sheets_put(row,column,new_value,spreadsheetId = "1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk")
#' }








#' change an items comment on google drive
#' @param file.id the items file id name as a string
#' @param comment the comment as a single character string
#' @param overwrite if TRUE, will completely overwrite the current comment. Otherwise it will append the new comment to the existing one (with a date stamp)
#' @export
rcm_comment<-function(file.id,comment,overwrite=F){
assertthat::assert_that(assertthat::is.string(comment))
  message(paste0("commenting on: ",file.id))
  if(!overwrite){
  rcm<-researchcyclematrix::rcm_download(raw=T)
  current_comment<-rcm$Comments[match(file.id,rcm$File.ID_Name)]
  }else{
    current_comment<-""
  }
  if(current_comment %in% c(""," ",NA)){current_comment<-NULL}else{
    current_comment <- paste0(current_comment,"\n")
  }
  comment<-paste0(current_comment,format(Sys.Date(),"%d-%b-%y"),": ",comment)
  researchcyclematrix:::rcm_change_value(file.id,column = "J",value = comment)
}









#' set the name of the person handling the item at HQ in the gdrive RCM
#' @param file.id the file id of the item to change
#' @param name the name of the person as a character string.
#' @return returns nothing important
rcm_set_HQ_focal_point<-function(file.id,name=NULL){

  if(is.null(file.id) | is.null(name)){stop("inputs can't be NULL")}

  name_text <- stringr::str_to_title(name)
  if(length(name_text)!=1){stop("name must be a single character value")}

  rcm_change_value(file.id,column = "AI",value = name_text)


}































































































