
#' download the research cycle matrix
#'
#' @param include_archived logical: should archived entries be included?
#' @param include_validated logical: should validated entries be included?
#' @param after_year excludes all entries from before the given year (defaults to 2015)
#' @param main_columns_only logical include only most important columns?
#' @param fill.dates logical: should dates be filled from other dates? e.g. "first planned date" copyed to "latest planned date" where that is missing
#' @param remove_empty logical: should empty rows be removed (default TRUE)
#' @param gdrive_links logical: should a column with links to the google drive row be added?
#' @param raw logical: if TRUE ignores all other parameters and returns the raw download (default FALSE)
#' @return a data frame with the research cycle matrix
#' @export
rcm_download <- function(include_archived=F,include_validated=F,after_year="2015",main_columns_only=T,fill.dates=T,remove_empty=T,gdrive_links=F,raw=F){
  print("downloading rcm...")
  rcm<-read.csv("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/gviz/tq?tqx=out:csv&sheet=RC_Matrix_2.1",
                stringsAsFactors = F)

  x<-rcm[1,]
  if(remove_empty){rcm<-rcm[apply(rcm,1,function(x){
    x<-x[-which(names(x)=="index")];!all(is.na(x)|x=="")}),]
  }
  if(raw){
    message("RCM requested in raw format. ignoring all other parameters.")
    return(rcm)}
  dateformat<-"%d-%b-%y"
  datecols<-grep("[^[:alnum:] ]Date|[^[:alnum:] ]date|File.submission.to.HQ",names(rcm),value=T)

  for(i in datecols){
    rcm[,i] <- as.Date(as.character(rcm[,i]),format=dateformat)
  }

  after_year<-as.Date(paste0(after_year,"2014-12-31"),format="%Y-%m-%d")
  too_early_entries<-apply(rcm[,datecols],2,function(x){x<after_year}) %>% apply(1,any,na.rm =T)
  rcm<-rcm[!too_early_entries,,drop=F]


  rcm <- rcm_standardised_columns(rcm)
  if(fill.dates){
    rcm <- rcm_fill_dates(rcm)
  }

  if(gdrive_links){rcm$link<-rcm_gdrive_links(rcm)}
  if(main_columns_only){
    main_cols<-c("rcid","round","file.id","type",
                 "date.endcollection.planned","date.endcollection.actual",
                 "date.hqsubmission.planned.first","date.hqsubmission.planned.latest", "date.hqsubmission.actual",
                 "date.feedback","date.validated","date.milestone",
                 "status","archived","unit","comment","rc.title","project.code","hq.fp")
    if(gdrive_links){main_cols<-c(main_cols,"link")}
    rcm<-rcm[,main_cols]
  }

  if(!include_archived){
    rcm<-rcm[!rcm$archived,]
  }
  if(!include_validated){
    rcm<-rcm[!grepl("validated",rcm$status),]
  }


  rcm

}


#' get link to google drive row for file.id
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

#' add a value on google drive based on the file.id
rcm_add_value<-function(file.id,column,value){
  if(is.numeric(column)){
    if(column>26){stop('only columns <26 can be addressed with numeric index')}
    column<-LETTERS[column]
  }
  row<-get_gdrive_row(file.id)
  value_in_cell <- get_gdrive_relatedFiles(file.id)
  print(file.id)
  print(row)
  print(value_in_cell)
  new_value <- paste(value_in_cell, value, sep=" / ")
  g_sheets_put(row,column,new_value,spreadsheetId = "1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk")
}

#' change a value on google drive based on the file.id
rcm_change_value<-function(file.id,column,value){
  if(is.numeric(column)){
    if(column>26){stop('only columns <26 can be addressed with numeric index')}
    column<-LETTERS[column]
  }
  row<-get_gdrive_row(file.id)
  print(file.id)
  print(row)
  g_sheets_put(row,column,value,spreadsheetId = "1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk")
}



#' wrapper for google sheets api to change single value
g_sheets_put<-function(row,col,value,spreadsheetId="1Quu2P6z-uA2H64eQENWJkNIOGIfnsdXgKNg4qdiCvXc",...){
  if(spreadsheetId!="1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk"){stop("g_sheets_put implemented only for one specific spreadsheet. for other spreadsheets would overwrite column AR")}
  cell<-paste0(col,row)
  thisurl<-paste0("https://sheets.googleapis.com/v4/spreadsheets/",spreadsheetId,"/values/",cell,"?valueInputOption=USER_ENTERED")
  g_sheets_update_index()
  server_response <- httr::PUT(thisurl,googlesheets:::google_token(),valueInputOption="RAW",
            body=paste0('{
                        "values":[["',value,'"]]
}')
  )
  if(server_response$status_code==200){cat(crayon::silver(paste("successfully changed values in google sheet:",col," / ",row)))}
  if(server_response$status_code!=200){stop((paste0("\nfailed to modify google sheet\n",paste0(col,row),"\n","value:\n",value,"\n\n","status code: ",server_response$status_code)))}

}

#' wrapper for google sheets api to append a new row
g_sheets_append_row<-function(value,spreadsheetId="1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8",...){
  thisurl<-paste0("https://sheets.googleapis.com/v4/spreadsheets/",spreadsheetId,"/values/","submissions!A1:E2:append","?valueInputOption=USER_ENTERED")
  # https://sheets.googleapis.com/v4/spreadsheets/spreadsheetId/values/Sheet1!A1:E1:append?valueInputOption=USER_ENTERED
  httr::POST(thisurl,googlesheets:::google_token(),valueInputOption="RAW",
            body=

# paste0('{
#                         "values":[["',value,'"]]
# }') %>% cat

jsonlite::toJSON(list(values=t(as.matrix(value))))

  )%>% print
  # POST https://sheets.googleapis.com/v4/spreadsheets/{spreadsheetId}/values/{range}:append
  # https://sheets.googleapis.com/v4/spreadsheets/spreadsheetId/values/Sheet1!A1:E1:append?valueInputOption=USER_ENTERED
  # https://docs.google.com/spreadsheets/d/1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8/edit#gid=0
}


#' fills a column on google drive with incremental indices to ensure row matching works as expected
g_sheets_update_index<-function(col="AR",spreadsheetId="1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk",...){
  rows<-c(1,1000)

  for(i in 1:7){
    if(rows[2]>6770){rows[2]<-6770}
    if(rows[1]>6001){break}
    range<-paste0(paste0(paste0(col,rows[1]),":",paste0(col,rows[2])))
    thisurl<-paste0("https://sheets.googleapis.com/v4/spreadsheets/",spreadsheetId,"/values/",range,"?valueInputOption=USER_ENTERED")

    value='=IF(ROW()=1,\\"index\\",ROW())'
    httr::PUT(thisurl,googlesheets:::google_token(),valueInputOption="RAW",
              body=paste0('{
                          "values":[',paste0(paste0('["',rep(value,rows[2]-rows[1]+1),'"]'),collapse=','),']
  }')
  )%>% print
    rows<-rows+1000

}

}





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































































































