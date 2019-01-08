
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
rcm_download<-function(include_archived=F,include_validated=F,after_year="2015",main_columns_only=T,fill.dates=T,remove_empty=T,gdrive_links=T,raw=F){
  print("downloading rcm...")
  rcm<-read.csv("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/gviz/tq?tqx=out:csv&sheet=RC_Matrix_2.1",
                stringsAsFactors = F)

  # json_raw<-readLines("https://docs.google.com/a/google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/gviz/tq?tq=select%20H")[2]
  # json_raw<-gsub("google.visualization.Query.setResponse(","",json_raw,fixed = T)
  # json_raw<-gsub("\\);$","",json_raw,fixed = F)
  # file.id.remote<-rjson::fromJSON(json_raw)
  # file.id.remote$table$rows %>% unlist %>% unname %>% length
  # json_raw %>% cat
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

  after_year<-as.Date(paste0(after_year,"-12-31"),format="%Y-%m-%d")
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
                 "status","archived","unit","comment")
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
  href<-paste0("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/edit#gid=1191173775&range=",
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


#' get google drive row links matching items in a RCM
rcm_gdrive_links<-function(rcm){
  links<-to_shiny_link(gdrive_hyperlink_row(rcm$file.id),rcm$file.id)
}



#' set an item's status to "validated" on google drive
#' @param file.id the items file id name as a string
#' @export
rcm_set_to_validated<-function(file.id){
  print(file.id)
  rcm_change_value(file.id,column = "V",value = "validated (api_state)")
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



#' wrapper for google sheets api
g_sheets_put<-function(row,col,value,spreadsheetId="1Quu2P6z-uA2H64eQENWJkNIOGIfnsdXgKNg4qdiCvXc",...){
  cell<-paste0(col,row)
  thisurl<-paste0("https://sheets.googleapis.com/v4/spreadsheets/",spreadsheetId,"/values/",cell,"?valueInputOption=USER_ENTERED")
  g_sheets_update_index()
  httr::PUT(thisurl,googlesheets:::google_token(),valueInputOption="RAW",
            body=paste0('{
                        "values":[["',value,'"]]
}')
  )%>% print
}



#' fills a column on google drive with incremental indices to ensure row matching works as expected
g_sheets_update_index<-function(col="AR",spreadsheetId="1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk",...){
  rows<-c(1,1000)

  for(i in 1:4){
    if(rows[2]>3990){rows[2]<-3990}
    if(rows[1]>3001){break}
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
