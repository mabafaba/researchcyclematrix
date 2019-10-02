

#' change a value on google drive based on the file.id
#' @param file.id as a string
#' @param column best to just put the google drive colum letters (like "A" for the first column, or "AF")
#' @param value the value to put in the column, probably safest to give a character string
rcm_change_value<-function(file.id,column,value){
  # check if inputs are ok
  if(is.numeric(column)){
    if(column>26){stop('only columns <26 can be addressed with numeric index')}
    column<-LETTERS[column]
  }

  # find out which row in google drive matches the file id (this downloads the researchcyclematrix again to be sure
  # if you ever wondered why this is so slow, here it is - we download the whole matrix every time before we change a value
  # there's still the moment between starting the download and changing the value in which the rows could change and we fill the wrong value
  # there might be a much better workaround if we look into the gdrive api; maybe it lets us change a value based on a VLookup or something
  # good enough for now though i suppose
  row<-get_gdrive_row(file.id)

  # now we know the column and the row and the value, now we just need to actually call the api:
  g_sheets_put(row,column,value,spreadsheetId = "1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk")
}


#' generally we're using googlesheets package for authentification, but then httr package to call the google drive directly.
#' the workhorse is g_sheets_put which is used in most other functions that edit the remote RCM.
#'



#' wrapper for google sheets api to change single value
#' @param row the number of the row we want to fill
#' @param col the column to fill as a letter character, excel style: ( "A", "B", .... , "Z", "BA", ..., "ZZ")
#' @param value the value to put into the cell
#' @param spreadsheetId we could use this for other spreadsheets, but it's not working at the moment. Throwing an error if this param is changed
g_sheets_put<-function(row,col,value,spreadsheetId="1Quu2P6z-uA2H64eQENWJkNIOGIfnsdXgKNg4qdiCvXc",...){
  if(spreadsheetId!="1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk"){stop("g_sheets_put implemented only for one specific spreadsheet. for other spreadsheets would overwrite column AR")}
  cell<-paste0(col,row)
  thisurl<-paste0("https://sheets.googleapis.com/v4/spreadsheets/",spreadsheetId,"/values/",cell,"?valueInputOption=USER_ENTERED")

  # this puts an index into the rcm (this is the reason we can't use this function for a different google sheet)
  g_sheets_update_index()
  # httr::PUT: simple put request to googlesheets api. Take the api url, and add the value as custom made json
  # this json stuff is a bit tricky because of escaped characters etc (what if your value is closing quotes and a curly bracket..?.. );
  # had issues with this in the past; please be careful when editing and test carefully with json native symbolds in value

  # stores the server_response so we can throw an error if it didn't go through
  server_response <- httr::PUT(thisurl,googlesheets:::google_token(),valueInputOption="RAW",
                               body=paste0('{
                                           "values":[["',value,'"]]
}')
  )
  # read the response from google drive and throw an error if anything bad happened
  if(server_response$status_code==200){cat(crayon::silver(paste("successfully changed values in google sheet:",col," / ",row)))}
  if(server_response$status_code!=200){stop((paste0("\nfailed to modify google sheet\n",paste0(col,row),"\n","value:\n",value,"\n\n","status code: ",server_response$status_code)))}

  }

#' wrapper for google sheets api to append a new row
#' @param value a character vector of values to put in the row
#' @param spreadsheetId
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
