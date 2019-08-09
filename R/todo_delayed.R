


#' create per country csv files listing all delayed items
#' @param todo see ?todo_download
#' @result subset of rcm with delayed items
#' @export
todo_delayed_to_csv<-function(todo,path="./delayed/"){
  expected<-todo %>% filter(status=="delayed") %>%
    mutate(country=substr(rcid,1,3)) %>%
    select(country,rcid,file.id,date.hqsubmission.planned.latest) %>% mutate("new date"= "PLEASE FILL")%>% split.data.frame(.,.$country)


  mapply(function(x,y){
    write.csv(x,paste0(path,"/",y,".csv"),row.names = F)
  },
  expected,names(expected)
  )


}


#' which data unit items should have already been submitted but havent?
#' @param rcm the research cycle matrix from rcm_download()
#' @param days_since_planned_submission number of days grace period; if 14 (default), shows only items that are more than two weeks overdue
#' @result subset of rcm with delayed items
#' @export
todo_delayed<-function(rcm,days_since_planned_submission=14){
  rcm[

    !is.na(rcm$file.id) &
      !is.na(rcm$date.hqsubmission.planned.latest) &
      (rcm$date.hqsubmission.planned.latest <= (Sys.Date()-days_since_planned_submission)) &
      # rcm_is_data_unit_item(rcm) &
      !(grepl("validated|HQ|field",rcm$status)),

    ] %>%
    (tibble::as_tibble) %>% arrange(date.hqsubmission.planned.latest,substr(rcid,1,3))
}


rcm_date_missing <- function(rcm){

  missing<- rcm %>% filter(unit=="data",
                           is.na(lubridate::ymd(date.hqsubmission.planned.latest)),
                           !grepl("validated|HQ|field|partner",status))

  return(invisible(missing))
}




#' write delays to google sheet
#' @param todo todo list (see ?todo_download)
#' @seealso todo_delayed_browse()
#' @export
rcm_delayed_to_google_sheet<-function(rcm){
 delayed_items<-todo_delayed(rcm) %>%
    mutate(country=substr(rcid,1,3)) %>%
    mutate("field.status"="to be submitted") %>%
    select(country,rcid,file.id,date.hqsubmission.planned.latest,field.status) %>%
      arrange((country),(rcid))

  date_missing<-rcm_date_missing(rcm) %>%
    mutate(country=substr(rcid,1,3)) %>%
    mutate("field.status"="to be submitted") %>%
    mutate(date.hqsubmission.planned.latest=NA) %>%
    select(country,rcid,file.id,date.hqsubmission.planned.latest,field.status) %>%
    arrange((country),(rcid))

delayed_or_no_date <- rbind(delayed_items,date_missing)

# lets try all at once:

researchcyclematrix:::g_sheets_append_row(t(delayed_or_no_date),spreadsheetId = "1fldR9_dx64otky6TvFMK29-pxUKrRUrpqdSzE8npVHA")


# if that doesn't work revert to this:
  # apply(delayed_or_no_date %>% arrange(country),1,
  #       function(x){
  #         researchcyclematrix:::g_sheets_append_row(x,spreadsheetId = "1fldR9_dx64otky6TvFMK29-pxUKrRUrpqdSzE8npVHA")
  #       })
  todo_delayed_browse()
}



#' open delays google sheet in browser
#' @export
todo_delayed_browse<-function(){
  browseURL("https://docs.google.com/spreadsheets/d/1fldR9_dx64otky6TvFMK29-pxUKrRUrpqdSzE8npVHA/")
}

#' open research cycle matrix google sheet in browser
#' @export
rcm_browse<-function(){
  browseURL("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/edit#gid=1202281367&fvid=70520525")
}

#' open submission google sheet in browser
#' @export
subs_browse<-function(){
  browseURL("https://docs.google.com/spreadsheets/d/1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8/")
}

