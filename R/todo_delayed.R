


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
      rcm_is_data_unit_item(rcm) &
      !(grepl("validated|HQ|field",rcm$status)),

    ] %>%
    as_tibble %>% arrange(date.hqsubmission.planned.latest,substr(rcid,1,3))
}



#' write delays to google sheet
#' @param todo todo list (see ?todo_download)
#' @seealso todo_delayed_browse()
#' @export
todo_delayed_to_google_sheet<-function(todo){
  apply(rcm_delayed(todo),1,
        function(x){
          researchcyclematrix:::g_sheets_append_row(x,spreadsheetId = "1fldR9_dx64otky6TvFMK29-pxUKrRUrpqdSzE8npVHA")
        })
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

