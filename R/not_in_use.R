

# not in use I believe but I think was a good idea to have one function that decides validation priority order that can then be used anywhere

#' sort RCM by priority
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return the research cycle matrix sorted by 1. milestone passed, 2. days with HQ
rcm_sort_priority<-function(rcm){
alpha<-rcm_passed_milestone(rcm) %>% as.numeric
beta<-rcm_days_with_hq(rcm) %>% as.numeric
alpha[is.na(alpha)]<-0
beta[is.na(beta)]<-0
alpha = alpha*10e5
}

# also not in use; rcm just prints as a tibble. Todo has it's own print method print.todo

#' display a row in the rcm based on the file id
#' @param rcm the RCM
#' @param file.id the file id, or a file id search term (see `?rcm_find_row_by_file.id` for details)
#' @return no output, just printing to console
rcm_show<-function(rcm,file.id){
  rcm<-as.data.frame(rcm,stringsAsFactors=F)

  todorow<-tibble::as_tibble(rcm_find_row_by_file.id(rcm,file.id))
  if(nrow(todorow)==0){message(crayon::silver(paste0(italic(file.id)," not found")));return(invisible(NULL))}

  # all of this is just printing to the console; crayon:: functions are there for styling

  # print the file id
  message(paste((crayon::silver(todorow["rcid"])),
                crayon::bgBlack(crayon::white(crayon::bold(todorow["file.id"])))))
  if(grepl("HQ",tolower(todorow$status))){
    message(paste0(black("status: "),red(blurred("with HQ"))))
  }

  # print the status
  if(grepl("field",tolower(todorow$status))){message(paste0(black("status: "),red(blurred("with field"))))}
  if(grepl("partner",tolower(todorow$status))){message(paste0(black("status: "),red(blurred("with partner"))))}
  if(grepl("validated",tolower(todorow$status))){message(paste0(black("status: "),green(blurred("validated"))))}

  if(!todorow$date.endcollection.planned%in%c("",NA)){message(regular_style(paste0(bold(todorow$date.endcollection.planned)," planned end of collection")))}
  if(!todorow$date.endcollection.actual%in%c("",NA)){message(regular_style(paste0(bold(todorow$date.endcollection.actual)," actual end of collection")))}
  if(!todorow$date.hqsubmission.planned.latest%in%c("",NA)){message(regular_style(paste0(bold(todorow$date.hqsubmission.planned.latest)," latest submission planned date")))}
  if(!todorow$date.hqsubmission.actual%in%c("",NA)){message(regular_style(paste0(bold(todorow$date.hqsubmission.actual)," actual submission date")))}
  if(!todorow$date.validated%in%c("",NA)){message(regular_style(paste0(bold(todorow$date.validated)," date validated")))}

  message("\n")
}




# DEPRECIATED: before the monthly tracker existed i thought it was a good idea to make csv files with inconsistencies for each country

#' write matrix issues to file for each HQ focal point
#'
#' @param rcm use rcm_download(gdrive_links=F). can be ommited (then downloading from gdrive)
#' @return writes csv files to current wd, and also opens them in excel directly!
#' @export
rcm_check_csv<-function(rcm=NULL, unit = "data"){

  if(is.null(rcm)){
    rcm<-rcm_download(include_archived = F,include_validated = F,gdrive_links = F)
  }

  rcm<-rcm[grepl(unit,rcm$unit),]
  inconsistencies<-rcm_check(rcm,check.archived = F,check.validated = F)
  inconsistencies<-inconsistencies[inconsistencies$issue!="data unit item with non-standardisable status",]
  inconsistencies <- inconsistencies %>% dplyr::arrange(rcid)

  inconsistencies %>% split.data.frame(hq_focal_point(inconsistencies$rcid)) %>% mapply(.,names(.),FUN = function(inc,who){
    thisfile<-paste0("inconsistencies_",who,".csv")
    write.csv(inc,file = thisfile,row.names = F)
    open_in_browser(thisfile)
  })


  invisible(inconsistencies)
}
