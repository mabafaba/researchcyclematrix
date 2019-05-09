


# rcm_premature_validation<-function(rcm){
#   rcm$validation.depends.data<-rcm$type %in% c("report","factsheet","presentation","situation overview")
#
#   rcm %>% split.data.frame(paste(rcm$rcid,rcm$round)) %>% function(x){
#     rcm$type %>% table
#   }
#
# }

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


#' display a row in the rcm based on the file id
#' @param rcm the RCM
#' @param file.id the file id, or a file id search term (see `?rcm_find_row_by_file.id` for details)
#' @value no output, just printing to console
rcm_show<-function(rcm,file.id){
  rcm<-as.data.frame(rcm,stringsAsFactors=F)
  todorow<-as_tibble(rcm_find_row_by_file.id(rcm,file.id))
  if(nrow(todorow)==0){message(silver(paste0(italic(file.id)," not found")));return(invisible(NULL))}
  message(paste((crayon::silver(todorow["rcid"])),
                crayon::bgBlack(crayon::white(crayon::bold(todorow["file.id"])))))
  if(grepl("HQ",tolower(todorow$status))){
    message(paste0(black("status: "),red(blurred("with HQ"))))
  }
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
