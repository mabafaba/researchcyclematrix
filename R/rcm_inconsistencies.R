
#' Check inconsistencies in RCM
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return data frame with list of inconsistencies
#' @export
rcm_check<-function(rcm,check.archived=F,check.validated=F){
  if(!check.archived){rcm<-rcm[rcm$archived!=T,]}
  if(!check.validated){rcm<-rcm[rcm$status!="validated",]}


  issues<-rbind(
    rcm_find_issue(rcm,rcm_issue$passed_planned_not_recieved, issue_name = "planned submission passed but no received date"),
    rcm_find_issue(rcm,rcm_issue$dates_missing, issue_name = "none of the hq submission dates (planned/received) available"),
    rcm_find_issue(rcm,rcm_issue$with_hq_but_no_received_date, issue_name = "no received date but with HQ"),
    rcm_find_issue(rcm,rcm_issue$with_field_but_not_received_date, issue_name = "no received date but with field"),
    rcm_find_issue(rcm,rcm_issue$data_unit_no_status, issue_name = "data unit item with non-standardisable status"),
    rcm_find_issue(rcm,rcm_issue$duplicated_file_id, issue_name = "file-id-name not unique")
  )

  # issues_data_unit_no_status<-data.frame(id=which(is.na(rcm$date.hqsubmission.planned.latest) &
  #                                                   is.na(rcm$date.hqsubmission.planned.first) &
  #                                                   is.na(rcm$date.hqsubmission.actual)),
  #                                        issue = "no planned/received date for")

  issues$issue<-as.factor(issues$issue)
  issues$rcid<-rcm$rcid[issues$id]

  # issues$file.id<-rcm$file.id[issues$id]
  issues$`file id`<-rcm$link[issues$id]
  issues$comment<-rcm$comment[issues$id]
  issues$status<-rcm$status[issues$id]
  if(nrow(issues)>0){
    message(paste(nrow(issues),"inconsistencies in RCM:"))
    print(kable(list(issues$issue,rcm_unit(rcm[issues$id,])) %>% table))}else{
      message("no inconsistencies in RCM detected.")
    }
  issues$unit<-rcm$unit[issues$id]
  issues
}




#' applies an issue checking function to the RCM
#'
rcm_find_issue<-function(rcm,get_issue_ids_fun,issue_name){
  id=get_issue_ids_fun(rcm)
  if(length(id)>0){
    return(data.frame(id=id,issue=issue_name))
  }else{
    data.frame(id=character(0),issue=character(0))
  }

}

rcm_issue<-list()
rcm_issue$dates_missing<-function(rcm){which(is.na(rcm$date.hqsubmission.planned.first) & is.na(rcm$date.hqsubmission.planned.latest) & is.na(rcm$date.hqsubmission.actual))}
rcm_issue$with_hq_but_no_received_date<-function(rcm){which(status_with_hq(rcm) & is.na(rcm$date.hqsubmission.actual))}
rcm_issue$with_field_but_not_received_date<-function(rcm){which(status_with_field(rcm) & is.na(rcm$date.hqsubmission.actual))}
rcm_issue$data_unit_no_status<-function(rcm){which(rcm_is_data_unit_item(rcm) & !rcm_has_identified_status(rcm))}
rcm_issue$passed_planned_not_recieved<-function(rcm){which(rcm_past_planned_submission(rcm) & (is.na(rcm$date.hqsubmission.actual)))}
rcm_issue$duplicated_file_id<-function(rcm){which(duplicated(rcm$file.id))}


#' Identify research cycles that do not have data and analysis items
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return  data frame with columns..
#' - data.missing ("exists" or "missing")
#' - analysis.missing ("exists" or "missing")
#' - RCID (the research cycle ID)
#'
#' @export
rcm_missing_data_unit_items<-function(rcm){
  rcm %>% split.data.frame(rcm$rcid) %>% lapply(function(x){
    has_data_line<- ifelse(any(grepl("data",x$type)),"exists","missing")
    has_analysis_line<-ifelse(any(grepl("analysis",x$type)),"exists","missing")

    summary<-c(data.missing=has_data_line,analysis.missing=has_analysis_line)
    if(all(summary=="exists")){return(NULL)}
    summary
  }) %>% do.call(rbind,.) %>% (function(x){cbind("RCID"=rownames(x),x)}) %>% `rownames<-`(NULL)
}



