
#' has the submission date passed?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_past_planned_submission<-function(rcm){
  rcm$date.hqsubmission.planned.latest<=Sys.Date()
}





#' is with HQ?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
status_with_hq<-function(rcm){
  rcm$status %>% grepl("HQ",.)
}

#' is with field?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
status_with_field<-function(rcm){
  rcm$status %>% grepl("field",.)
}

#' how long has the item been with HQ?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return Date time difference vector
#' @export
rcm_days_with_hq<-function(rcm){
  rcm$days.with.hq<-NA
  whq<-grepl("HQ",rcm$status)
  rcm$days.with.hq[whq]<-Sys.Date()-rcm$date.hqsubmission.actual[whq]
  rcm$days.with.hq
}

#' is with field?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return a subset of the RCM: only rows that are with HQ; only basic information columns. Sorted by added column "days.with.hq"
#' @export
rcm_longest_with_hq<-function(rcm,n=NULL,add.columns=c()){
  rcm$days.with.hq<-rcm_days_with_hq(rcm)
  rcm<-rcm[!is.na(rcm$days.with.hq),]
  if(is.null(n)){n<-nrow(rcm)}
  if(nrow(rcm)<=1){warning("no items marked \"with HQ\" have a received date.")}
  return(rcm[order(rcm$days.with.hq,decreasing = TRUE),] %>% head(n) %>% select(c("link","rcid","days.with.hq","status","unit",add.columns)))
}

#' is a date today or older?
#' @param date date(s) to check
#' @return logical vector if date is today or in the passed
#' @export
date_arrived<-function(date){
  date<=Sys.Date()
}

#' has an item missed its milestone?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_passed_milestone<-function(rcm){
  date_arrived(rcm$date.milestone)
}


#' Days until a given date as ordinal categories
#'
#' Changes a date of a deadline into words such as "today", "tomorrow", "within 30 days"
#' @param date a vector of dates
#' @return vector with ordinal values "more than 30 days", "within 30 days", "within 14 days", "within 7 days", "tomorrow", "today" and "overdue"
#' @export
rcm_deadline_status<-function(date){
  deadlinestatus<-factor(length(date),levels = c("more than 30 days","within 30 days","within 14 days","within 7 days","tomorrow","today","overdue"))
  today<-Sys.Date()
  deadlinstatus[(date-today)>30]<-"more than 30 days"
  deadlinstatus[(date-today)<=30]<-"within 30 days"
  deadlinstatus[(date-today)<=14]<-"within 14 days"
  deadlinstatus[(date-today)<=7]<-"within 7 days"
  deadlinestatus[(date-today)==1]<-"tomorrow"
  deadlinstatus[date==today]<-"today"
  deadlinstatus[today>date]<-"overdue"
  deadlinstatus
}

#' is the RCM status coercible into a standard state?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_has_identified_status<-function(rcm){
  grepl("HQ|validated|field|partner|not received",rcm$status)
}

#' do RCM rows belong to the data unit?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_is_data_unit_item<-function(rcm){
  rcm$type %in% c("data","analysis","data & analysis")
}

#' do RCM rows belong to the reporting unit?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_is_reporting_unit_item<-function(rcm){
  rcm$type %in% c("report","factsheet","situation","presentation")
}

#' do RCM rows belong to the research design unit?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_is_design_unit_item<-function(rcm){
  rcm$type %in% c("ll matrix","M&E","concept note","methodology note","sampling frame","ToR")
}

#' do RCM rows belong to the GIS unit?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return logical vector
#' @export
rcm_is_gis_unit_item<-function(rcm){
  rcm$type %in% c("map")
}

#' which unit do RCM rows belong to?
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return character vector  (with values "data", "reporting", "research design" or "GIS")
#' @export
rcm_unit<-function(rcm){
  rcm$unit<-"other"
  rcm$unit[rcm_is_data_unit_item(rcm)]<-"data"
  rcm$unit[rcm_is_reporting_unit_item(rcm)]<-"reporting"
  rcm$unit[rcm_is_design_unit_item(rcm)]<-"research design"
  rcm$unit[rcm_is_gis_unit_item(rcm)]<-"GIS"
  return(rcm$unit)
}




#' Is an item expected for submission?
#'
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return A subset of the research cycle matrix: all items with planned dates arrived/passed but don't have status HQ, validated, with field or with partner
#' @export
rcm_submission_expected<-function(rcm){
  expected <-  (date_arrived(rcm$date.hqsubmission.planned.latest) & is.na(rcm$date.hqsubmission.actual) & !(grepl("HQ|validated|field|partner",rcm$status)))
  expected<-which(expected)
  rcm[expected,c("link","rcid","date.hqsubmission.planned.latest","status","unit")]
}
