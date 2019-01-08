
#' standardising columns
#'
#' Regexing messy values into predefined categories and renames them. Applied by default in \link{\code{rcm_download}}
#' @export
rcm_standardised_columns<-function(rcm){

  # people keep changing the column headers on gdrive..:

  rcm$Archived<-rcm[[grep("Archived",names(rcm))]]
  rcm$Archived.Yes..No<-NULL
  if("Current.status.Pending..With.Field..With.HQ..Internally.validated..Externally.validated..Internally.AND.Externally.validated...RC.publication.pending..Internally.AND.Externally.validated...RC.public..Internally.AND.Externally.validated...RC.not.public"
     %in% names(rcm)){
    rcm$Current.status<-rcm$Current.status.Pending..With.Field..With.HQ..Internally.validated..Externally.validated..Internally.AND.Externally.validated...RC.publication.pending..Internally.AND.Externally.validated...RC.public..Internally.AND.Externally.validated...RC.not.public
    rcm$Current.status.Pending..With.Field..With.HQ..Internally.validated..Externally.validated..Internally.AND.Externally.validated...RC.publication.pending..Internally.AND.Externally.validated...RC.public..Internally.AND.Externally.validated...RC.not.public<-NULL
  }
  rcm$original.status<-rcm$Current.status
  rcm$Current.status<-tolower(rcm$Current.status)
  rcm$status<-rcm$Current.status
  rcm$status[rcm$Current.status %>% grepl("validated|Validated",.)]<-"3 validated"
  rcm$status[rcm$Current.status %>% grepl("hq|HQ",.)]<-"2 with HQ"
  rcm$status[rcm$Current.status %>% grepl("not received",.)]<-"1 not received"
  rcm$status[rcm$Current.status %>% grepl("partner",.)]<-"with partner"
  rcm$status[rcm$Current.status %>% grepl("field",.)]<-"with field"
  rcm$Archived[!(rcm$Archived %>% tolower %>% grepl("y",.))]<-"FALSE"
  rcm$Archived[rcm$Archived %>% tolower %>% grepl("y",.)]<-"TRUE"

  rcm$Archived<-as.logical(rcm$Archived)
  for(i in c(1:ncol(rcm))){
    if(is.factor(rcm[,i])){
      rcm[,i]<-as.character(rcm[,i])
    }
  }
  rcm$File.type <-tolower(rcm$File.type)
  rcm$File.type[grepl("terms|tor",rcm$File.type)]<-"ToR"
  rcm$File.type[grepl("data",rcm$File.type) & grepl("analysis",rcm$File.type)]<-"data & analysis"
  rcm$File.type[grepl("data",rcm$File.type) & (rcm$File.type!="data & analysis")]<-"data"
  rcm$File.type[grepl("analysis",rcm$File.type) & (rcm$File.type!="data & analysis")]<-"analysis"
  rcm$File.type[grepl("map",rcm$File.type)]<-"map"
  rcm$File.type[grepl("m\\&e",rcm$File.type)]<-"M&E"
  rcm$File.type[grepl("presentation",rcm$File.type)]<-"presentation"
  rcm$id<-paste0(rcm$Research.Cycle.ID,":::",rcm$File.type,":::",rcm$File.ID_Name)

  # rcm %>% rename(date.endcollection.planned = Data.Collection...Planned.date.of.completion,
  #                date.endcollection.actual = Data.collection...Actual.date.of.completion,
  #                date.hqsubmission.planned.first = File.submission.to.HQ...First.planned.date..of.last.file.if.several.,
  #                date.hqsubmission.planned.latest = File.submission.to.HQ...Latest.planned.date..of.last.file.if.several.,
  #                date.hqsubmission.actual = File.submission.to.HQ...Actual.date.received..of.last.file.if.several.,
  #                date.feedback = First.round.of.feedback.submitted.to.field..of.last.file.if.several.,
  #                date.validated = Date.validated.on.RC,
  #                rcid=Research.Cycle.ID,
  #                round=month...round...batch,
  #                date.milestone = Milestone.date,
  #                file.id = File.ID_Name,
  #                type=File.type,
  #                archived=Archived
  #
  #                ) -> rcm
  #
  #
  #
  #
  rcm_renaming_findname<-function(x){
    grep(x,names(rcm),value=T)[1]
  }
  rcm %>% rename(date.endcollection.planned = {rcm_renaming_findname("Data.Collection...Planned.date.of.completion")},
                 date.endcollection.actual = {rcm_renaming_findname("Data.collection...Actual.date.of.completion")},
                 date.hqsubmission.planned.first = {rcm_renaming_findname("File.submission.to.HQ...First.planned.date..of.last.file.if.several")},
                 date.hqsubmission.planned.latest = {rcm_renaming_findname("File.submission.to.HQ...Latest.planned.date..of.last.file.if.several")},
                 date.hqsubmission.actual = {rcm_renaming_findname("File.submission.to.HQ...Actual.date.received..of.last.file.if.several")},
                 date.feedback = {rcm_renaming_findname("First.round.of.feedback.submitted.to.field..of.last.file.if.several")},
                 date.validated = {rcm_renaming_findname("Date.validated.on.RC")},
                 rcid={rcm_renaming_findname("Research.Cycle.ID")},
                 round={rcm_renaming_findname("month...round...batch")},
                 date.milestone = {rcm_renaming_findname("Milestone.date")},
                 file.id = {rcm_renaming_findname("File.ID_Name")},
                 type={rcm_renaming_findname("File.type")},
                 archived={rcm_renaming_findname("Archived")},
                 comment={rcm_renaming_findname("Comments")}

  ) -> rcm
  names(rcm)<-tolower(names(rcm))
  rcm$unit<-rcm_unit(rcm)


  rcm
}


#' filling dates (used in rcm_download when fill_dates=T)
#'
rcm_fill_dates<-function(rcm){
  rcm$date.hqsubmission.planned.latest[is.na(rcm$date.hqsubmission.planned.latest)]<-
    rcm$date.hqsubmission.planned.first[is.na(rcm$date.hqsubmission.planned.latest)]
  rcm
}
