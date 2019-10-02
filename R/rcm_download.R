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
  # print("downloading rcm...")
  rcm<-read.csv("https://docs.google.com/spreadsheets/d/1wX5k3cETrCbnw4vpfY07eSzTyWX6AwmJmxJQwPahrSk/gviz/tq?tqx=out:csv&sheet=RC_Matrix_2.1",
                stringsAsFactors = F)

  x<-rcm[1,]
  if(remove_empty){rcm<-rcm[apply(rcm,1,function(x){
    x<-x[-which(names(x)=="index")];!all(is.na(x)|x=="")}),]
  }
  if(raw){
    # message("RCM requested in raw format. ignoring all other parameters.")
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
