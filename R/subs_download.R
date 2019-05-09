
#' downloads items submitted for validation through the shiny submission form (stored in gdrive)
#' @return a data frame with all submissions
#' @export
subs_download<-function(){

  subs<-read.csv("https://docs.google.com/spreadsheets/d/1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8/gviz/tq?tqx=out:csv&sheet=submissions",
                stringsAsFactors = F)
  colnames(subs)<- c("submission.datetime",
                  "submission.date",
                  "country",
                  "rcid",
                  "file.id.new",
                  "file.id",
                  "new.file.id",
                  "round",
                  "email",
                  "comment",
                  "in.country.deadline",
                  "complete",
                  "type",
                  "emergency")



  subs$submission.datetime<-lubridate::as_datetime(subs$submission.datetime)

  subs$in.country.deadline<-lubridate::dmy(subs$in.country.deadline)
  subs$time.since.submission<-(lubridate::as_datetime(Sys.time())-subs$submission.datetime)
  subs$days.since.submission<-as.numeric(subs$time.since.submission,"days")
  subs<-subs[,!is.na(colnames(subs))] %>% as_tibble

  multiids<-subs
  multiids$rows<-1:nrow(multiids)

  ids_split<-multiids$file.id %>% strsplit("[[:space:]]_and id_[[:space:]]")
  ids_split[(sapply(ids_split,length)==0)]<-""
  multiids$ids_num<-sapply(ids_split,length)
  multiids$comment[multiids$ids_num>1]<-paste0(multiids$comment[multiids$ids_num>1],
                                               " _[[originally submitted ",multiids$ids_num[multiids$ids_num>1],
                                               " IDs at once:: ",
                                               multiids$file.id[multiids$ids_num>1])

  replications<-apply(multiids,1,function(x){rep(x["rows"],x["ids_num"])}) %>% unlist %>% as.numeric

  replicated<-multiids[replications,]
  replicated$file.id<-unlist(ids_split)
  replicated$rows<-NULL
  replicated$ids_num<-NULL
  subs<-replicated

  return(subs)
  }

#' find submission rows in the research cyclce matrix
#' @param subs the submissions from subs_download()
#' @param rcm the research cycle matrix from rcm_download()
#' @return a vector with row indices
#' @examples
#' # get the matching rcm rows:
#' rcm<-rcm_download()
#' subs<-subs_download()
#' subs_in_rcm_indices<-subs_rcm_rows(subs,rcm)
#' # subset rcm
#' rcm[subs_in_rcm_indices,]
subs_rcm_rows<-function(subs,rcm){
  match(subs$file.id,rcm$file.id)
}

#' Status of subs (asfound in rcm)
#' @export
subs_status<-function(subs,rcm){
  row_indices<-subs_rcm_rows(subs,rcm)
  found<-!is.na(row_indices)
  rcm_rows<-rcm[row_indices,]
  rcm_rows$status[!found]<-"not found in RCM"
  rcm_rows$status
}


#' Update RCM status based on form validation submissions
#' @param subs the submissions as received from subs_download()
#' @param rcm the RCM as received by rcm_download()
#' @return a data.frame listing which were / were not updated
#' @export
rcm_update_from_subs<-function(subs,rcm){
  subs$status<-subs_status(subs,rcm)
  ignore<-sapply(c("with HQ","validated","not found in RCM"),grepl,x=subs$status,simplify = F) %>% as.data.frame(stringsAsFactors=F)
  ignore<-apply(ignore,1,any)

  should_update<-sapply(c("not received","delayed","expected"),grepl,x=subs$status,simplify = F) %>% as.data.frame
  should_update$rcm_status_empty <- is.na(subs$status)
  should_update$rcm_status_empty[subs$status==""]<-TRUE
  should_update<-apply(should_update,1,any)

  ids_to_update<-subs$file.id[should_update & !ignore]
  ids_to_update<-unique(ids_to_update)
  ids_to_update<-ids_to_update[!is.na(ids_to_update)]
  ids_to_update<-ids_to_update[ids_to_update!=""]

  sapply(ids_to_update,rcm_set_to_withHQ)
  for(this_file.id in ids_to_update){
    this_file.id_date<-subs$submission.datetime[which(subs$file.id==this_file.id)[1]]
    rcm_set_submission_date(
      this_file.id,
      this_file.id_date)
  }
  log<-data.frame(file.id=subs$file.id,updated= subs$file.id %in% ids_to_update)
}



