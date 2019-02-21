

#' Get only submissions where new ids were suggested
#' @param subs the submission list as received by subs_download()
#' @return the subset of the submission list
#' @export
subs_with_new_id<-function(subs){

  subs[subs$file.id.new,]

}

