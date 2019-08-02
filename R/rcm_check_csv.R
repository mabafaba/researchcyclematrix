



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


open_in_browser<-function(folder){

  y <- getwd()
  y <- gsub("/", "\\\\", y) %>% paste0("\\",folder)
  shell(paste0("explorer ", y), intern = TRUE)

}
