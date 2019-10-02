#' who is the focal point for the rcid?
#' @param rcid a vector of research cycle ids
#' @return a vector of surnames of the HQ data unit focal point for the research cycle
hq_focal_point<-function(rcid){

  # download list from the focalpoints sheet in the submnitted validations google doc

  focalpointlist<-read.csv("https://docs.google.com/spreadsheets/d/1iNt__-uMMBTbLEsJkiIXglPJ4GK-9UCVqC7awhMTXF8/gviz/tq?tqx=out:csv&sheet=focalpoints",
                                          stringsAsFactors = F)


  colnames(focalpointlist)<-c("country","fp",'fp2','fp3')

  country<-rcid %>% substr(1,3)

  # pick the first one only; one day maybe we can expand to somehow consider second/third fp
  focalpointlist[match(country,focalpointlist[,"country"]),"fp"]
}

