#' who is the focal point for the rcid?
#' @param rcid a vector of research cycle ids
#' @return a vector of surnames of the HQ data unit focal point for the research cycle
hq_focal_point<-function(rcid){
  focalpointlist<-matrix(
    c(
      "AFG","Martin",
      "BGD","Martin",
      "BRA","Eliora",
      "CAM","Sharon",
      "CAR","Sharon",
      "COL","Eliora",
      "DRC","Eliora",
      "GLO","Martin",
      "LEB","Martin",
      "IND","Martin",
      "IRQ","Martin",
      "ITA","Eliora",
      "JOR","Martin",
      "KEN","Eliora",
      "LBY","Eliora",
      "NER","Eliora",
      "NGA","Sharon",
      "REG","Martin",
      "SOM","Eliora",
      "SSD","Sharon",
      "SYR","Sharon",
      "TNS","Eliora",
      "UGA","Eliora",
      "UKR","Martin",
      "YEM","Martin"
    )

    ,ncol = 2,byrow = T
  )

  colnames(focalpointlist)<-c("country","fp")

  country<-rcid %>% substr(1,3)
  focalpointlist[match(country,focalpointlist[,"country"]),"fp"]
}

