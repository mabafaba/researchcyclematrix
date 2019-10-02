




open_in_browser<-function(folder){

  y <- getwd()
  y <- gsub("/", "\\\\", y) %>% paste0("\\",folder)
  shell(paste0("explorer ", y), intern = TRUE)

}
