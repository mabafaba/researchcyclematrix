

researchtracker_create<-function(x){

  required_colnames<-tolower(c("country", "hq.unit", "research.cycle.title", "research.cycle.id",
                               "file.type", "file.id", "hq.update.request", "cfp.comment", "date.to.hq",
                               "status")
  )

  colnames(x)<-tolower(colnames(x))

  if(!all(required_colnames %in% colnames(x))){
    stop(paste("missing colnames:",paste0(required_colnames[which(!(required_colnames %in% colnames(df)))],collapse = ", ")))}

  x<-x[,required_colnames] %>% (tibble::as_tibble)

  x$date.to.hq<-lubridate::dmy(x$date.to.hq)
  x
}

researchtracker_read_xlsx<-function(file){
  tracker<-xlsx::read.xlsx(file = file,sheetIndex = 3,startRow = 4,trim_ws = T,stringsAsFactors = FALSE)
  researchtracker_create(tracker)
}

researchtracker_compare_rcm<-function(researchtracker,rcm = NULL){
  if(is.null(rcm)){rcm<-rcm_download(include_archived = F,include_validated = F,gdrive_links = F)}

}

log_column_changes<-function(df1,df2,compare_df1_variable, to_df2_variable){
  variable_name<-deparse(substitute(compare_df1_variable))
  df1_name<-deparse(substitute(df1))
  df2_name<-deparse(substitute(df2))

  df1<-select(df1,file.id,{{compare_df1_variable}})
  colnames(df1)<-c("file.id","value_df1")
  df1[,2]<-as.character(unlist(df1[,2]))
  df2<-select(df2,file.id,{{to_df2_variable}})
  df1[,2]<-as.character(unlist(df2[,2]))
  colnames(df2)<-c("file.id","value_df2")
  joined <- df1 %>%
    right_join(df2,by=c("file.id"="file.id")) %>%
    # select({{compare_df1_variable}},{{compare_df1_variable}}) %>%
    as_tibble
  joined$changed<-NA
  joined<-joined %>% rowwise %>% mutate(changed =
                                          (length(which(c(is.na(value_df1),is.na(value_df2))))==1) | value_df1 !=value_df2 ) %>% ungroup
  joined$changed[is.na(joined$changed)]<-FALSE
  joined<-joined %>% filter(changed) %>% select(-changed)
  joined$column <- variable_name
  colnames(joined)<-c("file.id",df1_name,df2_name,"column")

  joined
}


