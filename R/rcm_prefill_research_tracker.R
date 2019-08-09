
#' create a prefilled monthly RC tracker xlsx file
#' @param the rcm; see rcm_download()
#' @param output_xlsx_file_name the path to the newly created file
#' @return writes an xlsx template with prefilled file id rows as a side effect (if xlsx package is installed). Returns a data.frame with the rows that were prefilled
#' @details only creates an xslx template file if the _xlsx package is installed_.
#' In any case this function will return (invisibly) a data.frame with the rows that need to be put in the tracker.
#' if you want an xlsx template file to be created, you need to install.packages("xlsx"). Careful you must have compatible R and Java versions for this to work (both 32 or both 64 bit)
#' @export
rcm_prefill_research_tracker<-function(output_xlsx_file_name = paste0("tracker_",Sys.Date(),".xlsx"), rcm = NULL, use_xlsx_package = TRUE){

  if(!grepl("\\.xlsx$",output_xlsx_file_name)){stop("output_xlsx_file_name must end with '.xlsx'")}


  # make sure global options are changed back in the end (even if function fails with on.exit):
  xlsx_original_date_format<-options("xlsx.date.format")[[1]]
  on.exit({options(xlsx.dat.format = xlsx_original_date_format)})

  # get list of all file ids, check inconsistencies with rcm_check and create columns that match the template:
  tracker_rows<-rcm_prefill_research_tracker_create_rows(rcm)

  xlsx_installed<-requireNamespace("xlsx", quietly = TRUE)

  if(!use_xlsx_package){
    warning("user requested to not use the xlsx package. no xlsx file is created, just a data.frame with the rows to prefill will be returned.")
    return(tracker_rows)
  }else if(!xlsx_installed){
    warning("can not read/write xlsx because the package 'xlsx' is not installed. no xlsx file is created, just a data.frame with the rows to prefill will be returned. You can try running: \ninstall.packages('xlsx')")
    return(tracker_rows)
  }


  xlsx_original_date_format<-options("xlsx.date.format")[[1]]
  options(xlsx.dat.format = "dd/mm/yyyy")
  # get tracker template:
  tracker <- list.files(base::system.file(package = "researchcyclematrix"),full.names = T) %>%
    grep("Research tracker - template - V4.xlsx", ., value = TRUE) %>%
    (xlsx::loadWorkbook)

  tracker_sheets<-xlsx::getSheets(tracker)


  xlsx::addDataFrame(x = tracker_rows %>% as.data.frame(stringsAsFactors=FALSE),
               sheet = tracker_sheets[[3]],
               startRow = 5,
               startColumn = 1,
               row.names = FALSE,col.names = FALSE)
  xlsx::saveWorkbook(tracker, file=output_xlsx_file_name)

  message(crayon::black(paste0("to open the produced file run:\n browseURL(\"",output_xlsx_file_name,"\")")))
  # set global xlsx date format option back to original so we don't alter the global state:

  return(invisible(tracker_rows))

}




rcm_prefill_research_tracker_create_rows<-function(rcm = NULL){

  if(is.null(rcm)){
    rcm<-rcm_download(include_archived = FALSE,
                      include_validated = FALSE,
                      gdrive_links = FALSE)
  }

rcm<-rcm[rcm$file.id!="",]
rcm %<>% filter(file.id!="No TOR",
                           file.id!='TBD',
                           file.id!='NO TOR',
                           file.id!="",
                           !is.na(file.id))
  issues <- rcm %>% rcm_check


  issues <-issues %>%
    filter(grepl("planned submission passed but no received date|none of the hq submission dates", issue))
  issues$request<-issues$issue %>% as.character()

  issues$request[grepl("none of the hq submission dates", issues$issue)]<-"Estimated date of submission to HQ missing, please add in \"Date to HQ\" column"

  issues$request[grepl("planned submission passed but no received date", issues$issue)]<-"Estimated date of submission to HQ has passed, please add new estimated date in “Date to HQ” column and briefly explain delay in CFP comments"


  rcm$`HQ Update Request`<- issues[match(rcm$file.id,issues$`file id`),"request"]
  rcm$`HQ Update Request`<-as.character(rcm$`HQ Update Request`)
  rcm$`HQ Update Request`[is.na(rcm$`HQ Update Request`)]<- ""
  rcm$`Country`<-substr(rcm$rcid,1,3)
  rcm$`Research cycle title`<-""
  rcm$`Project Code`<- ""
  rcm$`CFP comment`<-""
  rcm$`Date to HQ` <- rcm$date.hqsubmission.actual
  rcm$`Date to HQ`[is.na(rcm$`Date to HQ`)]<-rcm$date.hqsubmission.planned.latest[is.na(rcm$`Date to HQ`)]
  rcm$`Date to HQ`[is.na(rcm$`Date to HQ`)]<-rcm$date.hqsubmission.planned.first[is.na(rcm$`Date to HQ`)]

  tracker_rows<-rcm %>% filter(!is.na(file.id) & file.id !="") %>% as_tibble %>% select(
                                                                          Country,
                                                                          `HQ Unit` = unit,
                                                                          `Research cycle title`,
                                                                          `Research Cycle ID`  = rcid,
                                                                          # `Project Code`,
                                                                          `File Type` = type,
                                                                          `file ID` = file.id,
                                                                          `HQ Update Request`,
                                                                          `CFP comment`,
                                                                          `Date to HQ`,
                                                                          `status`
  ) %>% arrange(Country, `Research Cycle ID`, `HQ Unit`, `File Type`,`file ID`) %>%
    return
  # %>%  write.csv("all_ongoing_items.csv")



}
