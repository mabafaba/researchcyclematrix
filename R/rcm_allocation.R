




#' save the hours worked in the rcm
#' @param file.id the file id
#' @param hours the number of hours worked on the file id
#' @return the file id
#' @export
rcm_set_hours_worked <- function (file.id,hours){
  if(!is.na(hours)){
  if(assertthat::is.number(hours))
    hours<-as.numeric(hours)
  }else{
    hours<-"NA"
  }

  rcm_change_value(file.id, column = rcm_gsheet_column_letters$hours, value = hours)
  invisible(file.id)
}

#' create allocation table
#' @param who regex pattern matching HQ Focal Point (defaults to  "." which matches all)
#' @param rcm the research cycle matrix. leave empty to download fresh
#' @param rcm_raw the raw research cycle matrix; get with rcm_download(raw = T) or levae empty to download fresh. Need this because "hours" worked column is not included in the cleaned up rcm download (maybe we should include it).
#' @param include.file.ids vector of file ids that should be included even if they do not match the focal point pattern in "who" parameter
#' @param month integer of the month for which to create the allocation table
#' @param year integer of the year for which to create the allocation table (defaults to 2019; that default will likely change, so even if it's 2019, you should provide it in case that stable future behaviour is needed)
#' @export
rcm_allocation_table<-function(who=".",rcm=NULL,rcm_raw = NULL,include.file.ids = c(),month, year = 2019){



  # check input is ok:
  if(is.na(as.numeric(year))){stop("year must be a 4 digit number, i.e. 2020")}
  year<-as.numeric(year)
  if(as.numeric(year)<2000){stop("year must be >2000")}
  if(as.numeric(year)>2100){stop("validation table for years afer 2100 not implemented - sorry future people.")}

  if(is.na(as.numeric(month))){stop("month must be a single digit number, i.e. 12")}
  month<-as.numeric(month)
  if(as.numeric(month)<1){stop("month must be >0")}
  if(as.numeric(month)>12){stop("month must be <=12")}

  # if we want this for more than one staff member (i.e. who is a vector >1), call this function for each, put them all together with rbind and return:
  if(length(who)!= 1){
    all_allocs <- lapply(who,
                         rcm_allocation_table,
                         rcm=rcm,
                         rcm_raw=rcm_raw,
                         include.file.ids = include.file.ids,
                         month=month, year = year)

    return(as_tibble(do.call(rbind, all_allocs)))


  }

  # a function to turn c("a","b") into "a, b"
  comma_separated<-function(x){paste0(unique(x),collapse=', ')}

  # download rcm if not provided
  if(is.null(rcm)){rcm<-rcm_download(include_validated = T, include_archived = T)}
  if(is.null(rcm_raw)){rcm_raw <- rcm_download(raw = T)}

  # pick existing work hours from raw rcm

  rcm$hours<-rcm_raw$hours[match(rcm$file.id,rcm_raw$File.ID_Name)]


  allocation_table <- rcm %>%
    as_tibble %>%
    # add focal point
    # mutate(hq.fp = tolower(researchcyclematrix:::hq_focal_point(rcm$rcid))) %>%
    # filter to keep validated in this month in data unit, by focal point
    # filter(unit == "data") %>%
    filter(grepl(tolower(who),tolower(hq.fp)) | file.id %in% include.file.ids) %>%
    filter_validated_in_month(month,year) %>%
    # reconfigure file id to match requirements for allocation table format: "RCID - Description"
    rowwise %>%
    mutate(output = gsub(paste0(rcid,"."),"",file.id)) %>%
    mutate(output = gsub("[^[:alnum:]]", " ", output)) %>%
    mutate(output = paste0(rcid, " - ", output)) %>%
    ungroup %>%
    # aggregate by focal point and project code
    group_by(hq.fp,project.code) %>%
    summarise(`No of working days`  = sum(hours,na.rm = T),
              `automatic RC` = comma_separated(rcid),
              `RCs - outputs` = comma_separated(output)
    ) %>%
    # sort by project code
    arrange(project.code) %>%
    ungroup %>%
    # get relevant columns only
    select(hq.fp,project.code,`automatic RC`,`RCs - outputs`,`No of working days`)
}


#' filter rows from rcm that have state 'validated' and date in a certain month / year
#' @param rcm the RCM
#' @param month the month to keep (single integer)
#' @param year the year to keep (single integer; defaults to 2019)
filter_validated_in_month<-function(rcm,month,year = 2019){
  rcm %>% filter(
    grepl('validated',status),
    lubridate::month(lubridate::dmy(rcm$date.validated))==month & lubridate::year(lubridate::dmy(rcm$date.validated))==year)
}
