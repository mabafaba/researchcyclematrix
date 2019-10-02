#' this is the backend behind rcm_dashboard; it prepares some data and adds them to an environment in which then ./inst/country_status.Rmd is executed
#' @param rcm the researchcyclematrix; should contain validated items too
#' @param delayed output from researchcyclematrix::todo_delayed()
#' @param inconsistencies output from rcm_check()
#' @param path the path to save the html file to
#' @param filename the name of the output html file (must end in '.html')
country_status_markdown <- function(rcm,delayed,inconsistencies, path="./", filename="country_overview.html") {

  # find the complete path to the Rmd file stored inside the package (in /inst)
    template <-
      system.file("md_templates",
                  "country_status.Rmd",
                  package = "researchcyclematrix")

  # create a new environment
  render_environment <- new.env()

  # add inputs to the environment
  render_environment$rcm <- rcm
  render_environment$inconsistencies <- inconsistencies
  render_environment$delayed <- delayed

  #render rmarkdown to HTML inside that enviornment
  rmarkdown::render(
    template,
    output_file = filename,
    output_dir = path,
    intermediates_dir = path,
    envir = render_environment,
    knit_root_dir = getwd()
  )

  # put together target file path
  full_path<-path %>% gsub("/$","",.) %>% gsub("^/","",.)
  full_path<-paste0(getwd(),full_path)
  message("document written to:")
  message(full_path)
  # open page
  browseURL(paste0(full_path,"/",filename))
  # return the path but don't print it
  invisible(full_path)

}















#' compile an overview report of each countries status
#' @param rcm can be ommited
#' @param subs can be ommited
#' @param update_rcm logical: whether to update the rcm from the submissions sheet before producing the report. Slows it down a bit but hey at least your report is up to date afterwards. default is TRUE
#' @param unit which unit(s) to include? Defaults to 'data'. uses regex: for all units, enter a dot "."; for multiple units, use | operator (i.e. "data|reporting|research design|gis")
#' @return nothing interesting, but writes a html file into the working directory and opens it in the browser
#' @export
rcm_dashboard <- function(rcm=NULL,subs=NULL,update_rcm = T, unit = "data"){


  if(is.null(rcm)){rcm<-rcm_download(include_validated=T)}
  if(is.null(subs)){subs<-subs_download()}



  if(update_rcm){
  message(crayon::black("updating matrix with any new submissions.."))
  rcm_update_from_subs(subs,rcm)
  }


  rcm<-rcm[!(rcm$rcid == "" & rcm$file.id ==""),]

  rcm<-rcm[grepl(unit,rcm$unit,ignore.case = TRUE),]
  todo<-todo_create(rcm,subs)
  delayed<-todo_delayed(rcm)
  inconsistent<-rcm_check(rcm)

  # inconsistent<-inconsistent #%>% filter(
    # !(issue %in% c("planned submission passed but no received date", "data unit item with non-standardisable status"))
  # )

  country_status_markdown(rcm = rcm,delayed = delayed,inconsistencies = inconsistent)

  # summary_comments<-paste(rcm$rcid[has_comment],": ",rcm$comment[has_comment])

}



















