#' Map results to an output template
#'
country_status_markdown <- function(rcm,delayed,inconsistencies, path="./", filename="country_overview.html") {

    template <-
      system.file("md_templates",
                  "country_status.Rmd",
                  package = "researchcyclematrix")

  render_environment <- new.env()

  render_environment$rcm <- rcm
  render_environment$inconsistencies <- inconsistencies
  render_environment$delayed <- delayed
  rmarkdown::render(
    template,
    output_file = filename,
    output_dir = path,
    intermediates_dir = path,
    envir = render_environment,
    knit_root_dir = getwd()
  )

  full_path<-path %>% gsub("/$","",.) %>% gsub("^/","",.)
  full_path<-paste0(getwd(),full_path)
  message("document written to:")
  message(full_path)
  browseURL(paste0(full_path,"/",filename))
  invisible(full_path)

}















#' compile an overview report of each countries status
#' @param rcm can be ommited
#' @param subs can be ommited
#' @update_rcm logical: whether to update the rcm from the submissions sheet before producing the report. Slows it down a bit but hey at least your report is up to date afterwards. default is TRUE
#' @return nothing interesting, but writes a html file into the working directory and opens it in the browser
#' @export
rcm_dashboard <- function(rcm=NULL,subs=NULL,update_rcm = T){


  if(is.null(rcm)){rcm<-rcm_download(include_validated=T)}
  if(is.null(subs)){subs<-subs_download()}

  if(update_rcm){
  message(crayon::black("updating matrix with any new submissions.."))
  rcm_update_from_subs(subs,rcm)
  }


  rcm<-rcm[rcm$unit=="data",]
  todo<-todo_create(rcm,subs)

  delayed<-todo_delayed(rcm)
  inconsistent<-rcm_check(rcm)

  inconsistent<-inconsistent %>% filter(
    !(issue %in% c("planned submission passed but no received date", "data unit item with non-standardisable status"))
  )

  country_status_markdown(rcm = rcm,delayed = delayed,inconsistencies = inconsistent)

  # summary_comments<-paste(rcm$rcid[has_comment],": ",rcm$comment[has_comment])

}



















