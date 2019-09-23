

rcm_under_review_with_data_unit_html<-function(){
rcm<-researchcyclematrix::rcm_download()
sink("validations_under_review_with_data_unit.html")
cat("items with hq: ")
rcm %>% filter(unit=="data") %>%
  filter(grepl("hq|partner|field",status,ignore.case = T)) %>%
  dplyr::select(rcid,file.id,comment,date.hqsubmission.actual,status) %>%
  mutate(comment = gsub("\n","<br>",comment)) %>%
  dplyr::arrange(date.hqsubmission.actual,rcid,status)%>% group_by(status) %>% summarise(count = n()) %>%( knitr::kable) %>% cat

cat("<br><br><br>")


rcm %>% filter(unit=="data") %>%
  filter(grepl("hq|partner|field",status,ignore.case = T)) %>%
  dplyr::select(rcid,file.id,comment,date.hqsubmission.actual,status) %>%
  mutate(comment = gsub("\n","<br>",comment)) %>%
  dplyr::arrange(date.hqsubmission.actual,rcid,status) %>% knitr::kable(format = "html",escape = FALSE) %>% kableExtra::kable_styling()%>% cat

cat("
<style>
tr:nth-child(even) {
  background-color: #EEEEEE
}
</style>
")


sink()
browseURL("validations_under_review_with_data_unit.html")
browseURL(getwd())

}
