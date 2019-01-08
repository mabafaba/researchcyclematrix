


# rcm_premature_validation<-function(rcm){
#   rcm$validation.depends.data<-rcm$type %in% c("report","factsheet","presentation","situation overview")
#
#   rcm %>% split.data.frame(paste(rcm$rcid,rcm$round)) %>% function(x){
#     rcm$type %>% table
#   }
#
# }

#' sort RCM by priority
#' @param rcm the research cycle matrix from rcm_download(raw=F)
#' @return the research cycle matrix sorted by 1. milestone passed, 2. days with HQ
#' @export
rcm_sort_priority<-function(rcm){
alpha<-rcm_passed_milestone(rcm) %>% as.numeric
beta<-rcm_days_with_hq(rcm) %>% as.numeric
alpha[is.na(alpha)]<-0
beta[is.na(beta)]<-0
alpha = alpha*10e5
}










