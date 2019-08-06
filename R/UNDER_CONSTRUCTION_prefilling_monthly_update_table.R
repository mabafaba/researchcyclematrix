#
# rcm<-rcm_download(include_archived = FALSE,include_validated = FALSE,gdrive_links = FALSE)
#
#
#
# issues <- rcm %>% rcm_check
#
# issues <-issues %>%
#   filter(grepl("planned submission passed but no received date|none of the hq submission dates", issue))
#
#
#
# rcm$`HQ Update Request`<- issues[match(rcm$file.id,issues$`file id`),"issue"]
# rcm$`HQ Update Request`<-as.character(rcm$`HQ Update Request`)
# rcm$`HQ Update Request`[is.na(rcm$`HQ Update Request`)]<- ""
# rcm$`Country`<-substr(rcm$rcid,1,3)
# rcm$`Research Cycle`<-""
# rcm$`Project Code`<- ""
# rcm %>% filter(!is.na(file.id) & file.id !="") %>% as_tibble %>% select(Country,
#                              Unit = unit,
#                              `Research Cycle`,
#                              `R. Cycle ID`  = rcid,
#                              `Project Code`,
#                              `Item Type` = type,
#                             `output item id` = file.id,
#                             `HQ Update Request`
# ) %>% arrange(Country, `Unit`, `Item Type`,`Research Cycle`) %>%  write.csv("all_ongoing_items.csv")
#
# browseURL("all_ongoing_items.csv")
# rcnrcm$type %>% unique %>% write.csv("../../misc/research_tracker_types.csv")
# browseURL('../../misc/research_tracker_types.csv')
# rcm %>% filter()
#
# bra_issues$`file id`
