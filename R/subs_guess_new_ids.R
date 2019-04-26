

subs_new_ids<-function(subs){
  invisible(subs[subs$file.id.new,])
}

str_special_chars_to_space<-  function(x){gsub("[^A-Za-z0-9]"," ",x)}

subs_new_ids_guess_next<-function(subs,rcm,n=3,search.also.for=""){
  new<-subs_new_ids(subs)
  pattern<-str_special_chars_to_space("YEM1806_ Yemen WASH Cluster Assessment_March_Analysis_2")
  pattern<-paste(pattern,search.also.for)
  found<-rcm_find(rcm,pattern,n)
  found$file.id
  display_single_guess<-function(rcmrow){
    text<-paste("\n",
      paste(
        crayon::black(paste(rcmrow["rcid"])),
        crayon::blurred(crayon::cyan(rcmrow["file.id"])),
        crayon::silver(rcmrow["round"])
      ),
      crayon::black(),
      crayon::silver(crayon::italic(rcmrow["comment"])),
      sep="\n")
    text<-gsub("\n\n\n\n","\n",text,fixed = T)
    text<-gsub("\n\n\n","\n",text,fixed = T)
    text<-gsub("\n\n","\n",text,fixed = T)
    text
  }

  message(crayon::blurred(new$new.file.id[1]))
  apply(found,1,function(x){cat(display_single_guess(x))})
  invisible(found$file.id)
}




rcm_find<-function(rcm,search,n=5){

    search<-strsplit(search %>% tolower," ") %>% unlist
    found<-lapply(search,function(x){
      data.frame(in.rcid =  grepl(x,tolower(rcm$rcid)) %>% as.numeric*1000,
                 in.file.id= grepl(x,tolower(rcm$file.id)) %>% as.numeric*100,
                 in.round=grepl(x,tolower(rcm$round)) %>% as.numeric*10,
                 in.type = grepl(x,tolower(rcm$type)) %>% as.numeric*1
      )

    }) %>% do.call(cbind,.)

    matchiness<-data.frame(matchiness=rowSums(found),id=1:nrow(found))
    matchiness<-matchiness[matchiness$matchiness!=0,]
    matchiness<-matchiness[order(matchiness$matchiness,decreasing = T),]
    best_matches<-matchiness[1:n,"id"]
    matching<-rcm[best_matches,]
    return(matching)

}


