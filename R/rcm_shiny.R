rcm_add_validation_button<-function(rcm){
  click_to_validate<-shinyInput(actionButton, nrow(rcm), paste0('valbutton_',rcm$file.id), label = "validate"
                                , onclick = 'Shiny.onInputChange(\"file_id_validated\",  this.id)' ,
                                style="background-color:#BB0000;color:white;",
                                onMouseOver="this.style.background-color='#FF0000'",
                                onMouseOut="this.style.background-color='#BB0000'")
  rcm<-data.frame(`change state`=click_to_validate,rcm)
}




shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(id[i], ...))
  }
  inputs
}



to_shiny_link<-function(href,title){
  paste0("<a href=",href,">",title,"</a>")
}
