#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
   # Application title
   title=("RCM"),
      header=mainPanel(selectInput("unit", "Unit:",
               c( "all" = "all",
                  "research design" = "research design",
                 "data" = "data",
                 "reporting" = "reporting",
                 "GIS"="GIS",
                 "other"="other"
                ),selected = "all"))
   ,
   tabPanel("Overview",mainPanel(plotOutput("timeline"))),

   tabPanel("All",


            # Show a plot of the generated distribution
            mainPanel(
              # titlePanel("Overview"),
              # dataTableOutput('issuetableoverview'),
              # titlePanel("All inconsistencies"),
              dataTableOutput('completetable')
              #

            )),
   tabPanel("Inconsistencies",


      # Show a plot of the generated distribution
      mainPanel(
        # titlePanel("Overview"),
        # dataTableOutput('issuetableoverview'),
        # titlePanel("All inconsistencies"),
        dataTableOutput('issuetable')
         #

      )),
   tabPanel("Longest with HQ",
   mainPanel(
     dataTableOutput('longestwithhq')

   )),
   tabPanel("Submission Due",
   mainPanel(
     dataTableOutput('expected')

   )),
   tabPanel("Milestone missed",
            mainPanel(
              dataTableOutput('passedmilestone')

            )),
   tabPanel("useless gant chart",
            mainPanel(
              plotOutput("distPlot",width = "100%",height=paste0(1000*5,"px"))

            ))
   ))


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  library("knitr")
  library("dplyr")
  library("knitr")
  library("tidyr")
  library("magrittr")
  print(getwd())
  sourcepath<-"../R/"
  sourcefiles<-paste0(sourcepath,list.files(sourcepath))
  sapply(sourcefiles,source)




  rcm<-rcm_download(include_archived = F,include_validated = F,after_year = "2015")
  rcm_unit_subset<-function(rcm,unit){
    if(unit=="all"){return(rcm)}
    rcm[rcm$unit==input$unit,]
  }

  output$completetable<-renderDataTable({rcm},)

  output$issuetable <- renderDataTable({rcm_check(rcm_unit_subset(rcm,input$unit))},escape = F)
  output$issuetableoverview<-renderDataTable({
    issues<-rcm_check(rcm_unit_subset(rcm,input$unit))
    issues_overview<-table(list(issues$issue,rcm_unit(rcm[issues$id,]))) %>% as.data.frame() %>% spread("X.2",value = "Freq")
    issues_overview<-issues_overview %>% rename(`issue type`=X.1)
    issues_overview
    },escape = F)



  output$longestwithhq<-renderDataTable({

    subs<-rcm_unit_subset(rcm,input$unit)
    subs<-rcm_add_validation_button(subs)
    subs<-rcm_longest_with_hq(subs,n=50,add.columns = "change.state")
      subs
    },escape = F)



  output$expected<-renderDataTable({rcm_submission_expected(rcm_unit_subset(rcm,input$unit))},escape = F)
  output$passedmilestone<-renderDataTable({
    rcm[which(rcm_passed_milestone(rcm) & !rcm$archived & !grepl("validated",rcm$status)),c("link","rcid","date.milestone","status")]
      },escape=F
    )
  output$distPlot <- renderPlot({

     # draw the histogram with the specified number of bins
     rcm_gant(rcm_unit_subset(rcm,input$unit))
  },height = 10000)

  output$timeline<-renderPlot({validation_timeline(rcm)},height = 300)


  observeEvent(input$file_id_validated, {
    rcm_set_to_validated(gsub("^valbutton_","",input$file_id_validated))
    # print(input$file_id_validated)
  })

  }

# Run the application
shinyApp(ui = ui, server = server)

