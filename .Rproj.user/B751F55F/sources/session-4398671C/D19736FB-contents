#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(stevedata)
library(tidyverse)
library(ggthemes)
library(reactable)
library(shinyjs)

source("./custom_modules/principal_view.R",local=TRUE)
source("./custom_modules/custom_chris_ward.R",local=TRUE)
source("./custom_modules/custom_spielman_tatum.R",local=TRUE)

### universal metrics
# avg api
# avg high school api
# avg elementary school api 
# alternatively, it could be suburban versus urban
# use kpi boxes plus bar graphs from mint.com as inspiration

### "administrator" view for individual counties
# worst performers by api with urban, suburban toggles
# worst performers based on fully qualified teachers
# worst performers based on meals 

### "school"-level view
# comparison of api versus the average
# comparison of meals versus the average
# comparison of full qualified teachers versus the average

Dat <- fakeAPI
Dat$originaluid <- Dat$uid
Dat$uid <- paste("USD",as.character(Dat$uid))
Dat$schooltype <- ifelse(Dat$schooltype=="E","Elementary",
                         ifelse(Dat$schooltype=="M","Middle School","High School"))
TopCounties <- Dat %>% group_by(county) %>% summarize(n=n()) %>% arrange(desc(n)) %>% .[1:15,]

MicroPlots <- function(dat,x,y) {
  ggplot(dat,aes(x=x,y=y)) +
    geom_col(fill="#00b5ad") +
    geom_text(aes(label=x),color="white",size=4.25,hjust=1.5) +
    theme_pander() +
    theme(axis.text.x=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          plot.title=element_blank()) +
    labs(title="")  
}
  
### Primary app
SchoolPerformanceApp <- function() {
  
  ui <- dashboardPage(
    dashboardHeader(left=h3(class="ui inverted light header","High School Performance"),
                    inverted=TRUE,
                    color="grey",
                    menu_button_label=NULL),
    dashboardSidebar(
      sidebarMenu(
        menuItem(tabName = "home", text = "Home", icon = icon("home")),
        menuItem(tabName = "another", text = "Info", icon = icon("info"))
      ),
      color="black",
      inverted=TRUE,
      visible=FALSE,
      class="ui secondary vertical menu"),
    dashboardBody(
      useShinyjs(),
      div(class="ui stackable grid",
          div(class="three wide column",
              div(class="ui padded segment",style="min-width: 175px; max-width: 350px",
                  selectInput("Position","Position",choices=c("Superintendent"=1,"Principal"=2),selected=1),
                  selectInput("SchoolCounty",
                              "School County",
                              choices=sort(unique(TopCounties$county))),
                  shinyjs::hidden(div(id="SchoolUIDDiv",selectInput("SchoolUID","School ID",choices=""))))
              ),
          div(class="three wide column",
              div(class="ui padded segment",style="min-width: 250px; max-width: 350px",
                  h3("Academic Performance Index (API)"),
                  h3(class="ui grey sub header",textOutput("CountyName")),
                  div(class="ui row",
                    h1(class="ui header",textOutput("CountyAPI"),inline=TRUE)),
                  h3(class="ui dividing sub header","API By School Type"),
                  plotOutput("CountySchoolTypeAPI",height = "125px"),
                  h3(class="ui dividing sub header","API By Community Type"),
                  plotOutput("CountyCommunityTypeAPI",height = "125px"))
              ),
          shinyjs::hidden(div(id="PrincipalDiv",class="three wide column",
              MySchoolPerformanceUI("Principals"))
          ),
          div(class="eight wide column",
              div(class = "ui very padded segment",
                  h3("Complete Performance Data"),
                  reactableOutput("Summary")
              ),
              shinyjs::hidden(div(id="SpielmanTatumDiv",SpielmanTatumPerformanceUI("SpielmanTatum"))),
              shinyjs::hidden(div(id="ChrisWardDiv",ChrisWardSchoolPerformanceUI("ChrisWard")))
          )
      )
      
    )
  )
  
  server <- function(input, output,session) {
    
    observeEvent(req(input$Position),ignoreInit = TRUE,{
      
      if (input$Position==2) {
        shinyjs::show("PrincipalDiv")
        shinyjs::show("SchoolUIDDiv")
        MySchoolPerformanceServer("Principals",data=reactive({Dat}),county=reactive({input$SchoolCounty}),uid=reactive({input$SchoolUID}))
      }
      
      else if (input$Position==1) {
        shinyjs::hide("PrincipalDiv")
        shinyjs::hide("SchoolUIDDiv")
      }
      
    })
    
    observeEvent(input$SchoolCounty,ignoreInit=TRUE,{
      
      if (input$SchoolCounty=="Chris Ward") {
        shinyjs::show("ChrisWardDiv")
        ChrisWardSchoolPerformanceServer("ChrisWard",reactive({Dat}))
      }
      
      else if (input$SchoolCounty!="Chris Ward") {
        shinyjs::hide("ChrisWardDiv")
      }
      
      if (input$SchoolCounty %in% c("Jack Tatum","Chris Spielman")) {
        shinyjs::show("SpielmanTatumDiv")
        SpielmanTatumPerformanceServer("SpielmanTatum",reactive({Dat}),reactive({input$SchoolCounty}))
      }
      
      else if (input$SchoolCounty !="Jack Tatum" & input$SchoolCounty !="Chris Spielman") {
        shinyjs::hide("SpielmanTatumDiv")
      }
      
    })

    observeEvent(input$SchoolCounty,{
      SchoolUIDs <- Dat %>%
        filter(county %in% c(input$SchoolCounty)) %>%
        select(uid)

      updateSelectInput(session,"SchoolUID",choices=SchoolUIDs$uid)

    })
    
    output$Summary <- renderReactable({
      
      SummaryTable <- Dat %>% 
        filter(county %in% c(input$SchoolCounty)) %>%
        arrange(originaluid) %>%
        select(uid,schooltype,community,api,meals,colgrad,fullqual) %>%
        rename(`School District`=uid,
               `School Type`=schooltype,
               Community=community,
               `Academic Performance Index`=api,
               `% of Students Eligable for Meals`=meals,
               `% of Parents w/ College Degree`=colgrad,
               `% of Fully Qualified Teachers`=fullqual)
      
      reactable(SummaryTable,
                defaultPageSize = 5,
                striped=TRUE,
                columns=list(
                  `Academic Performance Index`=colDef(format=colFormat(digits=2)),
                  `% of Students Eligable for Meals`=colDef(format=colFormat(digits=2)),
                  `% of Parents w/ College Degree`=colDef(format=colFormat(digits=2)),
                  `% of Fully Qualified Teachers`=colDef(format=colFormat(digits=2))
                )
      )
      
    })
    
    output$CountyName <- renderText({
      paste0(input$SchoolCounty," County")
    })
    
    output$CountyAPI <- renderText({
      Dat <-  Dat %>% 
        filter(county %in% c(input$SchoolCounty)) %>%
        summarize(Average=mean(api))
      round(Dat$Average)
    })
    
    output$CountySchoolTypeAPI <- renderPlot({
      
      CountyAPI <- Dat %>% 
        filter(county %in% c(input$SchoolCounty)) %>%
        group_by(schooltype) %>%
        summarize(api=round(mean(api)))
      
      NewOrder <- c("High School","Middle School","Elementary")
      CountyAPI$schooltype <- factor(CountyAPI$schooltype,levels=NewOrder)
      CountyAPI <- CountyAPI[order(CountyAPI$schooltype),]
      
      MicroPlots(CountyAPI,CountyAPI$api,CountyAPI$schooltype)
      
    })
    
    output$CountyCommunityTypeAPI <- renderPlot({
      
      CountyAPI <- Dat %>% 
        filter(county %in% c(input$SchoolCounty)) %>%
        group_by(community) %>%
        summarize(api=round(mean(api)))
      
      MicroPlots(CountyAPI,CountyAPI$api,CountyAPI$community)
      
    })

  }
  
  shinyApp(ui, server)
}

SchoolPerformanceApp()
