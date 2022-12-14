library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(stevedata)
library(tidyverse)
library(ggthemes)

MySchoolPerformanceUI <- function(id) {
  
  tagList(
    div(class="ui padded segment",style="min-width: 250px; max-width: 350px",
        selectInput(NS(id,"SelectMetric"),
                    label="Select Performance Metric",
                    choices=c("Academic Performance Index","Subsidized Meals","Parents w/College Degree","Fully Qualified Teachers"),
                    select="Academic Performance Index")
        ),
    div(class="ui padded segment",style="min-width: 250px; max-width: 350px",
        h3("My School's ",textOutput(NS(id,"MetricLabel"),inline=TRUE)),
        h3(class="ui grey sub header",textOutput(NS(id,"SchoolName"),inline=TRUE)),
        div(class="ui row",
            h1(class="ui header",textOutput(NS(id,"Metric"),inline=TRUE))),
        h3(class="ui dividing sub header",textOutput(NS(id,"MetricLabelCounty"))),
        plotOutput(NS(id,"CountyAverage"),height = "90px"),
        h3(class="ui dividing sub header",textOutput(NS(id,"MetricLabelSchoolType"))),
        plotOutput(NS(id,"SchoolTypeAverage"),height = "90px"),
        h3(class="ui dividing sub header",textOutput(NS(id,"MetricLabelCommnity"))),
        plotOutput(NS(id,"CommunityTypeAverage"),height = "90px")
    )    
  )  

}

MySchoolPerformanceServer <- function(id,data,countyfilter,uidfilter) {
  moduleServer(id, function(input, output, session) {

    PrincipalData <- reactive({
      
      MappedMetricInput <- c("Academic Performance Index"="api",
                          "Subsidized Meals"="meals",
                          "Parents w/College Degree"="colgrad",
                          "Fully Qualified Teachers"="fullqual")
      MappedMetricInput <- MappedMetricInput[input$SelectMetric]
      
      SchoolMetaData <- data() %>%
        filter(uid == uidfilter()) %>%
        select(schooltype,community)
      
      IndividualSchool <- data() %>%
        filter(uid == uidfilter()) %>%
        transmute(uid,api=round(api),
                  meals=round(meals),
                  colgrad=round(colgrad),
                  fullqual=round(fullqual))
      
      AvgSchoolInCounty <- data() %>%
        filter(county %in% countyfilter()) %>%
        summarize(api=round(mean(api)),
                  meals=round(mean(meals)),
                  colgrad=round(mean(colgrad)),
                  fullqual=round(mean(fullqual))) %>%
        transmute(uid="Avg. County School",api,meals,colgrad,fullqual)
      
      AvgSchoolBySchoolType <- data() %>%
        filter(county %in% countyfilter(),schooltype==SchoolMetaData$schooltype) %>%
        group_by(schooltype) %>%
        summarize(api=round(mean(api)),
                  meals=round(mean(meals)),
                  colgrad=round(mean(colgrad)),
                  fullqual=round(mean(fullqual))) %>%
        transmute(uid=paste0("Avg. ",SchoolMetaData$schooltype),api,meals,colgrad,fullqual)
      
      AvgSchoolByCommunityType <- data() %>%
        filter(county %in% countyfilter(),community==SchoolMetaData$community) %>%
        group_by(community) %>%
        summarize(api=round(mean(api)),
                  meals=round(mean(meals)),
                  colgrad=round(mean(colgrad)),
                  fullqual=round(mean(fullqual)))  %>%
        transmute(uid=paste0("Avg. ",SchoolMetaData$community),api,meals,colgrad,fullqual)
      
      list(MappedMetricInput=MappedMetricInput,
           SchoolMetaData=SchoolMetaData,
           IndividualSchool=IndividualSchool,
           AvgSchoolInCounty=AvgSchoolInCounty,
           AvgSchoolBySchoolType=AvgSchoolBySchoolType,
           AvgSchoolByCommunityType=AvgSchoolByCommunityType)
      
    })
    
    # h3(class="ui grey sub header",textOutput(NS(id,"SchoolName")),"'s ",textOutput(NS(id,"MetricLabel"))),
    # div(class="ui row",
    #     h1(class="ui header",textOutput(NS(id,"Metric"),inline=TRUE))),
    # 
    output$SchoolName <- renderText({
      uidfilter()
    })
    
    output$MetricLabel <- renderText({
      input$SelectMetric    
    })
    
    output$Metric <- renderText({
      PrincipalData()$IndividualSchool$api
    })
    
    output$MetricLabelCounty <- renderText({
      paste0(input$SelectMetric," vs County Average")
    })
    
    output$MetricLabelSchoolType <- renderText({
      paste0(input$SelectMetric," vs School Type Average")
    })
    
    output$MetricLabelCommnity <- renderText({
      paste0(input$SelectMetric," vs Community Type Average")
    })
    
    output$CountyAverage <- renderPlot({
      
      Dat <- rbind(PrincipalData()$IndividualSchool,PrincipalData()$AvgSchoolInCounty)
      Dat <- Dat %>% select(uid,matches(PrincipalData()$MappedMetricInput))
      colnames(Dat) <- c("uid","metric")
      MicroPlots(Dat,Dat$metric,Dat$uid)
      
    })
    
    output$SchoolTypeAverage <- renderPlot({
      
      Dat <- rbind(PrincipalData()$IndividualSchool,PrincipalData()$AvgSchoolBySchoolType)
      Dat <- Dat %>% select(uid,matches(PrincipalData()$MappedMetricInput))
      colnames(Dat) <- c("uid","metric")
      MicroPlots(Dat,Dat$metric,Dat$uid)
      
    })
    
    output$CommunityTypeAverage <- renderPlot({
      
      Dat <- rbind(PrincipalData()$IndividualSchool,PrincipalData()$AvgSchoolByCommunityType)
      Dat <- Dat %>% select(uid,matches(PrincipalData()$MappedMetricInput))
      colnames(Dat) <- c("uid","metric")
      MicroPlots(Dat,Dat$metric,Dat$uid)
      
    })
    
  })
}

MySchoolPerformanceApp <- function() {
  ui <- semanticPage(
    div(class="ui stackable grid",
        div(class="three wide column",
          MySchoolPerformanceUI("test")
        )
    )
  )
  server <- function(input, output, session) {
    MySchoolPerformanceServer("test",data=Dat,county="A.J. Hawk",uid="USD 32")
  }
  shinyApp(ui, server)  
}

MySchoolPerformanceApp()