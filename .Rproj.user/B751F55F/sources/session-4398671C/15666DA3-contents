library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(stevedata)
library(tidyverse)
library(ggthemes)
library(forcats)

SpielmanTatumPerformanceUI <- function(id) {
  
  div(class="ui padded segment",style="min-width: 250px; max-width: 400px",
      h3(textOutput(NS(id,"CountyName"),inline=TRUE)," Subsidized Meal Comparison by Community Type"),
      plotOutput(NS(id,"Boxplot"),height="125px")
  )
}

SpielmanTatumPerformanceServer <- function(id,dat,countyselected) {
  moduleServer(id, function(input, output, session) {
    
    CountyData <- reactive({
      dat() %>%
        filter(county %in% countyselected()) %>%
        mutate(community=ifelse(community=="Urban","Urban","NonUrban")) %>%
        rename(County=county,Community=community,Meals=meals)
    })
    
    output$CountyName <- renderText({
      countyselected()
    })

    output$Boxplot <- renderPlot({

      ggplot(CountyData(),aes(x=Meals,y=Community,fill=Community)) +
        geom_boxplot(show.legend = FALSE) +
        theme_pander() +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title=element_blank()) +
        labs(title="") 
      
    })
    
    
  })
}

SpielmanTatumPerformanceApp <- function() {
  ui <- semanticPage(
    div(class="ui stackable grid",
        div(class="eight wide column",
            SpielmanTatumPerformanceUI("test")
        )
    )
  )
  server <- function(input, output, session) {
    SpielmanTatumPerformanceServer("test",dat=reactive({Dat}),"Jack Tatum")
  }
  shinyApp(ui, server)  
}

SpielmanTatumPerformanceApp()