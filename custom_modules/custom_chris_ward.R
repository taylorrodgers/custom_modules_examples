library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(stevedata)
library(tidyverse)
library(ggthemes)
library(forcats)

ChrisWardSchoolPerformanceUI <- function(id) {
  
  div(class="ui padded segment",
      h3("Chris Ward County's Worst Schools by API"),
      div(class="ui horizontal segments",
          div(class="ui segment",
              h3(class="ui dividing sub header","All"),
              plotOutput(NS(id,"BottomAll"),height = "275px")
              ),
          div(class="ui segment",
              h3(class="ui dividing sub header","Urban"),
              plotOutput(NS(id,"BottomUrban"),height = "275px")
              ),
          div(class="ui segment",
              h3(class="ui dividing sub header","Suburban"),
              plotOutput(NS(id,"BottomSuburban"),height = "275px")
              ),
          div(class="ui segment",
              h3(class="ui dividing sub header","Rural"),
              plotOutput(NS(id,"BottomRural"),height = "275px")
              )
      )
  )
}

ChrisWardSchoolPerformanceServer <- function(id,dat) {
  moduleServer(id, function(input, output, session) {
    
    ChrisWardData <- reactive({
      ChrisWardData <- dat() %>% 
        filter(county=="Chris Ward") %>%
        mutate(api=round(api,0)) %>%
        arrange(api)
    })

    output$BottomAll <- renderPlot({
      
      FilteredData <- ChrisWardData()[1:5,]

      ggplot(FilteredData,aes(x=api,y=fct_rev(fct_reorder(uid,api)))) +
        geom_col(fill="#00b5ad") +
        geom_text(aes(label=api),color="white",size=4.25,hjust=1.5) +
        theme_pander() +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title=element_blank()) +
        labs(title="") 
      
    })
    
    output$BottomUrban <- renderPlot({
      
      FilteredData <- ChrisWardData() %>%
        filter(community=="Urban") %>%
        .[1:5,]
      
      ggplot(FilteredData,aes(x=api,y=fct_rev(fct_reorder(uid,api)))) +
        geom_col(fill="#00b5ad") +
        geom_text(aes(label=api),color="white",size=4.25,hjust=1.5) +
        theme_pander() +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title=element_blank()) +
        labs(title="") 
      
    })
    
    output$BottomSuburban <- renderPlot({
      
      FilteredData <- ChrisWardData() %>%
        filter(community=="Suburban") %>%
        .[1:5,]
      
      ggplot(FilteredData,aes(x=api,y=fct_rev(fct_reorder(uid,api)))) +
        geom_col(fill="#00b5ad") +
        geom_text(aes(label=api),color="white",size=4.25,hjust=1.5) +
        theme_pander() +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title=element_blank()) +
        labs(title="") 
      
    })
    
    output$BottomRural <- renderPlot({
      
      FilteredData <- ChrisWardData() %>%
        filter(community=="Rural") %>%
        .[1:5,]
      
      ggplot(FilteredData,aes(x=api,y=fct_rev(fct_reorder(uid,api)))) +
        geom_col(fill="#00b5ad") +
        geom_text(aes(label=api),color="white",size=4.25,hjust=1.5) +
        theme_pander() +
        theme(axis.text.x=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              plot.title=element_blank()) +
        labs(title="") 
      
    })
    
  })
}

ChrisWardSchoolPerformanceApp <- function() {
  ui <- semanticPage(
    div(class="ui stackable grid",
        div(class="eight wide column",
            ChrisWardSchoolPerformanceUI("test")
        )
    )
  )
  server <- function(input, output, session) {
    ChrisWardSchoolPerformanceServer("test",dat=reactive({Dat}))
  }
  shinyApp(ui, server)  
}

ChrisWardSchoolPerformanceApp()