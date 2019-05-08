library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)
library(igraph)
library(ggraph)
library(ggthemes)
library(shinydashboard)
library(kableExtra)
library(plotly)
library(lubridate)


dashboardPage(
  dashboardHeader(title = "Journal Texts"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Wordcloud", 
               tabName = "Wordcloud"),
      menuItem("Comparison Cloud", 
               tabName = "CC"),
      menuItem("Trigrams",
               tabName = "Trigrams"),
      menuItem("Graphs",
               tabName = "Graphs")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Wordcloud",
              fluidRow(
                box(width = 4, title = "Inputs", selectInput("selection", "Choose a Journal:",
                                                             choices = sources),
                    actionButton("update", "Change"),
                    hr(),
                    
                    offset = 1,
                    sliderInput("yrs", "Years", 2013, 2018, value = c(2013, 2014)),
                    sliderInput(
                      "freq",
                      "Min Freq:",
                      min = 1,
                      max = 50,
                      value = 15),
                    
                    sliderInput(
                      "max",
                      "Max Number of Words:",
                      min = 1,
                      max = 300,
                      value = 100)),
                box(width = 8, height = 750, plotOutput("plot", width = "100%"))
              )),
      tabItem(tabName = "CC",
              fluidRow(
                box(width = 4, title = "Inputs", selectInput("selection3", "Choose Journal 1",
                                                             choices = sources),
                    hr(),
                    selectInput("selection4", "Choose Journal 2",
                                choices = sources2),
                    hr(),
                    offset = 1,
                    sliderInput("yrs2", "Years", 2010, 2019, value = c(2016, 2018)),
                    sliderInput(
                      "freq2",
                      "Min Freq:",
                      min = 1,
                      max = 50,
                      value = 15
                    ),
                    sliderInput(
                      "max2",
                      "Max Number of Words:",
                      min = 1,
                      max = 300,
                      value = 100
                    )),
                box(width = 8, height = 750, plotOutput("plot2", width = "90%"))
              )),     
      tabItem(tabName = "Trigrams",
              fluidRow(
                box(width = 4, title = "Inputs", selectInput("selection5", "Choose Journal:", choices = sources2),
                    
                    sliderInput(inputId = "number",
                                label = "Choose Frequency",
                                min = 5, max = 150, value = 20),
                    
                    #  sliderInput("yrs3", "Years", 2003, 2019, value = c(2013, 2014)),
                    actionButton(inputId = "click", label = "Update")
                ),
                box(width = 8, height = 750, plotOutput("ggraph", width = "100%"),
                    tableOutput("stats"))
              )),
      tabItem(tabName = "Graphs",
              fluidRow(
                box(width = 12, plotlyOutput("sentiment_time")),
                box(width = 12, plotlyOutput("percentile_time")),
                box(width = 12, plotlyOutput("altmetric_score")),
                box(width = 12, plotlyOutput("tweets"))
              ))
    ))
)