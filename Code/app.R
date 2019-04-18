library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)

# Global
full_txt <- read_csv("https://raw.githubusercontent.com/Glacieus/STAT-612-Final-Project/master/Data/full_txt.csv")

sources <<- list("American Political Science Association" = "APSA", "Political Science Quarterly" = "PSQ")
sources2 <<- list("Political Science Quarterly" = "PSQ", "American Political Science Association" = "APSA")

minyear <<- 2013:2018
maxyear <<- 2013:2018


# Cache the results
getTermMatrix <- memoise(function(sources, minyear = 2000, maxyear = 2014){
  if(missing(minyear)){
    text <- full_txt %>% 
      filter(source == sources, year >= 2000, year <= maxyear) %>% 
      select(text)
  }
  if(missing(maxyear)){
    text <- full_txt %>% 
      filter(source == sources, year >= minyear, year <= 2018) %>% 
      select(text)
  }
  else{
    text <- full_txt %>%
      filter(source == sources, year >= minyear, year <= maxyear) %>% 
      select(text)}
  
  myCorpus = Corpus(VectorSource(text))
  
  myDTM = TermDocumentMatrix(
    myCorpus,
    control = list(
      minWordLength = 1,
      wordLengths = c(0, Inf),
      removePunctuation = TRUE,
      removeNumbers = TRUE, stopwords = TRUE
    ))
  
  m = as.matrix(myDTM)
  
  sort(rowSums(m), decreasing = T)
})


get_comp_comm <- function(sources, sources2, minyear = 2013, maxyear = 2018){

    cloud1 <- full_txt %>% 
      filter(source == sources, year >= minyear, year <= maxyear) %>% 
      select(text)
    
    cloud2 <- full_txt %>% 
      filter(source == sources2, year >= minyear, year <= maxyear) %>% 
      select(text)
    
    pt1 <- Corpus(VectorSource(cloud1))
    pt2 <- Corpus(VectorSource(cloud2))
    
    myDTM1 = TermDocumentMatrix(
      pt1,
      control = list(
        minWordLength = 1,
        wordLengths = c(0, Inf),
        removePunctuation = TRUE,
        removeNumbers = TRUE, stopwords = TRUE
      ))
    
    myDTM2 = TermDocumentMatrix(
      pt2,
      control = list(
        minWordLength = 1,
        wordLengths = c(0, Inf),
        removePunctuation = TRUE,
        removeNumbers = TRUE, stopwords = TRUE
      ))
    
    tdm1 <- myDTM1 %>% 
      as.matrix() 
    
    tdm2 <- myDTM2 %>% 
      as.matrix()
    
    tdm10 <- sort(rowSums(tdm1), decreasing = T)
    tdm20 <- sort(rowSums(tdm2), decreasing = T)
    
    df1 <- rownames_to_column(as.data.frame(tdm10))
    df2 <- rownames_to_column(as.data.frame(tdm20))
    
    full_df <- full_join(df1, df2) %>% 
      mutate(tdm10 = replace_na(tdm10, 0)) %>% 
      mutate(tdm20 = replace_na(tdm20, 0))
    
    tdm <- as.matrix(column_to_rownames(full_df))
    
    name1 <- paste(sources, sep = "-")
    name2 <- paste(sources2, sep = "-")
    
    colnames(tdm) <- c(name1, name2)
    #par(mfrow = c(1, 2))
    tdm
}



ui <- navbarPage(
  "Journal Articles Wordcloud",
  theme = shinytheme("superhero"),
  tabPanel(
    "Wordcloud",
    sidebarPanel(
        selectInput("selection", "Choose a Journal:",
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
          value = 100)
      ),
    mainPanel(
      plotOutput('plot', width = "auto", height = "auto")
    )),
  tabPanel("Comparison Wordclouds",
    sidebarPanel(
      selectInput("selection3", "Choose Journal 1",
                  choices = sources),
      hr(),
      selectInput("selection4", "Choose Journal 2",
                  choices = sources2),
      hr(),
      offset = 1,
      sliderInput("yrs2", "Years", 2013, 2018, value = c(2013, 2014)),
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
      )
    ),
    mainPanel(
      plotOutput('plot2', width = "auto", height = "600px"))
  )
)


server <- function(input, output, session) {
  # Define a reactive expression for the document term matrix
  terms <- reactive({
    minyear <- input$yrs[1]
    maxyear <- input$yrs[2]
    req(input$selection)
    req(input$yrs[1])
    req(input$yrs[2])
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection, minyear, maxyear)
      })
    })
  })
  
  terms2 <- reactive({
    req(input$selection3)
    req(input$selection4)
    minyear <- input$yrs2[1]
    maxyear <- input$yrs2[2]
    req(input$yrs2[1])
    req(input$yrs2[2])
    input$update
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        get_comp_comm(input$selection3, input$selection4, minyear, maxyear)
      })
    })
  })
  
  
  # Make the wordcloud drawing predictable during a session
  wordcloud_rep <- repeatable(wordcloud)
  wordcloud_comp_rep <- repeatable(comparison.cloud)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(
      names(v),
      v,
      scale = c(7, 2),
      min.freq = input$freq,
      max.words = input$max,
      colors = brewer.pal(15, "Set2")
    )
  }, height = 700)
  
  
  output$plot2 <- renderPlot({
    v2 <- terms2()
    wordcloud_comp_rep(
      v2,
      scale = c(5, 1),
      min.freq = input$freq2,
      max.words = input$max2,
      #title.size = 1.5,
      colors = c("#6600CC", "red")
    )
  }, height = 525)
}

shinyApp(ui, server)




