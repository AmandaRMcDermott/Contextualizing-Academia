library(shiny)
library(tm)
library(wordcloud)
library(memoise)
library(tidyverse)
library(tidytext)
library(shinythemes)
library(readr)
library(ggthemes)
library(ggraph)
library(igraph)

# Global
full_txt <- read_csv("https://raw.githubusercontent.com/Glacieus/DataSci/master/Data/full_txt.csv")

full_txt <- full_txt %>% 
  separate(date, into = c("year", "delete", "delete2"), sep = "-") %>% 
  select(year, text, source)

sources <<- list("American Political Science Association" = "APSA", "Political Science Quarterly" = "PSQ", "Political Analysis" = "PA")
sources2 <<- list("Political Science Quarterly" = "PSQ", "American Political Science Association" = "APSA", "Political Analysis" = "PA")

minyear <<- 2013:2019
maxyear <<- 2013:2019
year <<- 2013:2019

# define arrows for network of words
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

# Cache the results
getTermMatrix <- memoise(function(sources, minyear = 2000, maxyear = 2019){
  if(missing(minyear)){
    text <- full_txt %>% 
      filter(source == sources, year >= 2000, year <= maxyear) %>% 
      select(text)
  }
  if(missing(maxyear)){
    text <- full_txt %>% 
      filter(source == sources, year >= minyear, year <= 2019) %>% 
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

# my stopwords
my_stopwords <- tibble(word = c("stix", "1", "2", "3", "4", "0", "5", "x1d6fc", "e.g", "al", "6", "x_", "10", "ij", "x1d6fd", "i.e", "y_", "7", "8", "9", "10", "11", "12", "gd", "20", "tq", "13", "14","15","16","17","18","19","20", "x1d702", "x170e", "ast", "x1d707", "mathbf", "unicode"))

# break full_txt down into trigrams
txt_words <- full_txt %>% 
  mutate(document = 1:nrow(full_txt)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  anti_join(my_stopwords, by = "word") %>% 
  nest(word) %>% 
  mutate(text = map(data, unlist),
         text = map_chr(text, paste, collapse = " ")) %>% 
  select(year, source, text) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

# word network filtering function
filtering <- function(source, year, number){
  txt_words %>% 
    filter(source == source, year == year) %>% 
    count(word1, word2, word3, sort = T) %>% 
    filter(n > number)
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
        sliderInput("yrs", "Years", 2013, 2019, value = c(2017, 2018)),
        sliderInput(
          "freq",
          "Min Freq:",
          min = 1,
          max = 1000,
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
      sliderInput("yrs2", "Years", 2013, 2019, value = c(2017, 2018)),
      sliderInput(
        "freq2",
        "Min Freq:",
        min = 1,
        max = 1000,
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
  ),
  tabPanel("Word Network",
           sidebarPanel(
             selectInput("selection5",
                         label = "Choose a Journal:", 
                         choices = sources),
             
             sliderInput(inputId = "number",
                         label = "Choose Frequency",
                         min = 5, max = 40, value = 5),
             
             sliderInput("yr", "Year", 2013, 2019, value = 2018),
             
             actionButton(inputId = "click",
                          label = "Update")
           ),
           mainPanel(
             plotOutput("ggraph")
           ))
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
  
  color_palette <- colorRampPalette(brewer.pal(15, "Paired"))(25)
  
  output$plot <- renderPlot({
    v <- terms()
    wordcloud_rep(
      names(v),
      v,
      scale = c(10, 1.5),
      min.freq = input$freq,
      max.words = input$max,
      colors = color_palette
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
      colors = c("#ef8a62", "#67a9cf")
    )
  }, height = 525)
  
  ev <- eventReactive(input$click, {
    req(input$selection5)
    req(input$yr)
    req(input$number)
    input$update
    filtering(input$selection5, input$yr, input$number)
  })
  
  ev_2 <- eventReactive(input$click, {
    input$update
    graph_from_data_frame(ev())
  })
  
  output$ggraph <- renderPlot(
    ggplot(ev_2(), layout = "fr") +
      geom_edge_link(arrow = a, alpha = 0.5) +
      geom_node_point() +
      geom_node_text(aes(label = name),
                     vjust = 1,
                     hjust = 1,
                     repel = T) +
      theme_economist() +
      theme(
        legend.position = "none", 
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color = "black", size = 0)
      )
  )
}

shinyApp(ui, server)




