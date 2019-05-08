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


# Global
full_txt <- read_csv("https://raw.githubusercontent.com/Glacieus/DataSci/master/Data/full_texts.csv")
#arm_poli_lexicon <- read_csv("https://raw.githubusercontent.com/Glacieus/DataSci/master/Data/arm_poli_lexicon.csv")
#poli_lexicon <- read_csv("https://raw.githubusercontent.com/Glacieus/DataSci/master/Data/poli_lexicon.csv")

#full_txt <- full_txt %>% sample_n(600)
full_texts <- full_txt

# Tidy and tokenize
full_txt <- full_txt %>% 
  separate(date, into = c("year", "month", "day")) %>% 
  select(-day, -month) %>% 
  mutate(year = as.numeric(year)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words)

my_stopwords <- tibble(word = c("stix", "1", "2", "3", "4", "0", "5", "x1d6fc", "e.g", "al", "6", "x_", "10", "ij", "x1d6fd", "i.e", "y_", "7", "8", "9", "10", "11", "12", "gd", "20", "tq", "13", "14","15","16","17","18","19","20", "x1d702", "x170e", "ast", "x1d707", "mathbf", "unicode", "au", "library", "libraryfind", "scholarfind", "eqnarray", "displaystyle", "web", "crossref", "journal", "political", "science", "association"))

full_txt <- full_txt %>% 
  anti_join(my_stopwords) %>% 
  unnest_tokens(text, word, token = "sentences")


sources <<- list("American Journal of Political Science" = "AJPS", "Political Science Quarterly" = "PSQ", "Political Analysis" = "PA", "American Political Science Association" = "APSA")
sources2 <<- list("Political Science Quarterly" = "PSQ", "American Journal of Political Science" = "AJPS", "Political Analysis" = "PA", "American Political Science Association" = "APSA")

minyear <<- 2000:2018
maxyear <<- 2013:2018


# define trigram functions
text_trigram <- full_txt %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>% 
  separate(trigram, c("word1", "word2", "word3"), sep = " ")

filtering <- function(sources,  number = 20){
  text_trigram %>% 
    filter(source == sources) %>% 
    count(word1, word2, word3, sort = T) %>% 
    filter(n > number)
}

# for plotly
#colnames(poli_lexicon) <- c("word", "sent_score")
#tidytexts <- full_texts %>% 
#  unnest_tokens(word, text) %>% 
#  left_join(arm_poli_lexicon, by = "word") %>% 
#  left_join(poli_lexicon, by = "word") %>% 
#  anti_join(stop_words) %>% 
#  anti_join(my_stopwords)




# define arrows
a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

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
