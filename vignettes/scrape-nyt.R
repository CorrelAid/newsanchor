## ----load packages-------------------------------------------------------
# load all required packages
library(newsanchor) # download newspaper articles
library(robotstxt)  # get robots.txt
library(httr)       # http requests
library(rvest)      # web scraping tools
library(dplyr)      # easy data frame manipulations
library(stringr)    # string/character manipulations 
library(tidytext)   # tidy text analysis

## ----get meta data, eval=FALSE-------------------------------------------
#  # get heaadlines published by spiegel online
#  response <- get_everything(query   = "Trump",
#                             sources = "the-new-york-times",
#                             from    = "2018-12-03",
#                             to      = "2018-12-09")
#  
#  # extract response data frame
#  articles <- response$results_df

## ----load example data set-----------------------------------------------
articles <- sample_response$results_df

## ----plot results--------------------------------------------------------
plot(sentiment ~ date,
     data = articles_sentiment)

