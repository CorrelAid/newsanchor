## ----load packages, warning=FALSE, message=FALSE-------------------------
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
#  response <- get_everything_all(query   = "Trump",
#                                 sources = "the-new-york-times",
#                                 from    = "2018-12-03",
#                                 to      = "2018-12-09")
#  
#  # extract response data frame
#  articles <- response$results_df

## ----load example data set-----------------------------------------------
articles <- sample_response$results_df

## ----plot results, dev='svg', fig.align='center', fig.width=7.3, fig.height=4.3----
# enable two plots in one figure
old_par <- par(mfrow=c(1, 2))

# plot number of articles vs time 
barplot(height    = sentiment_by_day$n,
        names.arg = format(sentiment_by_day$date, "%a"),
        ylab      = "# of articles",
        ylim      = c(-10, 35),
        las       = 2)

# plot sentiment score vs time
barplot(height    = sentiment_by_day$sentiment,
        names.arg = format(sentiment_by_day$date, "%a"),
        ylab      = "Sentiment Score",
        ylim      = c(-10, 35),
        las       = 2)

