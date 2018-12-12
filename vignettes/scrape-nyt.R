## ----load packages, eval=FALSE-------------------------------------------
#  library(newsanchor) # download newspaper articles
#  library(httr)       # http requests
#  library(rvest)      # web scraping tools
#  library(dplyr)      # pipes
#  library(stringr)    # string/character manipulations
#  library(tidytext)   # tidy text analysis

## ----get meta data, eval=FALSE-------------------------------------------
#  # get heaadlines published by spiegel online
#  response <- get_everything(query   = "Trump",
#                             sources = "the-new-york-times",
#                             from    = "2018-10-29",
#                             to      = "2018-11-04")
#  
#  # extract response data frame
#  articles <- response$results_df

## ----define parsing function, eval=FALSE---------------------------------
#  get_article_body <- function (url) {
#  
#    # download article page
#    response <- GET(url)
#  
#    # check if request was successfull
#    if (response$status_code != 200) return(NA)
#  
#    # extract html
#    html <- content(x        = response,
#                    type     = "text",
#                    encoding = "UTF-8")
#  
#    # parse html
#    parsed_html <- read_html(html)
#  
#    # define paragraph DOM selector
#    selector <- "article#story div.StoryBodyCompanionColumn div p"
#  
#    # parse content
#    parsed_html %>%
#      html_nodes(selector) %>%      # extract all paragraphs within class 'article-section'
#      html_text() %>%               # extract content of the p tags
#      str_replace_all("\n", "") %>% # replace all line breaks
#      paste(collapse = " ")         # join all paragraphs into one string
#  }
#  

## ----apply function to urls, eval=FALSE----------------------------------
#  # create new text column
#  articles$body <- NA
#  
#  # initialize progress bar
#  pb <- txtProgressBar(min     = 1,
#                       max     = nrow(articles),
#                       initial = 1,
#                       style   = 3)
#  
#  # loop through articles and "apply" function
#  for (i in 1:nrow(articles)) {
#    # "apply" function to i url
#    articles$body[i] <- get_article_body(articles$url[i])
#  
#    # update progress bar
#    setTxtProgressBar(pb, i)
#  
#    # sleep for 1 sec
#    Sys.sleep(1)
#  }

## ----calculate sentiment, eval=FALSE-------------------------------------
#  articles_sentiment <- articles %>%
#    select(url, body) %>%                                     # extract required columns
#    unnest_tokens(word, body) %>%                             # split each article into single word
#    anti_join(get_stopwords(), by = "word") %>%               # remove stopwords
#    inner_join(get_sentiments("afinn"), by = "word") %>%      # join sentiment scores
#    group_by(url) %>%                                         # group text again by their url
#    summarise(sentiment = sum(score)) %>%                     # sum up sentiment scores
#    left_join(articles, by = "url") %>%                       # add sentiment column to articles
#    select(published_at, sentiment) %>%                       # extract required columns
#    group_by(date = as.Date(published_at, "%Y-%m-%d")) %>%  # group by date
#    summarise(sentiment = sum(sentiment))

## ----plot results, eval=FALSE--------------------------------------------
#  plot(sentiment ~ date,
#       data = articles_sentiment)

