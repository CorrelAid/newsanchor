## ------------------------------------------------------------------------
library(newsanchor) # download newspaper articles
library(rvest)      # web scraping tools
library(magrittr)   # pipes
library(stringr)    # string/character manipulations

## ------------------------------------------------------------------------
# get heaadlines published by spiegel online
response <- get_headlines(source = "spiegel-online") 

## ------------------------------------------------------------------------
get_article_body <- function (url) {
  # download and parse website
  html <- read_html(url)                    
  
  # parse content
  html %>% 
    html_nodes("div.article-section p") %>% # extract all paragraphs within class 'article-section'
    html_text() %>%                         # extract content of the p tags
    str_replace_all("\n", "") %>%           # replace all line breaks
    paste(collapse = " ")                   # join all paragraphs into one string
}


## ------------------------------------------------------------------------
# apply function to the given URLs
response$results_df$body <- sapply(response$results_df$url, get_article_body)

