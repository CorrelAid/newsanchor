## ----load packages, warning=FALSE, message=FALSE-------------------------
# load all required packages
library(newsanchor) # download newspaper articles
library(robotstxt)  # get robots.txt
library(httr)       # http requests
library(rvest)      # web scraping tools
library(dplyr)      # easy data frame manipulation
library(stringr)    # string/character manipulation 
library(tidytext)   # tidy text analysis
library(textdata)   # contains the AFINN lexicon 

## ----get meta data, eval=FALSE-------------------------------------------
#  # get headlines published by the NYT
#  response <- get_everything_all(query   = "Trump",
#                                 sources = "the-new-york-times",
#                                 from    = "2018-12-03",
#                                 to      = "2018-12-09")
#  
#  # extract response data frame
#  articles <- response$results_df

## ----load example data set-----------------------------------------------
articles <- sample_response$results_df

## ----check robots txt, cache=T, warning=F, prompt=F, message=F, error=F, results=F----
allowed <- paths_allowed(articles$url)
all(allowed)

## ----define parsing function, cache=T------------------------------------
get_article_body <- function (url) {
  
  # download article page
  response <- GET(url)
  
  # check if request was successful
  if (response$status_code != 200) return(NA)
  
  # extract html
  html <- content(x        = response, 
                  type     = "text", 
                  encoding = "UTF-8")
  
  # parse html
  parsed_html <- read_html(html)                   
  
  # define paragraph DOM selector
  selector <- "article#story div.StoryBodyCompanionColumn div p"
  
  # parse content
  parsed_html %>% 
    html_nodes(selector) %>%      # extract all paragraphs within class 'article-section'
    html_text() %>%               # extract content of the <p> tags
    str_replace_all("\n", "") %>% # replace all line breaks
    paste(collapse = " ")         # join all paragraphs into one string
}


## ----apply function to urls, cache=T, message=FALSE, results='hide'------
# create new text column
articles$body <- NA

# initialize progress bar
pb <- txtProgressBar(min     = 1, 
                     max     = nrow(articles), 
                     initial = 1, 
                     style   = 3)

# loop through articles and "apply" function
for (i in 1:nrow(articles)) {
  
  # "apply" function to i url
  articles$body[i] <- get_article_body(articles$url[i])
  
  # update progress bar
  setTxtProgressBar(pb, i)
  
  # sleep for 1 sec
  Sys.sleep(1)
}

## ----calculate sentiment, cache=T----------------------------------------
sentiment_by_day <- articles %>%
  select(url, body) %>%                                  # extract required columns 
  unnest_tokens(word, body) %>%                          # split each article into single words
  anti_join(get_stopwords(), by = "word") %>%            # remove stopwords
  inner_join(get_sentiments("afinn"), by = "word") %>%    # join sentiment scores
  group_by(url) %>%                                      # group text again by their URL
  summarise(sentiment = sum(value)) %>%                  # sum up sentiment scores
  left_join(articles, by = "url") %>%                    # add sentiment column to articles
  select(published_at, sentiment) %>%                    # extract required columns 
  group_by(date = as.Date(published_at)) %>%            # group by date
  summarise(sentiment = mean(sentiment), n = n())        # calculate summaries

## ----plot-results, dev='svg', fig.align='center', fig.width=7.3, fig.height=4.3----
# enable two plots in one figure
old_par <- par(mfrow=c(1, 2))

# plot number of articles vs. time 
barplot(height    = sentiment_by_day$n,
        names.arg = format(sentiment_by_day$date, "%a"),
        ylab      = "# of articles",
        ylim      = c(-10, 35),
        las       = 2)

# plot sentiment score vs. time
barplot(height    = sentiment_by_day$sentiment,
        names.arg = format(sentiment_by_day$date, "%a"),
        ylab      = "Sentiment Score",
        ylim      = c(-10, 35),
        las       = 2)

