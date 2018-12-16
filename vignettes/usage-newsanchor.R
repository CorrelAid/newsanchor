## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE, eval = FALSE, fig.align="center")

## ----libraries-----------------------------------------------------------
#  library(newsanchor)

## ----set_api_key---------------------------------------------------------
#  # save the api key in the .Renviron file
#  set_api_key(api_key = "YOUR API KEY",
#              path = "~/.Renviron")

## ----headlines: examples-------------------------------------------------
#  # get headlines published by the Washington Post
#  results <- get_headlines(sources = "the-washington-post")
#  # get headlines published in the category sports
#  results <- get_headlines(category = "sports")
#  # get headlines published in Germany
#  results <- get_headlines(country = "de")
#  # get headlines published about Trump
#  results <- get_headlines(query = "Trump")
#  

## ----headlines: page, page_size------------------------------------------
#  results <- get_headlines(category = "sports", page = 2)
#  results <- get_headlines(category = "sports", page = 3, page_size = 20)

## ----headlines: get_all--------------------------------------------------
#  results <- get_headlines_all(category = "sports")

## ----headlines: searchterms----------------------------------------------
#  terms_category
#  terms_country
#  terms_sources

## ----everything: search and advanced search------------------------------
#  # get everything published about Trump
#  results <- get_everything(query = "Trump")
#  # get everything published with the phrase "Trump says"
#  results <- get_everything(query = "Trump says")

## ----everything: examples------------------------------------------------
#  # get everything published about Trump
#  results <- get_everything(query = "Trump")
#  # get everything published about Trump in the Washington Post
#  results <- get_everything(query = "Trump", sources = "the-washington-post")
#  # get everything published about Trump in french
#  results <- get_everything(query = "Trump", languages = "fr")
#  # get everything published about Trump at bbc.com
#  results <- get_everything(query = "Trump", domains = "bbc.com")
#  # get everything published about Trump BUT NOT at bbc.com
#  results <- get_everything(query = "Trump", exclude_domains = "bbc.com")
#  # get everything published about Trump BUT NOT at bbc.com
#  results <- get_everything(query = "Trump", from = "2018-09-08")

## ----everything: page, page_size-----------------------------------------
#  results <- get_everything(query = "Trump", page = 2)
#  results <- get_everything(query = "Trump", page = 3, page_size = 20)

## ----everything: sort----------------------------------------------------
#  # sort results by relevancy
#  results <- get_everything(query = "Trump", sort_by = "relevancy")
#  # sort results by popularity
#  results <- get_everything(query = "Trump", sort_by = "popularity")
#  # sort results by date
#  results <- get_everything(query = "Trump", sort_by = "publishedAt")
#  

## ----everything: get_all-------------------------------------------------
#  results <- get_everything_all(query = "Trump")

## ----everything: searchterms---------------------------------------------
#  terms_language
#  terms_sources
#  terms_country
#  terms_category

## ----sources-------------------------------------------------------------
#  publisher <- get_sources()

