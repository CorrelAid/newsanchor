#' Get all resources that the API from newsapi.org provides
#'
#' @param key Character string with the API key you get from newsapi.org. Passing it is compulsory.
#' @param content Character string that contains the content of your search in the API data base
#' @param nrtexts The number (numeric!) of articles per page that are returned. Maximum is 100, default is 20.
#' @param sources Character string with IDs (comma separated) of the news outlets you want to focus on (e.g., "usa-today, spiegel-online").
#' @param domains Character string (comma separated) with domains that you want to restrict your search to (e.g. "bbc.com, nytimes.com").
#' @param exclude_domains Similar usage as 'domains'. Will exclude these domains from your search.
#' @param from Marks the date of the start of your search. Must be in ISO 8601 format (e.g., '2018-09-08' or '2018-09-08T12:51:42'). Default is the oldest.
#' @param to Marks the end date of your search. Works similarly as 'from'. Default is the latest article.
#' @param language Specifies the language of the articles of your search. Must be in ISO shortcut format (e.g., "de", "en"). See list of all languages on https://newsapi.org/docs/endpoints/everything. Default is all languages.
#' @param sort_by Character string that specifies the sorting of your article results. Has three options: "publishedAt", "relevancy", "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results. Must be numeric. Default is first page.
#' @return A list containing a data frame with results for your search and another list with metadata on your search.
#' @examples
#' get_everything(key = key, content = "stuttgart", language = "de")
#' get_everything(key = key, content = "mannheim", from = "2018-01-07T12:00:00")
#'


library(needs)
needs(httr, tidyverse, plyr)


get_everything <- function(key,
                       content,
                       nrtexts = NULL, # Default is 20
                       sources = NULL,
                       domains = NULL,
                       exclude_domains = NULL,
                       from = NULL, # Default is all
                       to = NULL, # Default is latest
                       language = NULL, # Default is all languages
                       sort_by = NULL, # Default is "publishedAt"
                       page = NULL) {

  # Check evaluation of passed arguments
  sortings <- c("publishedAt", "relevancy", "popularity")

  ISO <- c("ar", "de", "en", "es", "fr", "he", "it", "nl", "no",
  "pt", "ru", "se", "ud", "zh")

  # Make sure that the API key is passed
  if(missing(key) == TRUE)
    stop("You need to pass your API key")

  # Make sure that some content is passed
  if(missing(content) == TRUE)
    stop("You need to specify at least some content that you search for")

  # Make sure page size does not exceed 100
  if(is.null(nrtexts)) {

    nrtexts <- 20

  } else if(!is.numeric(nrtexts)) {

    stop("You need to insert numeric values for the number of texts per page")

  }   else if(nrtexts > 100) {

    stop("Page size cannot exceed 100 articles")

  }

  if(!is.null(page)) {
  stopifnot(is.numeric(page))#, call. =  FALSE)
  #stop("Arguments 'page' and 'nrtexts' need to be numeric")
    }

  if(!is.null(language)) {
  stopifnot((sum(language == ISO) == 1))#, call. = FALSE) # language
  #stop("Language must be specified as one of these ISO codes: ar, de, en, es, fr, he, it, nl, no, pt, ru, se, ud, zh)
  }

  if(!is.null(sort_by)) {
  stopifnot((sum(sort_by == sortings) == 1))#, call. = FALSE) # sortBy
  # stop("sortings can be only by 'publishedAt', 'relevancy', or 'popularity'")
  }

  # Build URL

  rawurl <- parse_url("https://newsapi.org/v2/everything")

  rawurl$query <- list(q = content,
                    pageSize = nrtexts,
                    page = page,
                    language = language,
                    sources = sources,
                    domains = domains,
                    excludeDomains = exclude_domains,
                    from = from,
                    to = to,
                    sortBy = sort_by)

  url <- build_url(rawurl)

  # Make request & save result
  raw <- httr::GET(url, httr::add_headers("X-Api-Key" = key))

  # Convert raw request into meaningful content
  rawtext <- httr::content(raw)

  ## Extract articles

  # Make a data.frame out of the list
  results_df = as.data.frame(do.call("rbind", rawtext$articles))

  # Extract sources and add to data
  results_df$id <- results_df %>% unnest(source) %>% as.data.frame() %>%
    .[c(TRUE, FALSE), ]

  results_df$name <- results_df %>% unnest(source) %>% as.data.frame() %>%
    .[c(FALSE, TRUE), ]

  results_df$publishedAt <- as.POSIXct(paste(substr(results_df$publishedAt, 1, 10),
                                             substr(results_df$publishedAt, 12, 19)),
                                       tz = "GMT",
                                       format("%Y-%m-%d %H:%M:%OS"))

  # Drop list from data
  results_df$source <- NULL

  # Change data type
  results_df$author <- unlist(results_df$author)
  results_df$description <- unlist(results_df$description)
  results_df$url <- unlist(results_df$url)
  results_df$urlToImage <- unlist(results_df$urlToImage)
  results_df$title <- unlist(results_df$title)
  #results_df$id <- unlist(results_df$id)
  results_df$name <- unlist(results_df$name)

  # Remove NULL to NA
  results_df[is_empty(results_df)] = NA


  ## Create metadata list

  metadata <- as.list(c(rawtext$totalResults,
                        raw$status_code,
                        raw$date,
                        raw$url))

  names(metadata) <- c("TotalResults", "HTTPStatus", "DateofRequest", "URL")

  metadata$DateofRequest <- type.convert(raw$date, as.is = TRUE)

  return(results_df)

}



# Execute function
# search2 <- everything(key = key, content = content, language = language, sort_by = sort, page = page)



