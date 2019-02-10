#' Get resources of newsapi.org
#' 
#' \code{get_everything} returns articles from large and small news 
#' sources and blogs. This includes news as well as other regular articles.
#' You can search for multiple \code{sources}, different \code{language}, 
#' or use your own keywords. Articles can be sorted by the earliest date 
#' \code{publishedAt}, \code{relevancy}, or \code{popularity}. To automatically 
#' download all results, use \code{get_everything_all()}.\cr\cr
#' Please check that the \code{api_key} is available. You can provide an explicit
#' definition of the key or use \code{set_api_key()}. \cr\cr
#' Valid languages for \code{language} are provided in the dataset 
#' \code{terms_language}. 
#' 
#' @param query Character string that contains the searchterm for the API's 
#'              data base. API supports advanced search parameters, see 'details'.
#'              Passing a searchterm is compulsory. 
#' @param sources Character vector with with IDs of the news outlets 
#'                you want to focus on (e.g., c("usa-today", "spiegel-online")).
#' @param domains Character vector with domains that you want 
#'                to restrict your search to (e.g. c("bbc.com", "nytimes.com")).
#' @param exclude_domains Similar usage as with 'domains'. Will exclude these 
#'                        domains from your search.
#' @param from Marks the start date of your search. Must be in ISO 8601 format 
#'             (e.g., "2018-09-08" or "2018-09-08T12:51:42"). Package also 
#'             accepts: "YYYY/MM/DD", "YYYY.MM.DD", "YYYY,MM,DD" and "YYYY MM DD",
#'             including precise time (via %H:%M:%S). Default is the oldest 
#'             available date (depends on your paid/unpaid plan from newsapi.org).
#' @param to Marks the end date of your search. Works similarly to 'from'. 
#'           Default is the latest article available.
#' @param language Specifies the language of the articles of your search. Must 
#'                 be in ISO shortcut format (e.g., "de", "en"). See list of all 
#'                 languages using \code{newsanchor::terms_language}. Default 
#'                 is all languages.
#' @param sort_by Character string that specifies the sorting of your article 
#'                results. Accepts three options: "publishedAt", "relevancy", 
#'                "popularity". Default is "publishedAt".
#' @param page Specifies the page number of your results that is returned. Must 
#'             be numeric. Default is first page. If you want to get all results 
#'             at once, use \code{get_everything_all} from 'newsanchor'.
#' @param page_size The number of articles per page that are returned. 
#'                  Maximum is 100 (also default).
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key()}).
#' 
#' @details Advanced search (see also www.newsapi.org): Surround entire phrases 
#'          with quotes (") for exact matches. Prepend words/phrases that must 
#'          appear with "+" symbol (e.g., +bitcoin). Prepend words that must not 
#'          appear with "-" symbol (e.g., -bitcoin). You can also use AND, OR, 
#'          NOT keywords (optionally grouped with parenthesis, e.g., 'crypto AND 
#'          (ethereum OR litecoin) NOT bitcoin)').
#' @examples
#' \dontrun{
#' df <- get_everything(query = "stuttgart", language = "de")
#' df <- get_everything(query = "mannheim", from = "2019-01-02T12:00:00")
#' }
#' @importFrom httr content GET build_url parse_url add_headers
#' @importFrom jsonlite fromJSON
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' @export

get_everything <- function(query,
                           sources         = NULL,
                           domains         = NULL,
                           exclude_domains = NULL,
                           from            = NULL, 
                           to              = NULL,
                           language        = NULL, 
                           sort_by         = "publishedAt", 
                           page            = 1,
                           page_size       = 100, 
                           api_key         = Sys.getenv("NEWS_API_KEY")) {
  
  # Initial proceedings -----------------------------------------------------
  
  # Provide a vector with available ways of sorting the articles  
  sortings <- c("publishedAt", "relevancy", "popularity")
  
  # Errors and warnings -----------------------------------------------------
  
  # Make sure that any search term is passed
  if(missing(query) == TRUE)
    stop("You need to specify at least some content that you search for.")
  
  # check that page_size is <= 100
  if(!is.numeric(page_size)) {
    stop("You need to insert numeric values for the number of texts per page.")
  } 
  
  if(page_size > 100) {
    stop("Page size cannot not exceed 100 articles per page.")
  }
  
  # Error for non-numeric page parameter
  if(!is.numeric(page)) {
    stop("Page should be a number.")
  }
  
  # Error if language indicated does not match the ones provided by the API
  if(!is.null(language)){
    if(length(language) > 1){
      stop("You cannot specify more than one language.")
    }
    stop_if_invalid_language(language)
  }
  
  # Error if selected sorting does not match the ones provided by the API
  if(!sort_by %in% c("publishedAt", "relevancy", "popularity")){
    stop("Sortings can be only by 'publishedAt', 'relevancy', or 'popularity'.")
  }
  
  # Bind together various search parameters as comma-separated strings as required by API
  # Parameter: sources (plus limit to maximum of 20 sources)
  if(!is.null(sources)){
    if(length(sources) > 20){
      stop("You cannot specify more than 20 sources.")
    }
    sapply(sources, stop_if_invalid_source)
    sources <- collapse_to_comma_separated(sources)
  }
  
  # Parameter: domains 
  if(!is.null(domains)){
    domains <- collapse_to_comma_separated(domains)
  }
  
  # Parameter: exclude_domains
  if(!is.null(exclude_domains)) {
    exclude_domains <- collapse_to_comma_separated(exclude_domains)
  }
  
  # Make sure an API key is provided
  if(nchar(api_key) == 0)
    stop(paste0("You did not specify your API key as an argument or as a global variable.", 
                " See documentation for further info."))
  
  
  # Test if date format is in correct format
  
  
  valid_dates <- c("%Y-%m-%d", 
                   "%Y/%m/%d",
                   "%Y.%m.%d",
                   "%Y %m %d",
                   "%Y,%m,%d",
                   "%Y-%m-%d", 
                   "%Y/%m/%d %H:%M:%S",
                   "%Y.%m.%d %H:%M:%S",
                   "%Y %m %d %H:%M:%S",
                   "%Y,%m,%d %H:%M:%S")
  
  if(!is.null(from)) {
    
    tryCatch({
      from <- as.Date(from, tryFormats = valid_dates)},
      error = function(e){
        print(paste0("Your date format needs to be in one of these formats:", 
              " YYYY-MM-DD, YYYY/MM/DD, YYYY.MM.DD, YYYY,MM,DD, YYYY MM DD.",
              " For further information, check the package documentation.",
              " Default time range (all available) applies for results."))
         })
  }
  
  if(!is.null(to)) {
    
    tryCatch({
      to <- as.Date(to, tryFormats = valid_dates)},
      error = function(e){
        print(paste0("Your date format needs to be in one of these formats:", 
              " YYYY-MM-DD, YYYY/MM/DD, YYYY.MM.DD, YYYY,MM,DD YYYY MM DD.",
              " For further information, check the package documentation.",
              " Default time range (all available) applies for results."))
        })
  }
  
  
  # Accessing the API  -----------------------------------------------------
  # Build URL
  query_params <- list(q              = query,
                      language        = language,
                      sources         = sources,
                      domains         = domains,
                      excludeDomains  = exclude_domains,
                      from            = from,
                      to              = to,
                      sortBy          = sort_by,
                      pageSize        = page_size,
                      page            = page)
  
  url <- build_newsanchor_url("https://newsapi.org/v2/everything", query_params)
  response <- make_newsanchor_get_request(url = url, api_key = api_key)
  
  # extract meta data and results from response --------------------------------------------------
  content_parsed <- parse_newsanchor_content(response)
  if(is.null(content_parsed$totalResults)){
    content_parsed$totalResults <- 0
  }
  
  metadata <- extract_newsanchor_metadata(response, content_parsed, page, page_size)
  results_df <- extract_newsanchor_articles(metadata = metadata, content_parsed = content_parsed)
  # return results ----------------------------------------------------------
  return(list(metadata    = metadata, 
              results_df  = results_df))
  
}

