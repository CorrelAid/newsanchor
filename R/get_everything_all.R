#' Returns all articles from newsapi.org in one data frame
#'
#'  \code{get_everything} searches through articles from large and small news 
#' sources and blogs. This includes breaking news as well as other regular articles.
#' You  can search for multiple \code{sources}, different \code{language}, 
#' or use your own keywords. Articles can be sorted by the earliest date 
#' \code{publishedAt}, \code{relevancy}, or \code{popularity}. To automatically 
#' download all results, use \code{get_everything_all()}\cr\cr
#' Please check that the \code{api_key} is available. You can provide an explicit
#' definition of the api_key or use \code{set_api_key()}. \cr\cr
#' Valid languages for \code{language} are provided in the dataset \cr\cr
#' \code{terms_language}. To automatically download all results for one search, 
#' use \code{get_everything_all}\cr\cr.
#' Please check that the \code{api_key} is available. You can provide an explicit
#' definition of the api_key or use \code{set_api_key} \cr\cr
#' For valid searchterms see \code{data(searchterms)}
#' 
#' 
#' @param query Character string that contains the searchterm for the API's 
#'              data base. API supports advanced search parameters, see 'details'. 
#' @param sources Character string with IDs (comma separated) of the news outlets 
#'                you want to focus on (e.g., "usa-today, spiegel-online").
#' @param domains Character string (comma separated) with domains that you want 
#'                to restrict your search to (e.g., "bbc.com, nytimes.com").
#' @param exclude_domains Similar usage as with 'domains'. Will exclude these 
#'                        domains from your search.
#' @param from Marks the start date of your search. Must be in ISO 8601 format 
#'             (e.g., "2018-09-08" or "2018-09-08T12:51:42"). Default is the 
#'             oldest available date (depends on your paid/unpaid plan from 
#'             newsapi.org).
#' @param to Marks the end date of your search. Works similarly to 'from'. 
#'           Default is the latest article available.
#' @param language Specifies the language of the articles of your search. Must 
#'                 be in ISO shortcut format (e.g., "de", "en"). See list of all 
#'                 languages on https://newsapi.org/docs/endpoints/everything. 
#'                 Default is all languages.
#' @param sort_by Character string that specifies the sorting of your article 
#'                results. Accepts three options: "publishedAt", "relevancy", 
#'                "popularity". Default is "publishedAt".
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key}).
#'  
#' @examples
#' \dontrun{
#' df <- get_everything_all(query = "mannheim")
#' df <- get_everything_all(query = "stuttgart", language = "en")
#' }
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' @export


get_everything_all <- function(query, 
                              sources           = NULL,
                              domains           = NULL, 
                              exclude_domains   = NULL, 
                              from              = NULL,
                              to                = NULL,
                              language          = NULL,
                              sort_by           = "publishedAt",
                              api_key           = Sys.getenv("NEWS_API_KEY")){
  
  
  # initial request at newsapi.org ------------------------------------------
  
  # request
  results <- get_everything(query, 
                            sources, 
                            domains, 
                            exclude_domains,
                            from,
                            to,
                            language,
                            sort_by,
                            page = 1, 
                            page_size = 100, 
                            api_key)
  
  # additional requests (if necessary) --------------------------------------
  
  # check whether number of results is greater than results per page
  if (results$metadata$total_results > results$metadata$page_size) {
    
    # calculate max number of pages to download all results
    max_no_of_pages <- ceiling(results$metadata$total_results / 
                                 results$metadata$page_size)
    
    #--- and now loop across search queries
    for(i in seq.int(2, max_no_of_pages)) {
      
      # temporary results
      results_tmp <-  get_everything(query, 
                                     sources, 
                                     domains, 
                                     exclude_domains,
                                     from,
                                     to,
                                     language,
                                     sort_by,
                                     page = i, 
                                     page_size = 100, 
                                     api_key)
      
      # bind new results
      results$metadata   <- rbind(results$metadata,   results_tmp$metadata)
      results$results_df <- rbind(results$results_df, results_tmp$results_df)
      
      # avoid unnecessary requests if last status-code !=200 
      if (results_tmp$metadata$status_code != 200) break
      
    }
  }
  
  
  # return results ----------------------------------------------------------
  return(results)
}
