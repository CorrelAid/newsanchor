#' Returns all headlines from newsapi.org
#'
#' \code{get_headlines} returns live top and breaking headlines for a country, 
#' specific category in a country, single source, or multiple sources. You can 
#' also search with keywords. Articles are sorted by the earliest date 
#' published first. To automatically download all results, use 
#' \code{get_headlines_all}.\cr\cr
#' Please check that the api_key is available. You can provide an explicit
#' definition of the api_key or use \code{set_api_key} \cr\cr
#' Valid searchterms are provided in \code{terms_category}, 
#' \code{terms_country} or \code{terms_sources}
#' 
#' @param query Character string that contains the searchterm 
#' @param category Category you want headlines from
#' @param country Country you want headlines for
#' @param sources Character string with IDs (comma separated) of the news outlets 
#'                you want to focus on (e.g., "usa-today, spiegel-online").
#' @param api_key Character string with the API key you get from newsapi.org. 
#'                Passing it is compulsory. Alternatively, function can be 
#'                provided from the global environment (see \code{set_api_key}).
#'  
#' @examples
#' \dontrun{
#' df <- get_headlines_all(query = "sports")
#' df <- get_headlines_all(category = "health")
#' }
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' @export


get_headlines_all <- function(query     = NULL, 
                              category  = NULL,
                              country   = NULL, 
                              sources   = NULL, 
                              api_key   = Sys.getenv("NEWS_API_KEY")){
  
  


  # initial request ---------------------------------------------------------

  # initial call to newsapi.org
  page = 1
  results <- get_headlines(query, category, country, sources, 
                           page = page, page_size = 100, api_key)
  
  

  # further requests (if necessary) -----------------------------------------

  # calculate maximal number of pages to download all results
  max_no_of_pages <- ceiling(results$metadata$total_results / 
                             results$metadata$page_size)
   
  # make further requests while current page < than maximal no. of pages
  while(page < max_no_of_pages) {
    
    # avoid unnecessary requests if status-code !=200 
    if (tail(results$metadata$status_code, n=1) != 200)  {
      break
    }
      
    # update page number
    page= page + 1

    # temporary results
    results_tmp <- get_headlines(query, category, country, sources, 
                                 page = page, page_size = 100, api_key)
      
    # bind new results
    results$metadata   <- rbind(results$metadata,   results_tmp$metadata)
    results$results_df <- rbind(results$results_df, results_tmp$results_df)

  }

  # return results ----------------------------------------------------------
  return(results)
}
