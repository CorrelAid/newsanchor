#' create_data_searchterms
#'
#' This script creates three different dataframes with possible search terms 
#' for 'country', 'category', and news 'sources'. This function should be 
#' ignored from building the package, but rather allows to update the dataframes
#' of available searchterms.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom devtools use_data
#' @examples
#' create_list_searchterms()

create_data_searchterms <- function(){

  
  ############################## HEADLINES #####################################
  
  # access URL --------------------------------------------------------------
  
  # define URL
  headlines_url = "https://newsapi.org/docs/endpoints/top-headlines"
  # read data from webpage
  headlines_webpage <- xml2::read_html(headlines_url)

  
  # scrap 'country' information ---------------------------------------------

  # use CSS selector to scrap countries
  headlines_country_html <- rvest::html_nodes(headlines_webpage,
                                       paste('.table-group:nth-child(2) ',
                                             '.table-group-item:nth-child(1) ',
                                             'code'))
  # converting countries to text
  headlines_terms_country <- rvest::html_text(headlines_country_html)
  # delete erroneous extraction from vector 
  headlines_terms_country <- headlines_terms_country[
                                       headlines_terms_country != "sources"]

  
  # scrap 'category' information --------------------------------------------

  # use CSS selector to scrap categories
  category_html <- rvest::html_nodes(headlines_webpage,
                                        paste('.table-group-item:nth-child(2) ',
                                              'code'))

  # converting categories to text
  terms_category <- rvest::html_text(category_html)
  # delete erroneous extraction from vector and change to DF
  terms_category <- as.data.frame(terms_category[terms_category != "sources"])
  # rename
  names(terms_category)[1] <- 'headlines'

  
  ############################## EVERYTHING ####################################
  
  # access URL --------------------------------------------------------------
  
  # define URL
  everything_url = "https://newsapi.org/docs/endpoints/everything"
  # read data from webpage
  everything_webpage <- xml2::read_html(everything_url)
  
  
  # scrap 'country' information ---------------------------------------------
  
  # use CSS selector to scrap countries
  everything_country_html <- rvest::html_nodes(everything_webpage,
                                        paste('.table-group-item:nth-child(7) ',
                                          'code'))
  # converting countries to text
  everything_terms_country <- rvest::html_text(everything_country_html)

  ############################## SOURCES #######################################
  
  # get 'sources' from newsapi.org ------------------------------------------
  terms_sources <- get_sources()
  terms_sources <- terms_sources$results_df["id"]  
  # adjust colnames
  names(terms_sources) <- c('all')
  
  ############################# SAVE DATA ######################################
  
  #--- combine country-dfs from headlines and everything 
  # adjust length of different vectors
  n <- max(length(headlines_terms_country), length(everything_terms_country))
  length(headlines_terms_country) <- n; length(everything_terms_country) <- n
  # combine dataframes
  terms_country <- cbind(as.data.frame(headlines_terms_country),
                         as.data.frame(everything_terms_country))
  # adjust colnames
  names(terms_country) <- c('headlines', 'everything')
  
  #--- save data
  devtools::use_data(terms_category, terms_country, terms_sources, overwrite = T)

}
