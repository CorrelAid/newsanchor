#' create_data_searchterms
#'
#' This script creates four different dataframes with possible search terms 
#' for 'country', 'category', news 'sources' and 'languages. This function should be 
#' ignored from building the package, but rather allows to update the dataframes
#' of available searchterms.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom devtools use_data
#' @examples
#' create_data_searchterms()

create_data_searchterms <- function(){
  
  ############################### HEADLINES ######################################
  
  # access URL --------------------------------------------------------------
  
  # define URL
  headlines_url = "https://newsapi.org/docs/endpoints/top-headlines"
  # read data from webpage
  headlines_webpage <- xml2::read_html(headlines_url)
  
  
  # scrap 'country' information ---------------------------------------------
  
  # use CSS selector to scrap countries
  country_html <- rvest::html_nodes(headlines_webpage,
                                    paste('.table-group:nth-child(2) ',
                                          '.table-group-item:nth-child(1) ',
                                          'code'))
  
  # converting countries to text
  terms_country <- rvest::html_text(country_html)
  # delete erroneous extraction from vector 
  terms_country <- data.frame(terms_country[terms_country != "sources"],
                              stringsAsFactors = F)
  # rename
  names(terms_country)[1] <- 'country'
  
  # scrap 'category' information --------------------------------------------
  
  # use CSS selector to scrap categories
  category_html <- rvest::html_nodes(headlines_webpage,
                                     paste('.table-group-item:nth-child(2) ',
                                           'code'))
  
  # converting categories to text
  terms_category <- rvest::html_text(category_html)
  # delete erroneous extraction from vector and change to DF
  terms_category <- data.frame(terms_category[terms_category != "sources"],
                               stringsAsFactors = F)
  # rename
  names(terms_category)[1] <- 'category'
  
  
  ############################## EVERYTHING ####################################
  
  # access URL --------------------------------------------------------------
  
  # define URL
  everything_url = "https://newsapi.org/docs/endpoints/everything"
  # read data from webpage
  everything_webpage <- xml2::read_html(everything_url)
  
  
  # scrap 'language' information ---------------------------------------------
  
  # use CSS selector to scrap language
  language_html <- rvest::html_nodes(everything_webpage,
                                     paste('.table-group-item:nth-child(7) ',
                                           'code'))
  # converting countries to text
  terms_language <- rvest::html_text(language_html)
  # change to DF
  terms_language <- data.frame(terms_language, stringsAsFactors = F)
  # rename
  names(terms_language)[1] <- 'language'
  
  ############################## SOURCES #######################################
  
  # get 'sources' from newsapi.org ------------------------------------------
  terms_sources <- get_sources()
  terms_sources <- terms_sources$results_df["id"]  
  # adjust colnames
  names(terms_sources) <- 'sources'
  
  ############################# SAVE DATA ######################################
  
  #--- save data
  devtools::use_data(terms_category, terms_country, terms_sources, 
                     terms_language, overwrite = T)
  
}
