#' create_list_searchterms
#'
#' This function creates a list with possible search terms for 'country',
#' 'category', and news 'sources'. This is not intended to be distributed in
#' the package, but rather allows to update the dataset of searchterms.
#'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @importFrom devtools use_data
#' @examples
#' create_list_searchterms()

create_list_searchterms <- function(){

  # access URL --------------------------------------------------------------
  
  # define URL
  url_headlines = "https://newsapi.org/docs/endpoints/top-headlines"

  # read data from webpage
  webpage_headlines <- xml2::read_html(url_headlines)

  
  
  # scrap 'country' information ---------------------------------------------

  # use CSS selector to scrap countries
  country_html <- rvest::html_nodes(webpage_headlines,
                             paste('.table-group:nth-child(2) ',
                                   '.table-group-item:nth-child(1) ',
                                   'code'))
  # converting countries to text
  terms_country <- rvest::html_text(country_html)
  # delete erroneous extraction from vector and change to DF
  terms_country <- as.data.frame(terms_country[terms_country != "sources"])
  # rename
  names(terms_country)[1] <- 'country'

  # scrap 'category' information --------------------------------------------

  # use CSS selector to scrap categories
  category_html <- rvest::html_nodes(webpage_headlines,
                              paste('.table-group-item:nth-child(2) ',
                                    'code'))

  # converting categories to text
  terms_category <- rvest::html_text(category_html)
  # delete erroneous extraction from vector and change to DF
  terms_category <- as.data.frame(terms_category[terms_category != "sources"])
  # rename
  names(terms_category)[1] <- 'category'

  
  # get 'sources' from newsapi.org ------------------------------------------
  terms_sources <- get_sources()
  terms_sources <- terms_sources$results_df["id"]

  #--- save list
  devtools::use_data(terms_category, terms_country, terms_sources, overwrite = T)

}
