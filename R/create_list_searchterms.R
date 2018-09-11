#' create_list_searchterms
#'
#' This function creates a list with possible search terms for 'country',
#' 'category', and news 'sources'. This is not intended to be distributed in
#' the package, but rather allows to update the dataset of searchterms.
#'
#' @param api_key API key required to access 'sources'
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_text
#' @examples
#' create_list_searchterms(api_key)
#' create_list_searchterms()

create_list_searchterms <- function(api_key = NULL){

  # check global environment for api_key
  if (is.null(api_key)){
    api_key <- Sys.getenv("api_key")
  }


  # URL to access
  url_headlines = "https://newsapi.org/docs/endpoints/top-headlines"

  # read data from webpage
  webpage_headlines <- xml2::read_html(url_headlines)

  #--- scrap 'country' information from URL
  # use CSS selector to scrap countries
  country_html <- rvest::html_nodes(webpage_headlines,
                             paste('.table-group:nth-child(2) ',
                                   '.table-group-item:nth-child(1) ',
                                   'code'))
  # converting countries to text
  country_data <- rvest::html_text(country_html)
  # delete erroneous extraction from vector
  country_data <- country_data[country_data != "sources"]


  #--- scrap 'category' information from URL
  # use CSS selector to scrap categories
  category_html <- rvest::html_nodes(webpage_headlines,
                              paste('.table-group-item:nth-child(2) ',
                                    'code'))

  # converting categories to text
  category_data <- rvest::html_text(category_html)
  # delete erroneous extraction from vector
  category_data <- category_data[category_data != "sources"]


  #--- get sources from newsapi.org
  sources <- get_sources(api_key)


  #--- combine different search terms to list

  searchterms <- list()
  searchterms[["country"]]   <- country_data
  searchterms[["category"]]  <- category_data
  searchterms[["sources"]]   <- sources


  #--- save list
  save(searchterms, file = "data/searchterms.Rda")

}
