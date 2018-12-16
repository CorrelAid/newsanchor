#' Sample Response Object
#' 
#' A sample response object generated using `get_everything`.
#' 
#' This response object was mainly created for demonstrating purposes.
#' The data set is used in the "Scrape New York Times Online Articles" vignette.
#' The object was created using the following query.
#' 
#' @return List with two dataframes:\cr
#'         1) Data frame with \code{results_df}\cr
#'         2) Data frame with \code{meta_data}
#' 
#' @examples 
#' \dontrun{
#' response <- get_everything(query   = "Trump",
#'                            sources = "the-new-york-times",
#'                            from    = "2018-12-03",
#'                            to      = "2018-12-09") 
#' }
"sample_response"