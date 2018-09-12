#'@title Add API key to the .Renviron
#'
#'@description Function to set you API Key to the R environment when starting using \code{newsanchor} package. Attention: You should only execute this functions once.
#'
#'@param api_key character. The personal API key To request an API key see: \url{https://newsapi.org/register}
#'@param path character. Path where the environment is stored. Default is the normalized path.
#'
#'@return None.
#'
#'@examples
#'\dontrun{
#'# this is not an actual API key
#'api_key <- "5t5yno5qqkufxis5q2vzx26vxq2hqej9"
#'set_api_key(api_key, tempdir())
#'}
#'@author Jan Dix <\email{jan.d@@correlaid.org}>
#'
#'@export

set_api_key <- function(api_key,
                        path = stop("Please specify a path.")) {
  
  # check if an environment file exists
  if (!file.exists(path)) file.create(path)
  
  # read environment file
  env_file <- readLines(path, encoding = "UTF-8")
  
  # setup key variable
  key <- paste0("NEWS_API_KEY=", api_key)
  
  # add api key
  env_file <- c(env_file, key)
  
  # write environment file
  writeLines(env_file, path)
  
  # send success message
  message <- paste("Your api key was successfully appended to your .Renviron.",
                   "Please restart the session to automatically load the key.",
                   sep = "\n")
  message(message)
}