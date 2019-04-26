#'@title Add API key to the .Renviron
#'
#'@description Function to set you API Key to the R environment when starting using \code{newsanchor} package. Attention: You should only execute this functions once.
#'
#'@param path character. Path where the environment is stored. Default is the normalized path.
#'
#'@return None.
#'
#'@examples
#'\dontrun{
#'set_api_key(tempdir()) # you will be prompted to enter your API key.
#'}
#'@author Jan Dix <\email{jan.d@@correlaid.org}>
#'@importFrom askpass askpass
#'@export

set_api_key <- function(path = stop("Please specify a path.")) {
  
  # check if an environment file exists
  if (!file.exists(path)) file.create(path)
  
  # read environment file
  env_file <- readLines(path, encoding = "UTF-8")
  
  # setup key variable
  key <- paste0("NEWS_API_KEY=", askpass::askpass("Please enter your api key"))
  
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
