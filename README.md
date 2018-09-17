
[![Build Status](https://travis-ci.com/CorrelAid/newsanchor.svg?token=61qJUDkqyXs9KzgSbKeK&branch=master)](https://travis-ci.com/CorrelAid/newsanchor)

**newsanchor**

An R Wrapper for the [NewsAPI](newsapi.org). 


API key
-------

1. Register and get your personal API key from [here](https://newsapi.org/register)

2. To automatically set the API-Key in R, save it locally:
``` r 
# your own key goes here:
apiKey = "98c687e159c34c8f98000000xxxxxxxxxxxxx" 
```
or set it globally (much more convenient):
``` r
Sys.setenv("NEWS_API_KEY" = apiKey)
```