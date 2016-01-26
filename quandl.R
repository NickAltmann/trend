library(Quandl)

# Directory to cache timeseries in.
ts_cache_dir <- "zoo"

# If a series isn't found on Quandl it will be added to this file.  
# To avoid repeatedly querying for files that aren't there.
missing_series_file <- "missing_series.csv"


form_future_cache_file_name <- function(symbol) {
  return (file.path(ts_cache_dir, paste(gsub("/", "_", symbol), "csv", sep=".")))
}

# Loads from local cache or Quandl.
# Would need to manually remove from cache in case of update.

load_timeseries_data <- function(symbol) {
  # Local cache is stored as an attribute of the function, 
  # see http://www.r-bloggers.com/emulating-local-static-variables-in-r/.
  time_series_cache <- attr(load_timeseries_data, "time_series_cache")
  if (is.null(time_series_cache)) {
    time_series_cache = list()
  }
  
  if (exists(symbol, where = time_series_cache)) {
    # Take from local cache if it is there.
    mydata <- time_series_cache[[symbol]]    
  } else {
    # Get from file if stored there.
    cache_file_name = form_future_cache_file_name(symbol)
    if (file.exists(cache_file_name)) {
      mydata <- read.zoo(cache_file_name, sep=",", header = TRUE)
    } else {
      # Go to quandl otherwise
      mydata = Quandl(symbol, type="zoo")
      
      # Cache on file as is
      if (!dir.exists(dirname(cache_file_name))) {
        dir.create(dirname(cache_file_name))
      }
      write.zoo(mydata, cache_file_name, sep=",")
    }
    # Store in local cache.
    time_series_cache[[symbol]] <- mydata
    attr(load_timeseries_data, "time_series_cache") <<- time_series_cache
  }
  
  return (mydata)
}

exists_on_quandl <- function(symbol) {
  source = strsplit(as.character(symbol),"/")[[1]][1]
  res = Quandl.search(symbol, silent=TRUE, source=source)
  exists <- (length(res) > 0 & res[[1]]$code == symbol)
  return (exists)
}

