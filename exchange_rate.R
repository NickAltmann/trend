source('quandl.R')

# When to find exchange rates.
currency_definition_file <- "currencies.csv"

# -----------------------------------------------------------------------------
# Exchange rate functions ---------------------------------------------

get_exchange_rate_series <- function(currency) {
  specs <- read.csv(file=currency_definition_file,head=TRUE,sep=",", stringsAsFactors=FALSE)
  spec_row <- specs[which(specs$Currency==currency), ]
  symbol = paste0(spec_row$Exchange, "/", spec_row$Symbol)
  series <- load_timeseries_data(symbol)
  if (spec_row$Direction != "Long") {
    series$Rate <- 1 / series$Rate
  }
  return (series)
}

# Returns factor to multiple currency amount by to get dollar amount.
get_exchange_rate <- function(currency, date) {
  if (currency == "USD") {
    rate <- 1.0
  } else if (currency == "USd") {
    rate <- 0.01
  } else {
    series = get_exchange_rate_series(currency)
    if ((date < min(index(series))) | date > max(index(series))) {
      stop('Date is out of exchange rate range.')
    }
    rate <- series[findInterval(date, index(series)),'Rate']
  }
  return (rate)
}

# Get the first full year of the exchange rate series for the given currency.
get_currency_start_year <- function(currency) {
  if (currency == "USD" | currency == "USd") {
    return (NA)
  }
  series <- get_exchange_rate_series(currency)
  start_date <- index(series)[1]
  start_year <- as.numeric(format(as.yearmon(start_date), "%Y"))
  if (start_date > as.Date(ISOdate(start_year, 1, 3))) {
    start_year <- start_year + 1
  }
  return (start_year)
}
