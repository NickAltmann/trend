source('common.R')

# -----------------------------------------------------------------------------
# Test data functions ---------------------------------------------------------

is_test_symbol <- function(symbol) {
  return (length(grep('^TEST', symbol) == 1))
}

test_data_parameters <- function() {
  return (list (  seed_date       = '1980-01-01', # Start all test data generation from same point for consitency.
                  end_date        = '2016-01-01',
                  av_price        = 100.0,
                  annual_spread   = 10.0, # Amount to gain and lose in year, so price spread is actually double this.
                  monthly_spread  = 1.0,
                  weekly_spread   = 0.5,
                  daily_spread    = 0.25,
                  cycle           = 90,   # Approximate days in cycle    
                  basis           = 0.01, # Amount to drop price per day
                  roll_day        = 20,   # Day of month to roll contract
                  years_back      = 3,    # Years that each future contract extends
                  open_int_peak   = 30))  # Days before contract ends that open interest peaks
}

get_base_test_data_series <- function() {
  t <- test_data_parameters()        
  
  seed_date <- as.Date(t$seed_date)
  end_date <- as.Date(t$end_date)
  av_price <- t$av_price
  annual_spread <- t$annual_spread
  monthly_spread <- t$monthly_spread
  weekly_spread <- t$weekly_spread
  daily_spread <- t$daily_spread
  cycle <- t$cycle
  basis <- t$basis
  
  # Create base series.
  dd <- seq(as.Date(seed_date), as.Date(end_date), "days")
  time_span <- length(dd)
  cc <- 1:time_span
  settle <- av_price + annual_spread * sin(cc*2*pi/365) + monthly_spread * sin(cc*2*pi/29) + weekly_spread * sin(cc*2*pi/7)
  high <- settle + daily_spread
  low <- settle - daily_spread
  shifted_settle <- c(settle[1], settle[1:length(settle) - 1])
  open <- (settle + shifted_settle) / 2
  
  df <- data.frame(open, high, low, settle, stringsAsFactors = FALSE)
  colnames(df) <- c("Open", "High", "Low", "Settle")
  
  series <- zoo(df, dd)
  
  return (series)
}

create_test_data_from_symbol <- function(symbol) {
  t <- test_data_parameters()        
  
  details = deconstruct_symbol(symbol)
  
  roll_day <- t$roll_day
  years_back <- t$years_back
  
  end_date <- ISOdate(details$year, details$month, roll_day)    
  start_date <- ISOdate(details$year - years_back, details$month, roll_day)
  
  return (create_test_data(start_date, end_date))    
}

create_test_data <- function(start_date, end_date) {
  t <- test_data_parameters()        
  peak <- t$open_int_peak
  cycle <- t$cycle
  basis <- t$basis
  
  # Always start from same base data, so we are consistent before applying basis.
  series <- get_base_test_data_series()
  
  # Progression of open interest up to the peak then falling away.
  OpenInterest <- as.integer(10000 * ((1-(peak/(peak + abs(as.integer(index(series) - as.Date(end_date)))))) * (cycle/(cycle + abs(as.integer(index(series) - as.Date(end_date) - peak))))))
  series <- merge(series, OpenInterest)
  
  # Just week days
  series <- series[!is_weekend(index(series))]
  
  # Reduce to just the span of interest    
  series <- window(series, start = as.Date(start_date), end = as.Date(end_date))    
  
  # Adjust prices for basis
  series$Settle = series$Settle - basis * as.integer(as.Date(end_date) - index(series))
  series$Open   = series$Open - basis * as.integer(as.Date(end_date) - index(series))
  series$High   = series$High - basis * as.integer(as.Date(end_date) - index(series))
  series$Low    = series$Low - basis * as.integer(as.Date(end_date) - index(series))
  
  return (series)
}
