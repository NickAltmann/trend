library(zoo)
library(Quandl)

ts_cache_dir <- "zoo"

# Config file in working directory given contract details.
future_specs_file <- "contract_specs.csv"

# If a series isn't found on Quandl it will be added to this file.  
# To avoid repeatedly querying for files that aren't there.
missing_series_file <- "missing_series.csv"

# -----------------------------------------------------------------------------
# Future series loading functions ---------------------------------------------

# Month to code mapping.
future_codes <- function() {
  return (c('F','G','H','J','K','M','N','Q','U','V','X','Z'))
}

future_month_code <- function(month) {
  codes <- future_codes()
  return (codes[month])
}

# Get the time series data
get_future_data <- function(source, ticker, month_in_year, year) {
  symbol = form_future_symbol(source, ticker, month_in_year, year)
  return (get_future_data_from_symbol(symbol))
}

form_future_cache_file_name <- function(symbol) {
  return (file.path(ts_cache_dir, paste(gsub("/", "_", symbol), "csv", sep=".")))
}

# Get the time series data
get_future_data_from_symbol <- function(symbol) {
  # Check if this test data.
  if (is_test_symbol(symbol)) {
    mydata = (create_test_data_from_symbol(symbol))        
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
        
    # Get consistent name for open interest column    
    open_interest_column <- grep("Interest",names(mydata))
    names(mydata)[open_interest_column] <- "OpenInterest"
    
    mydata <- mydata[,c("Open","High","Low","Settle", "OpenInterest")]
    
    # Small clean of the data, remove zero values.
    mydata <- mydata[mydata$Open > 0 & mydata$Settle > 0 & mydata$OpenInterest > 0,]
  }

  # Add a couple of derived fields we will need later.    
  PreviousSettle <- lag(mydata, -1)[,"Settle"]
  mydata = merge(mydata, PreviousSettle)
    
  NextOpen <- lag(mydata, 1)[,"Open"]
  mydata = merge(mydata, NextOpen)
  
  return (mydata)
}

future_data_exists <- function(source, ticker, month_in_year, year) {
  symbol = form_future_symbol(source, ticker, month_in_year, year)
  return (future_data_exists_from_symbol(symbol))
}

future_data_exists_from_symbol <- function(symbol) {
  if (file.exists(missing_series_file)) {
    missing_series <- read.csv(file=missing_series_file,head=TRUE,sep=",")     
  } else {
    missing_series <- data.frame(symbol=character())
  }
  

  if (is_test_symbol(symbol)) {
    return (TRUE)
  }
    
  if (symbol %in% missing_series$Symbol) {
    return (FALSE)
  }
    
  cache_file_name = form_future_cache_file_name(symbol)
  if (file.exists(cache_file_name)) {
    return (TRUE)
  }
    
  source = strsplit(as.character(symbol),"/")[[1]][1]
  res = Quandl.search(symbol, silent=TRUE, source=source)
  exists <- (length(res) > 0 & res[[1]]$code == symbol)
  if (!exists) {
    missing_series <- rbind(missing_series, data.frame(Symbol=symbol))
    write.csv(missing_series, file=missing_series_file, row.names=FALSE)
  }
  return (exists)
}

# Get future code from details.
form_future_symbol <- function(source, ticker, month_in_year, year) {
  symbol = paste0(source, "/", ticker, future_month_code(month_in_year), year)
  return (symbol)
}

# Get future details from code.
deconstruct_symbol <- function(symbol) {
  source <- strsplit(symbol, '/')[[1]][1]
  rest <- strsplit(symbol, '/')[[1]][2]
  year <- as.integer(substr(rest, nchar(rest)-3, nchar(rest)))
  month_sym <- substr(rest, nchar(rest)-4, nchar(rest)- 4)
  codes <- future_codes()
  month <- as.integer(match(month_sym, codes))
  ticker <- substr(rest, 1, nchar(rest)- 5)
  
  return (list(source=source, year=year, month=month, ticker=ticker))
}

get_future_spec_row <- function(source, ticker) {
  future_specs <- read.csv(file=future_specs_file,head=TRUE,sep=",")
  return (future_specs[ which(future_specs$Symbol==ticker & future_specs$Exchange==source), ])
}

get_point_value <- function(source, ticker) {
  return (get_future_spec_row(source,ticker)[,"PointValue"])
}

get_normal_months <- function(source, ticker) {
  month_string <- get_future_spec_row(source,ticker)[,"Months"]
  months <- 1:12
  if (month_string != "") {
    months <- (as.numeric(strsplit(as.character(month_string),",")[[1]]))
  }
  return (months)
}

# -----------------------------------------------------------------------------
# Future rolling functions ----------------------------------------------------

# Find the date we want to switch from using one future series to the next.
find_cross_over_date <- function(earlier_series, later_series, start_date=0) {
  # Returns first date on which we want to use later series.
  # This will be the date we perform the roll.
  cross_over_date <- start_date
  last_earlier_series_date <- end(earlier_series)
  
  if (last_earlier_series_date > cross_over_date) {
    combined_open <- merge(earlier_series[,"OpenInterest"], later_series[,"OpenInterest"])
    names(combined_open) <- c("Earlier","Later")
    combined_open <- window(combined_open, start = start_date, end = last_earlier_series_date)
    
    rows_where_later_is_greater <- which(combined_open$Earlier < combined_open$Later)
      
    if (length(rows_where_later_is_greater) > 0) {
      # Switch on first date later series exceeds earlier.
      cross_over_date = index(combined_open[rows_where_later_is_greater[1]])
    } else {
      # Switch to later series on last day of earlier series if it never exceeds open interest
      cross_over_date <- last_earlier_series_date
    }
  }
  return (cross_over_date)
}

# Find the basis between two future series at the point of switching.
find_cross_over_gap <- function(earlier_series, later_series, switch_date) {
  # Just  difference in settlement
  early = earlier_series[switch_date,"Settle"]
  late = later_series[switch_date,"Settle"]

  return (round(as.double(late - early), digits=4))
}

# Helper to manager a list of future series.
# Idea is to hold on to series that have already been loaded.
add_future_series_to_list <- function(future_series_list, symbol) {
  if (!symbol %in% names(future_series_list)) {
    s = get_future_data_from_symbol(symbol)
    future_series_list[[length(future_series_list)+1]] = s
    names(future_series_list)[length(future_series_list)] = symbol
  }
  return (future_series_list)
}

# Main method for creating a full series from a set of future series.
create_future_time_series <- function(start_year, end_year, source, ticker) {
  # This full time series of futures stitched together.
  full_series <- NA
  
  # The individual future series that are used to make up the full series
  future_series <- list() 
  
  # Keep track of roll details. This will become a data frame with rows sequencially added.
  # This is inefficient due to re-allocation, but numbers involved will be small.
  roll_details <- NA
  
  months <- get_normal_months(source, ticker)
  year <- start_year
  next_year <- start_year
  month_index <- 1
  next_month_index <- 1
  start_date <- as.Date(ISOdate(start_year, 1, 1))
  end_date <- as.Date(ISOdate(end_year, 12, 31))

  # Find the first exising series
  repeat {
    month <- months[month_index]
    if (future_data_exists(source, ticker, month, year)) {
      # Found the first series, now check it doesn't roll before the start date.
      symbol <- form_future_symbol(source, ticker, month, year)
      future_series <- add_future_series_to_list(future_series, symbol)
      series <- future_series[[symbol]]
      if (end(series) > start_date) {
        break
      }
    }
    # Move to the next series.
    month_index <- month_index + 1
    if (month_index > length(months)) {
      month_index <- 1
      year <- year + 1
    }        
    if (year > end_year) {
      # Failed to find any suitable future series.
      return (NA)
    }
  }
  
  next_year <- year
  next_month_index <- month_index

  repeat {
    next_month_index <- next_month_index + 1
    if (next_month_index > length(months)) {
      next_month_index <- 1
      next_year <- next_year + 1
    }
    
    next_month <- months[next_month_index]
    next_series_exists = future_data_exists(source, ticker, next_month, next_year)

    if (next_series_exists) {
      current_month <- months[month_index]
      current_symbol <- form_future_symbol(source, ticker, current_month, year)
      future_series <- add_future_series_to_list(future_series, current_symbol)
      current_series <- future_series[[current_symbol]]
      
      next_symbol <- form_future_symbol(source, ticker, next_month, next_year)
      future_series <- add_future_series_to_list(future_series, next_symbol)
      next_series <- future_series[[next_symbol]]

      cross_over_date <- find_cross_over_date(current_series, next_series, start_date)
      cross_over_gap <- find_cross_over_gap(current_series, next_series, cross_over_date)

      s <- current_series[index(current_series) >= start_date & index(current_series) < cross_over_date,] # Don't want cross_over_date itself
            
      if (nrow(s)) {
        res <- add_roll(current_symbol, s, full_series, roll_details)
        full_series <- res$full_series
        roll_details <- res$roll_details
      }
      
      full_series$AdjustedSettle <- full_series$AdjustedSettle + cross_over_gap
      
      start_date <- cross_over_date
      year <- next_year
      month_index <- next_month_index
    }

    # Break if we've reached the end or run out of months in the year after the end date.
    if  (next_year > end_year & 
          ( (next_series_exists & end(next_series) > end_date) 
            | next_month_index >= length(months) 
          )
        ) {
      break
    }
  }

  if (next_series_exists) {
    last_symbol <- form_future_symbol(source, ticker, next_month, next_year)
    s = window(next_series, start = cross_over_date, end = end_date)
    if (nrow(s)) {
      res <- add_roll(last_symbol, s, full_series, roll_details)
      full_series <- res$full_series
      roll_details <- res$roll_details
    }
  } 
  
  return (list(full_series=full_series, future_series=future_series, roll_details=roll_details))
}

# Helper to perform the roll operation from one series to the next.
# Returns the updated series and roll details.
add_roll <- function(new_symbol, new_series, full_series, roll_details) {
  new_series_start <- start(new_series)
  new_series_end <- end(new_series)
  new_series$RollBuy <- 0
  new_series$RollSell <- 0
  new_series$RollBuy[new_series_start] <- 1
  new_series$RollSell[new_series_end] <- 1
  new_series$AdjustedSettle <- new_series$Settle
  roll_detail <- data.frame(code=new_symbol, start_date=new_series_start, end_date=new_series_end)
  
  if (is.object(full_series)) {
    full_series <- rbind(full_series, new_series)
  } else {
    full_series <- new_series
  }
  if (is.object(roll_details)) {
    roll_details <- rbind(roll_details, roll_detail)
  } else {
    roll_details <- roll_detail
  }
  
  ll <- list(full_series = full_series, roll_details = roll_details)
  return (ll)
}

# -----------------------------------------------------------------------------
# Simulation functions --------------------------------------------------------

traded_price <- function(price, source, ticker, long_not_short) {
  # Want to put in adjustments for slippage and spread.
  return (price)
}

# Start and end date are the days of the signal, ie day before the trade is done.
calc_trade_pnl <- function(series, start_date, end_date, position) {
  in_price = as.double(series[start_date, "NextOpen"])
  out_price = as.double(series[end_date, "NextOpen"])

  roll_out = sum(series[index(series) >= start_date & index(series) <  end_date & series$RollSell == 1, "NextOpen"])
  roll_in  = sum(series[index(series) >  start_date & index(series) <= end_date & series$RollBuy  == 1, "Open"])
          
  factor <- position # Need to add point value
  
  return ((out_price - in_price + roll_out - roll_in) * factor)
}

moving_average <- function(series, span) {
  return (rollmeanr(series, span, fill = NA))
}

moving_min_max <- function(series, span, is_max) {
  return (rollapply(series, span, function(x) min_max(x, is_max), fill = NA, align = "right"))
}

min_max <- function(series, is_max) {
  if (is_max) {
    return (max(series))
  }
  return (min(series))
}

comparison <- function(x, y) {
  if (is.na(x) | is.na(y)) {
    return (NA)
  }
  if (x>y) {
    return (1)
  }
  if (x<y) {
    return (-1)
  }
  return (0)
}

# Returns:
#  1  up-trend
# -1  down-trend
#  0  flat or can't be computed
trend_from_moving_average <- function(series, fast_span, slow_span) {
  ma_slow <- moving_average(series$AdjustedSettle, slow_span)
  ma_fast <- moving_average(series$AdjustedSettle, fast_span)
  ma <- merge(ma_fast, ma_slow)
  t <- apply(ma, 1, function(x) return (comparison(x[1], x[2])))
  t[is.na(t)] <- 0
  return (t)
}

# Returns true if price at max level for the given span.
min_max_price_signal <- function(series, span, is_max) {
  x <- moving_min_max(series$AdjustedSettle, span, is_max)
  m <- merge(x, series$AdjustedSettle)
  t <- apply(m, 1, function(x) x[1] == x[2])
  t[is.na(t)] <- FALSE
  return (t)
}

average_true_range <- function(series, span) {
  f <- apply(series, 1, function(x) return(max(c(x['PreviousSettle'], x['High'])) - min(c(x['PreviousSettle'], x['Low']))))
  av <- rollmeanr(f, span, fill = NA)
  return (av)
}

simulation_parameters <- function() {
  return (list(quick_ma_span = 50,
               slow_ma_span = 100,
               signal_span = 50,
               atr_span = 50,
               atr_exit_factor = 3))
}

# Simulate trading for a series.  Returns a list of trades that would be executed.
run_simulation <- function(series, parameters) {
  # Frame that will hold trades we enter and exit
  trades <- data.frame("position" = double(), "date_in" = as.Date(character()), "settle_in" = double(), "date_out" = as.Date(character()), "settle_out" = double(), "pnl" = double(), stringsAsFactors = FALSE)    
  
  # Trend is 0 if flat, 1 if rising, -1 if falling.
  Trend <- trend_from_moving_average(series, parameters$quick_ma_span, parameters$slow_ma_span)

  LongSignal <- min_max_price_signal(series, parameters$signal_span, TRUE)
  ShortSignal <- min_max_price_signal(series, parameters$signal_span, FALSE)
  ATR <- average_true_range(series, parameters$atr_span)

  # Merge all of the above into a single series
  all <- merge(series, Trend, LongSignal, ShortSignal, ATR)

  position <- 0
  threshold <- 0.0
  for(ii in 1:nrow(all)) {
    row <- coredata(all)[ii,]
    dt <- index(all[ii,])
    settle <- as.double(row['AdjustedSettle'])
    atr <- as.double(row['ATR'])
    
    if (position != 0) {
      exit <- FALSE
      if (position > 0) {
        # In a long position
        threshold <- max(c(threshold, settle))
        if (settle < threshold - parameters$atr_exit_factor * atr) {
          exit <- TRUE
        }
      } else {
        # In a short position
        threshold <- min(c(threshold, settle))
        if (settle > threshold + parameters$atr_exit_factor * atr) {
          exit <- TRUE
        }
      }

      if (exit) {
        rn <- nrow(trades)
        trades[rn, "date_out"] <- dt
        trades[rn, "settle_out"] <- row["AdjustedSettle"]
        trades[rn, "pnl"] <- calc_trade_pnl(series, trades[rn, "date_in"], trades[rn, "date_out"], position)
        position <- 0
      }
    } else {
      # Not in a position
      long_signal <- as.logical(row['LongSignal'])
      short_signal <- as.logical(row['ShortSignal'])
      trend <- as.integer(row['Trend'])
      
      if (long_signal & trend == 1) {
        position <- 1
      } else if (short_signal & trend == -1) {
        position <- -1
      }
                
      if (position != 0) {
        threshold <- settle
        
        # Add empty row
        rn <- nrow(trades) + 1
        trades[rn,] <- rep(NA, ncol(trades))
        
        trades[rn, "date_in"] <- dt
        trades[rn, "position"] <- position
        trades[rn, "settle_in"] <- row['AdjustedSettle']
      }
    }
  }

  # Exit position if necessary at end of simulation
  if (position != 0) {
    rn <- rn <- nrow(trades)
    trades[rn, "date_out"] <- dt
    trades[rn, "settle_out"] <- row["AdjustedSettle"]
    trades[rn, "pnl"] <- calc_trade_pnl(series, trades[rn, "date_in"], trades[rn, "date_out"], position)
  }
      
  return (trades)
}

# -----------------------------------------------------------------------------
# Test data functions ---------------------------------------------------------

is_test_symbol <- function(symbol) {
  return (length(grep('^TEST', symbol) == 1))
}

is_weekend <- function(dd) {
  as.POSIXlt(as.Date(dd))$wday %in% c(0,6)
}

test_data_parameters <- function() {
  return (list (  seed_date       = '1995-01-01', # Start all test data generation from same point for consitency.
                  end_date        = '2015-01-01',
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

# -----------------------------------------------------------------------------
# Plotting functions ----------------------------------------------------------

plot_values <- function(series, value) {
  palate = c('blue','red','green')
  par(col='black')
  plot(as.Date(series[[1]][,'Date']), series[[1]][,value], type='n')
  for (ii in 1:length(series)) {
    par(col=palate[(ii-1) %% length(palate) + 1])
    lines(as.Date(series[[ii]][,'Date']), series[[ii]][,value], type='l')
  }
}

plot_series <- function(series) {
  plot(series$AdjustedSettle, col = "black")
}

plot_trades <- function(series, trades) {
  plot_series(series)
  long_trades = trades[trades$position > 0,]
  short_trades = trades[trades$position < 0,]
  points(as.Date(long_trades[,'date_in']), long_trades[,'settle_in'], pch=24, col='green', bg='green', cex=0.75)
  points(as.Date(long_trades[,'date_out']), long_trades[,'settle_out'], pch=25, col='red', bg='red', cex=0.75)   
  points(as.Date(short_trades[,'date_in']), short_trades[,'settle_in'], pch=25, col='green', bg='green', cex=0.75)
  points(as.Date(short_trades[,'date_out']), short_trades[,'settle_out'], pch=24, col='red', bg='red', cex=0.75)   
}

