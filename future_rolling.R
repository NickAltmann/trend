# -----------------------------------------------------------------------------
# Future rolling functions ----------------------------------------------------

library(zoo)

source('exchange_rate.R')
source('future_loading.R')


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
      # If there is no value here for earlier series, give up.
      if (last_earlier_series_date %in% index(later_series)) {
        cross_over_date <- last_earlier_series_date
      } else {
        cross_over_date <- NA
      }
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

# Helper to manage a list of future series.
# Idea is to hold on to series that have already been loaded.
# Will only add if series exists for symbol and series has rows.
add_future_series_to_list <- function(future_series_list, symbol) {
  if (!symbol %in% names(future_series_list)) {
    if (future_data_exists_from_symbol(symbol)) {
      s = get_future_data_from_symbol(symbol)
      if (is.zoo(s)){
        if (nrow(s) > 0) {
          future_series_list[[length(future_series_list)+1]] = s
          names(future_series_list)[length(future_series_list)] = symbol
        }
      }
    }
  }
  return (future_series_list)
}

# Create future time series based on the start and end dates in the spec file 
# or user supplied dates.
# Will check availability of exchange rate where appropriate and trim dates if necessary.
create_future_time_series <- function(description, start_year = NULL, end_year = NULL) {
  
  identifiers <- get_future_symbol_from_description(description)
  source = identifiers$exchange
  ticker = identifiers$symbol
  
  spec_row <- get_future_spec_row(source, ticker)

  # Start year is max of passed in, the future spec or the earliest currency year  
  spec_start_year <- spec_row$Start
  if (is.null(start_year)) {
    start_year <- spec_start_year
  } else {
    start_year <- max(c(start_year, spec_start_year))
  }
  currency_start_year = get_currency_start_year(spec_row$Currency)
  start_year <- if (is.na(currency_start_year)) start_year 
                else max(c(start_year, currency_start_year))
  
  # End year is earliest of year passed in, year in spec, defaulting to last year.
  last_year <- as.numeric(format(as.yearmon(Sys.time()), "%Y")) - 1
  spec_end_year <- if (is.na(spec_row$End)) last_year else spec_row$End
  
  if (is.null(end_year)) {
    end_year <- spec_end_year
  } else {
    end_year <- min(c(spec_end_year, end_year))
  }
  
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
    symbol <- form_future_symbol(source, ticker, month, year)
    future_series <- add_future_series_to_list(future_series, symbol)
    
    if (symbol %in% names(future_series)) {
      # Found the first series, now check it doesn't roll before the start date.
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
    next_symbol <- form_future_symbol(source, ticker, next_month, next_year)
    future_series <- add_future_series_to_list(future_series, next_symbol)
    
    if (next_symbol %in% names(future_series)) {
      current_month <- months[month_index]
      current_symbol <- form_future_symbol(source, ticker, current_month, year)
      current_series <- future_series[[current_symbol]]
      
      next_series <- future_series[[next_symbol]]
      
      cross_over_date <- find_cross_over_date(current_series, next_series, start_date)
      
      if (!is.na(cross_over_date)) {
        
        cross_over_gap <- find_cross_over_gap(current_series, next_series, cross_over_date)
        
        s <- current_series[index(current_series) >= start_date & index(current_series) < cross_over_date,] # Don't want cross_over_date itself
        if (nrow(s)) {
          res <- add_roll(current_symbol, s, cross_over_gap, full_series, roll_details)
          full_series <- res$full_series
          roll_details <- res$roll_details
          start_date <- cross_over_date
        }
        
        if (is.zoo(full_series)) {
          full_series$AdjustedSettle <- full_series$AdjustedSettle + cross_over_gap  
        }
        
      } else {
        if (is.zoo(full_series)) {
          start_date <- end(full_series) + 1
        } else {
          start_date <- start(next_series)
        }
      }
      year <- next_year
      month_index <- next_month_index
    }
    
    # Break if we've reached the end or run out of months in the year after the end date.
    if  (next_year > end_year) {
      if ( ((next_symbol %in% names(future_series)) & end(next_series) > end_date) 
           | next_month_index >= length(months) ) {
        break
      }
    }
  }
  
  if (next_symbol %in% names(future_series)) {
    s = window(next_series, start = cross_over_date, end = end_date)
    if (nrow(s)) {
      res <- add_roll(next_symbol, s, 0, full_series, roll_details)
      full_series <- res$full_series
      roll_details <- res$roll_details
    }
  } 
  
  return (list(description = description,
               full_series=full_series, 
               future_series=future_series, 
               roll_details=roll_details,
               source=source,
               ticker=ticker,
               start_year = start_year,
               end_year = end_year))
}

# Helper to perform the roll operation from one series to the next.
# Returns the updated series and roll details.
add_roll <- function(new_symbol, new_series, crossover_gap, full_series, roll_details) {
  new_series_start <- start(new_series)
  new_series_end <- end(new_series)
  new_series$RollBuy <- 0
  new_series$RollSell <- 0
  new_series$RollBuy[new_series_start] <- 1
  new_series$RollSell[new_series_end] <- 1
  new_series$AdjustedSettle <- new_series$Settle
  roll_detail <- data.frame(code=new_symbol, 
                            start_date=new_series_start, 
                            end_date=new_series_end, 
                            crossover_gap=crossover_gap,
                            stringsAsFactors=FALSE)
  
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

# Returns a list indexed by future description (from the spec file).
# Each item in the list represents the series object for the future, 
# returned from a call to create_future_time_series
load_future_series <- function(descriptions, start = NULL, end = NULL) {
  res <- lapply(descriptions, function (x) return (create_future_time_series(x, start, end)))
  names(res) <- descriptions
  return (res)
}

load_all_future_series <- function(start = NULL, end = NULL) {
  specs <- read_future_specs()
  descriptions <- specs[which(specs$Symbol != 'TEST' & !is.na(specs$Start)),"Description"]
  return (load_future_series(descriptions, start, end))
}
