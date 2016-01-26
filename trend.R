library(zoo)
library(TTR)

source('common.R')
source('test_data.R')
source('plotting.R')
source('quandl.R')
source('exchange_rate.R')
source('future_loading.R')
source('future_rolling.R')
source('simulation.R')



# -----------------------------------------------------------------------------
# Post-simulation functions ---------------------------------------------------

cumulative_pnl <- function(trades) {
  cum_pnl <- zoo(cumsum(trades$dollar_pnl), trades$date_out)
  max_pnl <- cummax(cum_pnl)
  drawdown <- max_pnl - cum_pnl

  return (merge.zoo(cum_pnl, max_pnl, drawdown))
}

trade_summary_by_sector <- function(list_of_series)

trade_list_summary <- function(list_of_trades, start_year, end_year) {
  # Create a combined data frame of summaries.
  res <- Reduce(function(x,y) return (rbind(x, trades_summary(y, start_year, end_year))), list_of_trades, NULL)
  
  # Label the rows
  row.names(res) <- names(list_of_trades)
  
  return (res)
}

filter_trades_from_details <- function(trade_details, start_year, end_year) {
  if (!is.null(start_year)) {
    actual_start_year <- max(start_year, trade_details$start_year)
  } else {
    actual_start_year <- trade_details$start_year
  }
  if (!is.null(end_year)) {
    actual_end_year <- min(end_year, trade_details$end_year)
  } else {
    actual_end_year <- trade_details$end_year
  }
  trades <- trade_details$trades[trade_details$trades$date_out >= as.Date(ISOdate(actual_start_year, 1, 1)) & 
                                   trade_details$trades$date_out <= as.Date(ISOdate(actual_end_year, 12, 31)),]
  return (list(trades = trades, start_year = actual_start_year, end_year = actual_end_year))
}

# Convert to an annual return.  Without compounding just an average.
annual_return <- function(pnl, equity, years) {
  annual_return <- pnl / (equity * years)
  return (annual_return)
}

# How to calculate compound return.  Not used.
compound_annual_return <- function(pnl, equity, years) {
  if (pnl > -equity) {
    total_return <- pnl / equity + 1
    annual_return <- exp(log(total_return) / years) - 1
  } else {
    annual_return <- NA
  }
  return (annual_return)
}

# Summary of a single series of trades.
trades_summary <- function(trade_details, start_year = NULL, end_year = NULL) {
  filtered_trades <- filter_trades_from_details(trade_details, start_year, end_year)
  trades <- filtered_trades$trades
  start_year <- filtered_trades$start_year
  end_year <- filtered_trades$end_year
  
  trade_count <- nrow(trades)
  
  if (trade_count > 0) {
    cum <- cumulative_pnl(trades)
    max_drawdown <- max(cum$drawdown)
  } else {
    max_drawdown <- 0
  }
  
  theo <- sum(trades$dollar_theoretical_gain)
  actual <- sum(trades$dollar_pnl)

  # Years are inclusive
  years <- end_year - start_year + 1
  
  winning_trades <- nrow(trades[trades$dollar_pnl > 0,])
  short_trades <- nrow(trades[trades$position < 0,])

  equity <- trade_details$parameters$equity
  annual <- annual_return(actual, equity, years)

  if (actual > 0) {
    drawdown_years <- max_drawdown / (equity * annual)
  } else {
    drawdown_years <- 0
  }
  
  slippage <- sum(trades$dollar_slippage)
  spread_cost <- sum(trades$dollar_bid_ask_cost)
  fees <- sum(trades$dollar_fees)
  
  return (data.frame(annual_return = round(annual * 100, 2),
                     actual_pnl = round(actual),
                     theo_pnl = round(theo), 
                     slippage = round(slippage),
                     spread_cost = round(spread_cost),
                     fees = fees,
                     trades = trade_count,
                     winners = winning_trades,
                     shorts = short_trades,
                     max_drawdown = round(max_drawdown),
                     drawdown_years = round(drawdown_years, 2),
                     duration = years))
}

# Combine the pnl from a list of trades.
# Creates a sparse zoo vector holding the pnl for each relevant day.
# PnL is assigned to the day of the signal for closing the trade.
combined_pnl <- function(list_of_trade_series, start_year = NULL, end_year = NULL) {
  # Create list of zoo vectors
  pnl_vecs <- lapply(list_of_trade_series, 
                     function (x) {
                       filtered_details <- filter_trades_from_details(x, start_year, end_year)
                       trades <- filtered_details$trades
                       return (zoo(trades[,'dollar_pnl'], trades[,'date_out']))
                     })

  combined_pnl_series <- Reduce(function (x,y) 
    return(merge(x, y, fill = 0)), 
    pnl_vecs)
  
  summed_pnl <- zoo(rowSums(combined_pnl_series), index(combined_pnl_series))
  
  return (summed_pnl)
}

combined_cumulative_pnl <- function(list_of_trade_series, start_year = NULL, end_year = NULL) {
  combined <- combined_pnl(list_of_trade_series, start_year, end_year)
  cum_pnl <- cumsum(combined)
  max_pnl <- cummax(cum_pnl)
  drawdown <- max_pnl - cum_pnl
  
  return (merge.zoo(cum_pnl, max_pnl, drawdown))
}

pnl_summary_by_sector <- function(list_of_trade_series, start_year, end_year) {
  specs = read_future_specs(TRUE)
  sectors <- unique(specs$Sector)
  res <- lapply(sectors, function (x) {
    # Pull out the descriptions for the series in the give sector.
    descriptions <- specs[specs$Sector == x,'Description']
    
    # Generate a summary for just that set of descriptions.
    return (pnl_summary(list_of_trade_series[descriptions], start_year, end_year))
  })
  df <- Reduce(function (x,y) return(rbind(x,y)), res) 
  row.names(df) <- sectors
  df$return <- 100 * df$return
  return (df)
}

pnl_summary <- function(list_of_trade_series, start_year, end_year) {
  # Get the total pnl
  cum_pnl <- combined_pnl(list_of_trade_series, start_year, end_year)
  pnl <- sum(cum_pnl)
  
  # Get the total equity in force between the give dates, pro-rating where necessary
  years <- (end_year - start_year + 1)
  equity <- sum(sapply(list_of_trade_series,
                   function (x) {
                     series_start <- max(start_year, x$start_year)
                     series_end <- min(end_year, x$end_year)
                     span <- max(series_end - series_start + 1, 0)
                     equity <- x$parameters$equity * span / years
                     return (equity)
         }))
  
  annual_return <- annual_return(pnl, equity, years)
    
  return (data.frame(return = annual_return, pnl = pnl, equity = equity))
}

compounded_returns <- function(list_of_trade_series, start_year, end_year, fixed = .015, perf = 0.15) {
  returns <- sapply(start_year:end_year, function (x) return (as.numeric(pnl_summary(sim, x, x)['return'])))
  after_fees <- sapply(returns, function (x) return (x - fixed - (if (x > 0) x * perf else 0)))
  compounded <- cumprod(1 + after_fees)
  return (data.frame(year = start_year:end_year,
                     return = round(returns * 100, 2),
                     compounded = compounded))
}

# Check details for a given day for given series.
inspect_date <- function(series, dt) {
  # Get future in force on date.
  roll_info <- series$roll_details[series$roll_details$start_date <= dt &
                                   series$roll_details$end_date >= dt,]
  print (roll_info)
  
  # Print details for the given day.
  print (series$future_series[[roll_info$code]][dt,])
}

future_slope <- function() {
  sort(round(100 * sapply(full_series_set, function (x) {
    return (nrow(x$roll_details[x$roll_details$crossover_gap < 0,]) 
            / nrow(x$roll_details))
    }), 1))
}
