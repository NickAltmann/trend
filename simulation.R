# -----------------------------------------------------------------------------
# Simulation functions --------------------------------------------------------

library(zoo)

traded_price <- function(price, source, ticker, long_not_short) {
  # Want to put in adjustments for slippage and spread.
  return (price)
}

# Start and end date are the days of the signal, ie day before the trade is done.
# Returns a data.frame with a single row in the format of the trades frame.
calc_trade_pnl <- function(series, start_date, end_date, position, point_value, tick_size, currency, parameters) {
  factor <- position * point_value
  
  settle_in  <- as.double(series[start_date, "AdjustedSettle"])
  settle_out <- as.double(series[end_date,   "AdjustedSettle"])
  
  in_price  <- as.double(series[start_date, "NextOpen"])
  out_price <- as.double(series[end_date,   "NextOpen"])
  
  # All prices are mid-prices.  
  # In particular the Open price that we assume we trade at is a mid-price.
  # Make the (heroic) assumption that all markets trade with a bid-ask spread of
  # parameters$spread_ticks either side of the mid (ie the spread is double spread_ticks).
  # Calculate the spread we pay each time we do a trade.
  spread_cost <- tick_size * parameters$spread_ticks * abs(factor)
  
  # Find the rolls that will need to be done in across the duration of the trade.
  rolls_out <- series[index(series) >= start_date & index(series) <  end_date & series$RollSell == 1,]
  rolls_in  <- series[index(series) >  start_date & index(series) <= end_date & series$RollBuy  == 1,]
  
  # Find how many trades are done in total, rolls plus entry and exit.
  trade_count <- nrow(rolls_out) + nrow(rolls_in) + 2
  
  # Find what we pay for the roll trades.
  if (nrow(rolls_out) > 0) {
    roll_out <- sum(rolls_out[,"NextOpen"])
    roll_in  <- sum(rolls_in[, "Open"])
  } else {
    roll_out <- 0
    roll_in <- 0
  }
  
  # Theoretical gain based just on our adjusted settlement price.
  theoretical_gain <- (settle_out - settle_in) * factor
  
  # Gain based on open prices, ignoring bid-ask spread
  open_gain <- (out_price - in_price + roll_out - roll_in) * factor
  
  # Slippage is cost due to not executing at the settle mid-price that triggers the signal
  slippage <- theoretical_gain - open_gain
  
  # Cost due to spread.
  bid_ask_cost <- trade_count * spread_cost
  
  # Use FX rate on exit, simple if not very realistic.
  fx_rate <- get_exchange_rate(currency, end_date)
  
  dollar_theoretical_gain <- theoretical_gain * fx_rate
  dollar_slippage <- slippage * fx_rate
  dollar_bid_ask_cost <- bid_ask_cost * fx_rate
  
  transaction_fees <- trade_count * abs(position) * parameters$transaction_fee
  
  pnl <- (open_gain - bid_ask_cost) * fx_rate - transaction_fees
  
  trade <- data.frame(position = position, 
                      date_in = start_date, 
                      date_out = end_date, 
                      settle_in = settle_in, 
                      settle_out = settle_out,
                      native_theoretical_gain = theoretical_gain,
                      dollar_theoretical_gain = dollar_theoretical_gain,
                      dollar_slippage = dollar_slippage,
                      dollar_bid_ask_cost = dollar_bid_ask_cost,
                      dollar_fees = transaction_fees,
                      dollar_pnl = pnl, 
                      stringsAsFactors = FALSE)    
  
  return (trade)
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
  return (zoo(t, index(series)))
}

true_range <- function(series) {
  f <- apply(series, 1, function(x) return(max(c(x['PreviousSettle'], x['High'])) - min(c(x['PreviousSettle'], x['Low']))))
  return (zoo(f, index(series)))
}

settle_range <- function(series) {
  f <- apply(series, 1, function(x) return(abs(x['PreviousSettle'] - x['Settle'])))
  return (zoo(f, index(series)))
}

average_settle_range <- function(series, span) {
  f <- apply(series, 1, function(x) return(x['PreviousSettle'] - x['Settle']))
  av <- rollmeanr(f, span, fill = NA)
  return (zoo(av, index(series)))
}

average_true_range <- function(series, span) {
  f <- apply(series, 1, function(x) return(max(c(x['PreviousSettle'], x['High'])) - min(c(x['PreviousSettle'], x['Low']))))
  av <- rollmeanr(f, span, fill = NA)
  return (zoo(av, index(series)))
}

average_true_range_exp <- function(series, span) {
  f <- apply(series, 1, function(x) return(max(c(x['PreviousSettle'], x['High'])) - min(c(x['PreviousSettle'], x['Low']))))
  av <- EMA(f, n=span)
  return (zoo(av['EMA'], index(series)))
}

contracts_to_trade <- function(atr, equity, risk_factor, point_value, currency, date) {
  exchange_rate = get_exchange_rate(currency, date)
  equity = equity / exchange_rate
  contracts = max(c(floor((equity * risk_factor) / (atr * point_value)), 1))
  return (contracts)
}

simulation_parameters <- function() {
  return (list(quick_ma_span = 50,
               slow_ma_span = 100,
               signal_span = 50,
               atr_span = 50,
               atr_exit_factor = 3,
               equity = 2000000,
               risk_factor = 0.1,
               transaction_fee = 20,
               spread_ticks = 1))
}

# Simulate trading for a series.  Returns a list of trades that would be executed.
# eg
# > ftse_series = create_future_time_series('FTSE 100', 1984, 2014)
# > trades = run_simulation(ftse_series, simulation_parameters())

run_simulation <- function(series, parameters = NULL) {
  if (is.null(parameters)) {
    parameters = simulation_parameters()
  }
  
  full_series = series$full_series
  point_value = get_point_value(series$source, series$ticker)
  tick_size = get_tick_size(series$source, series$ticker)
  currency = get_currency(series$source, series$ticker)
  risk_factor = parameters$risk_factor
  equity = parameters$equity
  trades <- NA
  
  # Trend is 0 if flat, 1 if rising, -1 if falling.
  Trend <- trend_from_moving_average(full_series, parameters$quick_ma_span, parameters$slow_ma_span)
  
  LongSignal <- min_max_price_signal(full_series, parameters$signal_span, TRUE)
  ShortSignal <- min_max_price_signal(full_series, parameters$signal_span, FALSE)
  ATR <- average_true_range(full_series, parameters$atr_span)
  
  # Merge all of the above into a single series
  all <- merge(full_series, Trend, LongSignal, ShortSignal, ATR)
  
  position <- 0
  threshold <- 0.0
  current_year <- 0
  start_year <- 0
  for(ii in 1:nrow(all)) {
    row <- coredata(all)[ii,]
    dt <- index(all[ii,])
    settle <- as.double(row['AdjustedSettle'])
    atr <- as.double(row['ATR'])

    # Keep track of year just to show progress and to add to output result    
    yr <- as.numeric(format(as.yearmon(dt), "%Y"))
    if (yr != current_year) {
      current_year <- yr
      print (current_year)
    }
    if (start_year == 0) {
      start_year <- yr
    }
    
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
        trade_row <- calc_trade_pnl(full_series, date_in, dt, position, point_value, tick_size, currency, parameters)
        if (is.object(trades)) {
          trades <- rbind(trades, trade_row)
        } else {
          trades <- trade_row
        }
        position <- 0
      }
    } else {
      # Not in a position
      long_signal <- as.logical(row['LongSignal'])
      short_signal <- as.logical(row['ShortSignal'])
      trend <- as.integer(row['Trend'])
      
      if (long_signal & trend == 1) {
        position <- contracts_to_trade(atr, equity, risk_factor, point_value, currency, dt)
      } else if (short_signal & trend == -1) {
        position <- -contracts_to_trade(atr, equity, risk_factor, point_value, currency, dt)
      }
      
      if (position != 0) {
        # Entering a new trade.
        threshold <- settle
        date_in <- dt
      }
    }
  }
  
  # Exit position if necessary at end of simulation
  if (position != 0) {
    trade_row <- calc_trade_pnl(full_series, date_in, dt, position, point_value, tick_size, currency, parameters)
    if (is.object(trades)) {
      trades <- rbind(trades, trade_row)
    } else {
      trades <- trade_row
    }
  }
  
  return (list(trades = trades,
               start_year = start_year,
               end_year = current_year,
               description = series$description,
               parameters = parameters))
}

run_simulations <- function(list_of_series, parameters = NULL) {
  res <- lapply(list_of_series, function (x) return (run_simulation(x, parameters)))
  names(res) <- names(list_of_series)
  return (res)
}
