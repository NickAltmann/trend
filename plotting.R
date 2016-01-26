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

# Simple plot of a series over time.
# eg 
# > ftse_series = create_future_time_series(1984, 2014, 'LIFFE', 'Z')
# > plot_series(ftse_series$full_series)
#
plot_series <- function(series, title = "Series") {
  plot(series$AdjustedSettle, col = "black", main = title, xlab = "Date", ylab = "Price")
}

plot_trades <- function(series_details, trade_details, start = NULL, end = NULL, title = NULL, cex = 0.75) {
  if (is.null(title)) {
    title <- trade_details$description
  }
  plot_series(window(series_details$full_series, start=start, end=end), title)
  trades <- trade_details$trades
  window_trades = trades[(if (is.null(start)) TRUE else trades$date_in >= start) & (if (is.null(end)) TRUE else trades$date_out <= end),]
  long_trades = window_trades[window_trades$position > 0,]
  short_trades = window_trades[window_trades$position < 0,]
  points(as.Date(long_trades[,'date_in']), long_trades[,'settle_in'], pch=24, col='forestgreen', bg='forestgreen', cex=cex)
  points(as.Date(long_trades[,'date_out']), long_trades[,'settle_out'], pch=25, col='red3', bg='red3', cex=cex)   
  points(as.Date(short_trades[,'date_in']), short_trades[,'settle_in'], pch=25, col='forestgreen', bg='forestgreen', cex=cex)
  points(as.Date(short_trades[,'date_out']), short_trades[,'settle_out'], pch=24, col='red3', bg='red3', cex=cex)   
}

plot_trades_from_lists <- function(series_list, trades_list, description, start = NULL, end = NULL) {
  series <- series_list[[description]]
  trades <- trades_list[[description]]
  plot_trades(series$full_series, trades, start, end)
}

