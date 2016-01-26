# -----------------------------------------------------------------------------
# Future series loading functions ---------------------------------------------

source('quandl.R')
source('test_data.R')

# Config file in working directory given contract details.
future_specs_file <- "contract_specs.csv"

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

# Get the time series data
get_future_data_from_symbol <- function(symbol) {
  # Check if this test data.
  if (is_test_symbol(symbol)) {
    mydata = (create_test_data_from_symbol(symbol))        
  } else {
    
    mydata <- load_timeseries_data(symbol)
    
    # Get consistent name for open interest column    
    open_interest_column <- grep("Interest",names(mydata))
    names(mydata)[open_interest_column] <- "OpenInterest"
    
    mydata <- mydata[,c("Open","High","Low","Settle", "OpenInterest")]
    
    # Remove zero values.
    # mydata <- mydata[mydata$Open > 0 & mydata$Settle > 0 & mydata$OpenInterest > 0,]
    mydata <- mydata[mydata$Open > 0 & mydata$Settle > 0,]
  }
  
  if (nrow(mydata) > 2) {
    # Horrible hack.  
    # Hang Seng, at least from this source, is scaled by 0.1 up to 2008.
    # Drive this from config at some point.
    decon <- deconstruct_symbol(symbol)
    if (decon$source == 'PXDATA' & decon$ticker == 'H5' & decon$year <= 2008) {
      mydata$Low    <- mydata$Low * 10
      mydata$High   <- mydata$High * 10
      mydata$Open   <- mydata$Open * 10
      mydata$Settle <- mydata$Settle * 10
    }
    
    # Set High and Low if missing.
    mydata$Low  <- apply(mydata, 1, function(x) return (if (x['Low'] > 0) x['Low'] else min(c(x['Settle'], x['Open']))))
    mydata$High <- apply(mydata, 1, function(x) return (if (x['High'] > 0) x['High'] else max(c(x['Settle'], x['Open']))))
    
    # Add a couple of derived fields we will need later.    
    PreviousSettle <- lag(mydata, -1)[,"Settle"]
    mydata = merge(mydata, PreviousSettle)
    NextOpen <- lag(mydata, 1)[,"Open"]
    mydata = merge(mydata, NextOpen)
    
    mydata = tail(head(mydata, -1),-1)
    
    print (symbol)
  } else {
    mydata = NA
  }
  
  return (mydata)
}

future_data_exists <- function(source, ticker, month_in_year, year) {
  symbol = form_future_symbol(source, ticker, month_in_year, year)
  return (future_data_exists_from_symbol(symbol))
}

future_data_exists_from_symbol <- function(symbol) {
  if (file.exists(missing_series_file)) {
    missing_series <- read.csv(file=missing_series_file,head=TRUE,sep=",", stringsAsFactors=FALSE)     
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
  exists <- exists_on_quandl(symbol)
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

read_future_specs <- function(remove_test = FALSE) {
  future_specs <- read.csv(file=future_specs_file, head=TRUE, sep=",", stringsAsFactors=FALSE)
  if (remove_test) {
    future_specs <- future_specs[future_specs$Exchange != "TEST",]
  }
  future_specs <- future_specs[!is.na(future_specs$Start),]
  return (future_specs)
}

get_future_spec_row <- function(source, ticker) {
  future_specs <- read_future_specs()
  row <- future_specs[which(future_specs$Symbol==ticker & future_specs$Exchange==source), ]
  if (nrow(row) != 1) {
    stop('No future spec available')
  }
  return (row)
}

get_future_symbol_from_description <- function(description) {
  future_specs <- read_future_specs()
  row <- future_specs[which(future_specs$Description == description), ]
  if (nrow(row) != 1) {
    stop('No future spec available')
  }
  return (list(exchange = row$Exchange, symbol = row$Symbol))
}

get_point_value <- function(source, ticker) {
  return (get_future_spec_row(source,ticker)[,"PointValue"])
}

get_tick_size <- function(source, ticker) {
  return (get_future_spec_row(source,ticker)[,"TickSize"])
}

get_currency <- function(source, ticker) {
  return (get_future_spec_row(source,ticker)[,"Currency"])
}

get_normal_months <- function(source, ticker) {
  month_string <- get_future_spec_row(source,ticker)[,"Months"]
  months <- 1:12
  if (month_string != "") {
    months <- (as.numeric(strsplit(as.character(month_string),",")[[1]]))
  }
  return (months)
}
