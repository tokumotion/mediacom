# Install required packages if not installed
if (!require("quantmod")) {
  install.packages("quantmod")
}
if (!require("lubridate")) {
  install.packages("lubridate")
}

# Load required libraries
library(quantmod)
library(lubridate)

# Set API key for Alpha Vantage
# Replace "your_api_key" with your actual Alpha Vantage API key
av_api_key <- "your_api_key"

# Define NASDAQ and Dow Jones stock symbols
nasdaq_symbols <- c("AAPL", "GOOGL", "AMZN", "FB", "MSFT")
dowjones_symbols <- c("IBM", "JNJ", "JPM", "MCD", "WMT")

# Function to fetch stock data
fetch_stock_data <- function(symbols, api_key) {
  stock_data <- list()
  
  for (symbol in symbols) {
    stock_data[[symbol]] <- getSymbols.av(symbol, src = "av", 
                                          api.key = api_key, 
                                          auto.assign = FALSE)
    Sys.sleep(15) # Sleep for 15 seconds to avoid API rate limits
  }
  
  return(stock_data)
}

# Function to continuously update stock data
update_stock_data <- function(nasdaq_symbols, dowjones_symbols, api_key) {
  while (TRUE) {
    cat("Fetching stock data...\n")
    nasdaq_data <- fetch_stock_data(nasdaq_symbols, api_key)
    dowjones_data <- fetch_stock_data(dowjones_symbols, api_key)
    
    # Process and save the data as desired
    # ...
    
    cat("Stock data fetched at:", Sys.time(), "\n")
    cat("Waiting for the next update...\n")
    
    # Wait for 5 minutes before fetching the data again
    Sys.sleep(5 * 60)
  }
}

# Call the function to start fetching stock data
update_stock_data(nasdaq_symbols, dowjones_symbols, av_api_key)
