#' Gets historical stock data from Yahoo Finance
#' 
#'  @param symbol - Yahoo stock ticker symbol
#'  @param endMon - ending month
#'  @param endDay - ending day
#'  @param endYear - ending year
#'  @param startMon - start month
#'  @param startDay - start day
#'  @param startYear - start year
#'  @param cacheDir - cache directory
#'  @param useCache - use cached data if available
#'  @return a data frame of historical stock data
#'  @import stringr
#'  @author Phillip Chin
#'  @details
#'  Based on a stock symbol and start and end dates, historical stock data is retrieved
#'  from Yahoo Finance.  If the start date isn't supplied, the default start date of 
#'  1/1/2010 will be used.  If the useCache is true, a cached copy of the data will 
#'  be saved the first time the data is retrieved.  Following calls will use the data
#'  from the cacheDir.
#'  
#'  The returned data frame will have the following columns:
#'  Date   Open   High    Low  Close   Volume Adj.Close 
#'  @examples
#'  # get data for Apple from 1/1/2010 - 1/1/2015
#'  getYahooStockData('AAPL', 1, 1, 2015)
#'  
#'  
#'  @export
getYahooStockData <- function(symbol, endMon, endDay, endYear,
                              startMon = 1, startDay = 1, startYear = 2010,
                              cacheDir = 'stockDataCache', useCache = FALSE) {
  # gets csv data from Yahoo Finance
  if (useCache && !dir.exists(cacheDir)) {
    dir.create(cacheDir)
  }
  
  # fix zero start for month
  startMon <- startMon - 1
  endMon <- endMon - 1
  
  startMonStr <- str_pad(as.character(startMon), 2, pad = '0')
  startDayStr <- str_pad(as.character(startDay), 2, pad = '0')    
  startDateStr <- paste(startYear, startMonStr, startDayStr, sep = '') 
  
  endMonStr <- str_pad(as.character(endMon), 2, pad = '0')
  endDayStr <- str_pad(as.character(endDay), 2, pad = '0')
  endDateStr <- paste(endYear, endMonStr, endDayStr, sep = '')
  
  if (grepl("\\^", symbol)) {
    # convert caret to urlencoded symbol
    symbol <- gsub('^', '%5E', symbol)
  }
  
  if (grepl('%5E', symbol)) {
    # replace caret for the file name
    filenameSymbol <- gsub('%5E', '_', symbol) 
  } else {
    filenameSymbol <- symbol
  }
  
  filename <- paste(filenameSymbol, startDateStr, endDateStr, 
                    "stockData.Rds", sep = '_')
  fullFilename <- paste(cacheDir, filename, sep = '/')
  
  if (useCache && file.exists(fullFilename)) {
    # Read the model in and assign it to a variable.
    stockData <- readRDS(fullFilename)
  } else {
    stockDataUrl <- paste('http://real-chart.finance.yahoo.com/table.csv?s=', 
                          symbol, 
                          '&a=', startMonStr, 
                          '&b=', startDay, 
                          '&c=', startYear, 
                          '&d=', endMonStr, 
                          '&e=', endDay, 
                          '&f=', endYear, 
                          '&g=d&ignore=.csv', sep = '')
    stockData <- read.csv(url(stockDataUrl))
    
    stockData$Date <- as.POSIXct(stockData[, 'Date']) 
    
    if (useCache) {
      saveRDS(stockData, fullFilename)
    }
  }  
  
  return(stockData)
}