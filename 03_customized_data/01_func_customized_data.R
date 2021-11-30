
###########################################################################
###########################################################################
###########################################################################
# 
#  Functions to reform raw return, excess return and raw price
#  Functions to pick universe by exchange code and share code
#  Functions to calculate adjusted price and price difference
#  Functions to pick time frames
# 
###########################################################################
###########################################################################
###########################################################################

if (!require(tidyr)) { 
  install.packages("tidyr")
}
library(tidyr)
# spread() is in "tidyr"

if (!require(dplyr)) {
  install.packages("dplyr")
}
library(dplyr)
# filter() is in "dplyr"
 
if (!require(magrittr)){
  install.packages("magrittr")
}
library(magrittr)
# This provides "%>%" operater






###########################################################################
# Help functions:
# Function to check whether the return is in real value or percentage
# Function to get all available dates
###########################################################################

is_perc = function(data){
  # Check whether the return is in real value or in precentage
  # Return TRUE if the return is percentage, FALSE otherwise
  
  rg = range(data)
  ratio_1 = sum(abs(data) < 1) / length(data)
  cat("\nRange of data is:", rg[1], "to", rg[2], "\n")
  cat("Ratio of data within [-1, 1]: ", ratio_1, "\n")
  hist(data[data < 1])
  ratio_1 < 0.9
}






finite_or_na = function(dat, verbose=T, correct_if_not=F) {
  # Check all numeric and integer columns
  # Each element should be either finite or NA
  # No NULL allowed.
  
  if (!is.data.frame(dat)) {
    stop("Error: dat should be a data frame!")
  }
  
  report = T
  for (i in 1:ncol(dat)) {
    cur_dat = dat[, i, drop=F]
    
    if (verbose) {
      cat("\n", colnames(dat)[i], ": ", class(cur_dat[, 1]), ".", sep="")
    }
    
    if (class(cur_dat[, 1]) %in% c("numeric", "integer")) {
      good = sapply(cur_dat, is.finite)[, 1] | is.na(cur_dat)
      
      if (verbose) {
        cat(" All good? ", all(good), ".", sep="")
      }
      
      report = report & all(good)
      if (!report & !verbose & !correct_if_not) {
        return(F)
      }
      
      if (correct_if_not & !all(good)) {
        dat[!good, i] = NA
        good = sapply(cur_dat, is.finite)[, 1] | is.na(cur_dat)
        
        if (verbose) {
          cat(" Now corrected? ", all(good), sep="")
        }
      }
    }
  }
  
  if (correct_if_not) {
    return(dat)
  }
  
  if (verbose) {
    cat("\n")
  }
  
  report
}






get_time_all = function(secu, ff5, verbose=F){
  # Get all available dates
  
  time_all = as.integer(unique(secu$date))
  if (verbose) {
    cat("\nLength of security dates:", length(time_all))
  }
  
  time_all = intersect(time_all, as.integer(ff5$date))
  time_all = sort(time_all[is.finite(time_all)])
  time_all = as.character(time_all)
  
  if (verbose) {
    cat("\nLength of FF5 dates:", length(ff5$date))
    cat("\nLength of time_all:", length(time_all))
    cat("\nHead: ", head(time_all))
    cat("\nTail:", tail(time_all), "\n")
  }
  
  time_all
}






###########################################################################
# Functions to reform raw return, excess return and raw price 
###########################################################################

reform_raw_ret = function(secu, ff5, time_all, verbose=F){
  # Reform raw return into a dataframe with each row a date
  #   and each column an FF5, ETF, or stock

  # There should not be any NA or NULL in the time_all
  time_all = as.integer(time_all)
  stopifnot(all(is.finite(time_all)))

  # Reform securities and FF5
  data = subset(secu, date %in% time_all,
                select=c("permno", "date", "ret"))
  data = spread(data, key=permno, value=ret)
  
  ff5_names = c("rf", "mkt", "smb", "hml", "rmw", "cma")
  ff5 = subset(ff5, date %in% time_all,
               select=c("date", ff5_names))
  
  # Merge and remove rows with all NAs
  data = merge(ff5, data, by="date", all=T) %>%
    filter(Reduce("+", lapply(., is.na)) != ncol(.))
  
  # Use date as the rownames
  rownames(data) = data$date
  data$date = NULL

  # Separate the dataframe into FF5, ETF, stocks, stocks_bar
  ff5 = subset(data, select=ff5_names)
  etf = subset(data, select=intersect(
    colnames(data), as.character(etf_info$etf_class$permno)))
  stocks = subset(data, select=setdiff(
    colnames(data), union(colnames(ff5), colnames(etf))))
  colnames(etf) = paste("per", colnames(etf), sep="")
  colnames(stocks) = paste("per", colnames(stocks), sep="")
  
  # Get expand window accumulative mean for stocks
  stocks_bar = as.data.frame(apply(stocks, 2, cum_mean, na.ignore=T))
  
  # Check dimension and rownames
  time_all = as.character(time_all)
  if (verbose) {
    cat("\nDimension of FF5, ETF, stocks, stocks_bar, all: \n",
        dim(ff5), "\n", dim(etf), "\n", 
        dim(stocks), "\n", dim(stocks_bar), "\n", dim(data))
    cat("\nidentical(rownames(stocks_bar), time_all) ?",
        identical(rownames(stocks_bar), time_all), "\n")
  }

  list(ff5=ff5, etf=etf, stocks=stocks, stocks_bar=stocks_bar, 
       time_all=time_all)
}






raw_to_ex_ret = function(raw_ret, verbose=F){
  # Transform raw return to excess return
  
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  rf = raw_ret$ff5$rf

  ff5 = raw_ret$ff5[ff5_names] - rf
  stocks = raw_ret$stocks - rf
  etf = raw_ret$etf - rf
  time_all=raw_ret$time_all
  
  # Get expand window accumulative mean for stocks
  stocks_bar = as.data.frame(apply(stocks, 2, cum_mean, na.ignore=T))

  # Check dimension and rownames
  if (verbose) {
    cat("\nDimension of FF5, ETF, stocks, stocks_bar: \n",
        dim(ff5), "\n", dim(etf), "\n",
        dim(stocks), "\n", dim(stocks_bar))
    cat("\nidentical(rownames(stocks_bar), time_all) ?",
        identical(rownames(stocks_bar), time_all), "\n")
  }

  list(ff5=ff5, etf=etf, stocks=stocks, stocks_bar=stocks_bar,
       time_all=time_all)
}






reform_raw_prc = function(secu, time_all, verbose=F){
  # Reform raw price into a dataframe with each row a date
  #   and each column an ETF, or stock

  # There should not be any NA or NULL in the time_all
  time_all = as.integer(time_all)
  stopifnot(all(is.finite(time_all)))

  # Reform securities
  data = subset(secu, date %in% time_all,
                select=c("permno", "date", "prc"))
  data = spread(data, key=permno, value=prc)

  # Remove rows with all NAs
  data = data %>% filter(Reduce("+", lapply(., is.na)) != ncol(.))

  # Use date as the rownames
  rownames(data) = data$date
  data$date = NULL

  # Separate the dataframe into ETF, stocks, stocks_bar
  etf = subset(data, select=intersect(
    colnames(data), as.character(etf_info$etf_class$permno)))
  stocks = subset(data, select=setdiff(colnames(data), colnames(etf)))
  colnames(etf) = paste("per", colnames(etf), sep="")
  colnames(stocks) = paste("per", colnames(stocks), sep="")
  
  # Get expand window accumulative mean for stocks
  stocks_bar = as.data.frame(apply(stocks, 2, cum_mean, na.ignore=T))
  
  # Check dimension and rownames
  time_all = as.character(time_all)
  if (verbose) {
    cat("\nDimension of ETF, stocks, stocks_bar: \n",
        dim(etf), "\n", dim(stocks), "\n", dim(stocks_bar))
    cat("\nidentical(rownames(stocks_bar), time_all) ?",
        identical(rownames(stocks_bar), time_all), "\n")
  }

  list(etf=etf, stocks=stocks, stocks_bar=stocks_bar, time_all=time_all)
}






reform_mcap = function(secu, time_all, verbose=F){
  # Reform market capital (mcap) into a dataframe with
  #   each row a date and each column an ETF, or stock

  # There should not be any NA or NULL in the time_all
  time_all = as.integer(time_all)
  stopifnot(all(is.finite(time_all)))

  # Reform securities
  data = subset(secu, date %in% time_all,
                select=c("permno", "date", "mcap"))
  data = spread(data, key=permno, value=mcap)

  # # (Don't do this. Keep the dimensions same.)
  # # Merge and remove rows with all NAs
  # data = data %>% filter(Reduce("+", lapply(., is.na)) != ncol(.))

  # Use date as the rownames
  rownames(data) = data$date
  data$date = NULL

  # Separate the dataframe into ETF, stocks, stocks_bar
  etf = subset(data, select=intersect(
    colnames(data), as.character(etf_info$etf_class$permno)))
  stocks = subset(data, select=setdiff(colnames(data), colnames(etf)))
  colnames(etf) = paste("per", colnames(etf), sep="")
  colnames(stocks) = paste("per", colnames(stocks), sep="")
  
  # Get expand window accumulative mean for stocks
  stocks_bar = as.data.frame(apply(stocks, 2, cum_mean, na.ignore=T))
  
  # Check dimension and rownames
  time_all = as.character(time_all)
  if (verbose) {
    cat("\nDimension of ETF, stocks, stocks_bar: \n",
        dim(etf), "\n", dim(stocks), "\n", dim(stocks_bar))
    cat("\nidentical(rownames(stocks_bar), time_all) ?",
        identical(rownames(stocks_bar), time_all), "\n")
  }

  list(etf=etf, stocks=stocks, stocks_bar=stocks_bar, time_all=time_all)
}






###########################################################################
# Pick the universe by share code and exchange code
###########################################################################

pick_universe = function(stocks_info, etf_info, secu){
  # Pick the universe by share code and exchange code
  # 
  # Select securites with shrcd (share code):
  #   10 or 11 for stocks, 73 for ETF
  # Select securites with exchcd (exchange code): 1, 2, 3
  
  stocks_per = subset(stocks_info$stocks_class,
                      (shrcd %in% c(10, 11)) & (exchcd %in% 1:3),
                      select=c("permno"))[, 1, drop=T]
  etf_per = subset(etf_info$etf_class,
                   (shrcd == 73) & (exchcd %in% 1:3),
                   select=c("permno"))[, 1, drop=T]
  good_per = c(stocks_per, etf_per)
  secu = subset(secu, permno %in% good_per)
  secu
}






###########################################################################
# Form adjusted price and adjusted price difference
###########################################################################

vec_adj_prc = function(raw_ret, raw_prc) {
  # Help function to form adjusted price from raw prc and raw return
  # Here raw_prc and raw_ret should be 2 vectors
  
  fin_prc = which(is.finite(raw_prc))
  if (length(fin_prc) == 0) {
    return(raw_prc)
  }
  
  raw_ret = raw_ret + 1
  raw_ret[1:fin_prc[1]] = raw_prc[1:fin_prc[1]]
  
  cum_prod(raw_ret, na.ignore=T)
}






df_adj_prc = function(raw_ret, raw_prc=NULL) {
  # Help function to form adjusted price from raw prc and raw return
  # Here raw_prc and raw_ret should be 2 data frames of same dimension
  # and with the same rownames and colnames
  # If raw_prc is NULL, set initial price to be 1.
  
  if (is.null(raw_prc)) {
    raw_prc = data.frame(matrix(
      NA, ncol=ncol(raw_ret), nrow=nrow(raw_ret)))
    rownames(raw_prc) = rownames(raw_ret)
    colnames(raw_prc) = colnames(raw_ret)
    raw_prc[1, ] = 1
  }
  if (!is.data.frame(raw_prc) | !is.data.frame(raw_ret)) {
    stop("Error: raw_prc or raw_ret is not a data frame!")
  }
  if (!identical(colnames(raw_prc), colnames(raw_ret))) {
    stop("Error: colnames of raw_prc and raw_ret not completely match!")
  }
  if (!identical(rownames(raw_prc), rownames(raw_ret))) {
    stop("Error: rownames of raw_prc and raw_ret not completely match!")
  } 
  if (!finite_or_na(dat=raw_prc, verbose=F, correct_if_not=F) 
      | !finite_or_na(dat=raw_ret, verbose=F, correct_if_not=F)) {
    stop("Error: Not all values are finite or NA. May have NULL!")
  }
  
  adj_prc = as.data.frame(sapply(1:ncol(raw_ret), function(i) {
    vec_adj_prc(raw_ret[, i, drop=T], raw_prc[, i, drop=T])}))
  rownames(adj_prc) = rownames(raw_ret)
  colnames(adj_prc) = colnames(raw_ret)
  
  adj_prc
}






get_adj_prc = function(raw_ret, raw_prc, verbose=F) {
  # Form adjusted price using raw price and raw return
  
  adj_prc = list()
  for (dat_name in c("ff5", "etf", "stocks")) {
    adj_prc[[dat_name]] = df_adj_prc(
      raw_ret[[dat_name]], raw_prc[[dat_name]])
  }
  adj_prc$time_all = raw_ret$time_all
  
  # Get expand window accumulative mean for stocks
  adj_prc$stocks_bar = as.data.frame(apply(
    adj_prc$stocks, 2, cum_mean, na.ignore=T))
  
  # Check dimension and rownames
  if (verbose) {
    cat("\nDimension of FF5, ETF, stocks, stocks_bar, all: \n",
        dim(adj_prc$ff5), "\n", dim(adj_prc$etf), "\n", 
        dim(adj_prc$stocks), "\n", dim(adj_prc$stocks_bar))
    cat("\nRownames of stocks_bar and time_all identical?",
        identical(rownames(adj_prc$stocks_bar), adj_prc$time_all), "\n")
  }
  
  adj_prc
}






get_prc_diff = function(prc, verbose=F) {
  # Form the 1st order difference of the adjusted price
  prc_diff = list()
  for (dat_name in c("ff5", "etf", "stocks")) {
    prc_diff[[dat_name]] = as.data.frame(apply(
      prc[[dat_name]], 2, diff))
  }
  prc_diff$time_all = prc$time_all[-1]
  
  # Get expand window accumulative mean for stocks
  prc_diff$stocks_bar = as.data.frame(apply(
    prc_diff$stocks, 2, cum_mean, na.ignore=T))
  
  # Check dimension and rownames
  if (verbose) {
    cat("\nDimension of FF5, ETF, stocks, stocks_bar, all: \n",
        dim(prc_diff$ff5), "\n", dim(prc_diff$etf), "\n", 
        dim(prc_diff$stocks), "\n", dim(prc_diff$stocks_bar))
    cat("\nRownames of stocks_bar and time_all identical?",
        identical(rownames(prc_diff$stocks_bar), prc_diff$time_all), "\n")
  }
  
  prc_diff
}






###########################################################################
# The end 
###########################################################################
