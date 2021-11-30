
###########################################################################
###########################################################################
###########################################################################
# 
#  Pre-processing functions for the Groupwise Interpretable 
#  Basis Selection (GIBS) algorithm        
# 
###########################################################################
###########################################################################
###########################################################################

prep_1period = function(start, end, pred_len, ret, prc, prc_diff, 
                        reps_by, etf_info, time_all, etf_th, stocks_th) {
  #########################################################################
  # Pre-process to form the data for the GIBS algorithm
  # This is done for 1 time period, for all stocks.
  # 
  # Arguments:
  # Training period is [start, end], 
  # Prediction period is (end, end + pred_len]
  # ret: Return (normally use excess return).
  # prc: Price (normally use adjusted price).
  # prc_diff: Price difference (normally use adjusted price difference).
  # reps_by: Data used to get basis assets representatives.
  #   can be "ret" or "prc_diff". 
  #   Don't get reps on prc because "Rf" is close to all 1. (rm_const_col).
  # etf_info: ETF information. For clustering.
  # time_all: A vector containing all weeks wider than the period.
  # etf_th: Threshold for ETF. Normally set to 1.
  # stocks_th: Threshold for stocks. Normally set to 0.9.
  # 
  # If raw_ret or raw_prc is NULL, only provide excess returns, 
  # otherwise also provide prices and price differences.
  # 
  # Return Values:
  # start, end
  # time_train, time_pred 
  # time_train1, time_train2 (1st and 2nd half of the time_train)
  # stocks_valid, etfs_valid: securities available in 90% of weeks
  # etf_reps_info: ETF representatives info list for the whole period. 
  # etf_reps_info2: ETF representatives info list for the 2nd half period.
  # etf_reps_union: ETF representatives name vector, union of the 2 above.
  # ret: Return data. 3 elements: stocks, stocks_bar, factors.
  # prc: Price data. 3 elements: stocks, stocks_bar, factors.
  # prc_diff: Price difference. 3 elements: stocks, stocks_bar, factors.
  #########################################################################
  
  # Form time vectors (must be characters !!!) ----------------------------
  if (!all(time_all == sort(time_all))) {
    stop("Error: time_all is not sorted!")
  }
  time_all = as.character(time_all)
  start = as.character(start)
  end = as.character(end)
  
  time_train = time_all[(time_all >= start) & (time_all <= end)]
  time_pred = date_change(tail(time_train, 1), time_all, 1, pred_len)
  
  n = length(time_train)
  time_train1 = time_train[1 : (n %/% 2)]
  time_train2 = time_train[(n %/% 2 + 1) : n]
  
  result = list(start=start, end=end, 
                time_train=time_train, time_pred=time_pred, 
                time_train1=time_train1, time_train2=time_train2)
  
  # Pick time frame on ret, prc, prc_diff ---------------------------------
  # All 3 possible datasets have time: time_train + time_pred.
  
  get_ret = length(ret) > 0
  get_prc = length(prc) > 0 | length(prc_diff) > 0
  if (!get_ret & !get_prc) {
    stop("Error: Nothing to preprocess in the time period!")
  }
  
  if (get_ret) {
    # Excess return
    ret = pick_time_frame(
      dat=ret, start=time_train[1], end=tail(time_pred, 1), 
      etf_th=0, stocks_th=0)
    
    # Stocks and ETFs valid
    ret_train = pick_time_frame(
      dat=ret, start=time_train[1], end=tail(time_train, 1), 
      etf_th=etf_th, stocks_th=stocks_th)
    stocks_valid = colnames(ret_train$stocks)
    etfs_valid = colnames(ret_train$etf)
    
    # No NA in predicting period
    ret_pred = pick_time_frame(
      dat=ret, start=time_pred[1], end=tail(time_pred, 1),
      etf_th=1, stocks_th=1)
    stocks_valid = intersect(
      stocks_valid, colnames(ret_pred$stocks))
    etfs_valid = intersect(
      etfs_valid, colnames(ret_pred$etf))
  }

  if (get_prc) {
    # Adjusted price
    prc = pick_time_frame(
      dat=prc, start=time_train[1], end=tail(time_pred, 1), 
      etf_th=0, stocks_th=0)
    
    # Adjusted price difference
    prc_diff = pick_time_frame(
      dat=prc_diff, start=time_train[1], end=tail(time_pred, 1), 
      etf_th=0, stocks_th=0)
    
    # Remove rf (risk free rate) in prc_diff since it is almost always 0
    # range of price difference of the rf in 2011 - 2018 is 0 - 0.000514
    # and it's always exactly 0 in 2011 - 2015, can't put it in regression.
    # Still keep the rf in prc for intercept test.
    prc_diff$ff5$rf = NULL
    
    # Stocks and ETFs valid
    prc_diff_train = pick_time_frame(
      prc_diff, time_train[1], tail(time_train, 1), 
      etf_th=etf_th, stocks_th=stocks_th)
    if (!get_ret) {
      stocks_valid = colnames(prc_diff_train$stocks)
      etfs_valid = colnames(prc_diff_train$etf)
    } else {
      stocks_valid = intersect(
        stocks_valid, colnames(prc_diff_train$stocks))
      etfs_valid = intersect(
        etfs_valid, colnames(prc_diff_train$etf))
    }
    
    # No NA in predicting period
    prc_diff_pred = pick_time_frame(
      dat=prc_diff, start=time_pred[1], end=tail(time_pred, 1),
      etf_th=1, stocks_th=1)
    stocks_valid = intersect(
      stocks_valid, colnames(prc_diff_pred$stocks))
    etfs_valid = intersect(
      etfs_valid, colnames(prc_diff_pred$etf))
  }
  
  result = c(result, list(
    stocks_valid=stocks_valid, etfs_valid=etfs_valid))
  
  # Form the ETF representatives union ------------------------------------
  if (!(reps_by %in% c("ret", "prc_diff"))) {
    stop("Error: reps_by has to be 'ret' or 'prc_diff'!")
  }
  
  if (reps_by == "ret") {
    fac = cbind(ret_train$ff5, ret_train$etf[, etfs_valid, drop=F])
  } else {
    fac = cbind(
      prc_diff_train$ff5, prc_diff_train$etf[, etfs_valid, drop=F])
  }
  
  # GIBS cluster on training period to find ETF representatives 
  etf_reps_info = gibs_cluster(
    fac, etf_info, pca_th=0.9, h_th=0.3, mkt_th=0.9)
  result$etf_reps_info = etf_reps_info
  
  # GIBS cluster on second-half training period 
  fac2 = fac[time_train2, ]
  etf_reps_info2 = gibs_cluster(
    fac2, etf_info, pca_th=0.9, h_th=0.3, mkt_th=0.9)
  result$etf_reps_info2 = etf_reps_info2
  
  etf_reps_union = union(
    etf_reps_info$etf_per_keep, etf_reps_info2$etf_per_keep)
  result$etf_reps_union = etf_reps_union
  
  # Form data for the coming analysis -------------------------------------
  
  # The time match of ret, prc, prc_diff are:
  # prc_diff[t] = prc[t] - prc[t - 1]
  # ret[t] = (prc[t] - prc[t - 1]) / prc[t - 1]
  
  if (get_ret) {
    result$ret = list(
      stocks=ret$stocks[, stocks_valid, drop=F], 
      stocks_bar=ret$stocks_bar[, stocks_valid, drop=F],
      factors=cbind(ret$ff5, ret$etf[, etf_reps_union, drop=F]))
  }
  
  if (get_prc) {
    result$prc = list(
      stocks=prc$stocks[, stocks_valid, drop=F],
      stocks_bar=prc$stocks_bar[, stocks_valid, drop=F],
      factors=cbind(prc$ff5, prc$etf[, etf_reps_union, drop=F]))
    result$prc_diff = list(
      stocks=prc_diff$stocks[, stocks_valid, drop=F], 
      stocks_bar=prc_diff$stocks_bar[, stocks_valid, drop=F],
      factors=cbind(prc_diff$ff5, prc_diff$etf[, etf_reps_union, drop=F]))
  }
  
  result
}






prep_1secu = function(stock, stock_bar, factors, predictive, 
                      time_train, time_pred,
                      check_time_stock, check_time_factors) {

  #########################################################################
  # Function to preprocess the data for the GIBS algorithm
  #   for one security.
  # 
  # Arguments:
  # stock: An n * 1 data frame with the stock data.
  #   The rownames should be the date.
  # stock_bar: An n * 1 data frame with stock expand window mean.
  #   The rownames should be the date.
  # factors: An n' * p data frame with factor data as the
  #   independent variables. The rownames should be the date. 
  #   Note that n' >= n, and rownames(stock) %in% rownames(factors).  
  # predictive: Use predictive model if TRUE, simultaneous model if FALSE.
  # time_train: The possible dates for the training time period. 
  #   Set time_train to all possible dates if it is NULL.
  # time_pred: The dates for predicting time period. 
  #   If time_predict == NULL, do not use the model to predict.
  #   Note that time_predict can be any time periods, but using the time
  #   before the training period may result in look-ahead bias.
  # check_time_stock: Remove rows containing NA from stock if TRUE.
  # check_time_factors: Remove rows containing NA from factors if TRUE.
  # 
  # Return:
  # data_train: Data for training, with column "y" being the response.
  # data_pred: Data for predicting, with column "y" being the response.
  # y_name: The column name of the response.
  #########################################################################
  
  # Check the data format and save the response name ----------------------
  if (!is.data.frame(stock) | !is.data.frame(stock_bar) 
      | !is.data.frame(factors)) {
    stop("Error: The stock, stock_bar, or factors is not a data frame!")
  }
  if ((dim(stock)[2] != 1) | (dim(stock_bar)[2] != 1)) {
    stop("Error: There should be only one stock!")
  }
  if (colnames(stock) %in% colnames(factors)) {
    stop("Error: The stock is an ETF!")
  }
  dat = list(y_name=colnames(stock))
  
  # Clean time ------------------------------------------------------------
  time_stock = rownames(stock)
  time_factors = rownames(factors)
  if (any(duplicated(time_stock)) 
      | any(duplicated(time_factors))) {
    stop("Error: Duplicated time stamp!")
  }
  all_time = sort(union(time_stock, time_factors))
  
  # Remove time with NA values
  if (check_time_factors) {
    time_factors = time_factors[apply(factors, 1, function(z) { 
      all(is.finite(z)) })]
  }
  if (check_time_stock) {
    time_stock = time_stock[is.finite(stock[, 1, drop=T])]
    time_stock_bar = rownames(stock_bar)
    time_stock_bar = time_stock_bar[is.finite(stock_bar[, 1, drop=T])]
    if (!identical(time_stock, time_stock_bar)) {
      stop("Error: stock and stock_bar do not have same valid time!")
    }
  }
  
  good_time = intersect(time_stock, time_factors)
  if (predictive) {
    good_id = which(all_time %in% good_time)
    good_id = intersect(good_id, good_id - 1)
    good_time = all_time[good_id]
    names(good_time) = all_time[good_id + 1] 
    # Name of the good_time is the good_time_next (1 day after goog_time)
    # Use names to switch between get good_time_next and good_time
    rm(good_id)
  }
  
  # Split the train and predict data --------------------------------------
  # The rownames are always same to the response since
  #   when you say predict 2018-01-02, you predict the y of 2018-01-02
  #   using all the data accessible in 2018-01-01
  
  if (length(time_train) == 1) {
    stop("Error: Can't use train length 1!")
  }
  if (length(time_train) == 0) {
    time_train = good_time
  } 
  
  if (!predictive) {
    
    # Simultaneous model
    time_train = intersect(time_train, good_time)
    time_pred = intersect(time_pred, good_time)
    
    fac = as.data.frame(factors[time_train, , drop=F])
    pred_fac = as.data.frame(factors[time_pred, , drop=F])
    
    sto = stock[time_train, 1, drop=T]
    pred_sto = stock[time_pred, 1, drop=T]
    
  } else {
    
    # Predictive model
    time_train = time_train[1 : (length(time_train) - 1)]
    time_train = intersect(time_train, good_time)
    time_train_next = names(good_time)[good_time %in% time_train]
    time_pred_next = intersect(time_pred, names(good_time))
    time_pred = good_time[names(good_time) %in% time_pred_next]
    
    # Now the row names are in t, not t + 1
    factors = cbind(y_t=stock[, 1, drop=T], 
                    y_bar=stock_bar[, 1, drop=T], factors)
    factors = factors[c(time_train, time_pred), , drop=F]
    stock = stock[c(
      time_train[1], time_train_next, time_pred_next), , drop=F]
    
    # Change row names to t + 1, not t
    fac = as.data.frame(factors[time_train, , drop=F])
    rownames(fac) = time_train_next
    pred_fac = as.data.frame(factors[time_pred, , drop=F])
    rownames(pred_fac) = time_pred_next
    
    sto = stock[time_train_next, 1, drop=T]
    pred_sto = stock[time_pred_next, 1, drop=T]
    
  }
  
  dat$data_train = cbind(y=sto, fac)
  dat$data_pred = cbind(y=pred_sto, pred_fac)
  
  dat
}



  


###########################################################################
# The end 
###########################################################################
