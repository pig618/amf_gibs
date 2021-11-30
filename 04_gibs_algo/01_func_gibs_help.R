
###########################################################################
###########################################################################
###########################################################################
# 
#  Help functions to for the GIBS algorithm:
#    pick time frame, project out market return, remove constant columns,
#    remove highly correlated columns.
# 
###########################################################################
###########################################################################
###########################################################################

pick_time_frame = function(dat, start, end, 
                           etf_th=1, stocks_th=0.8) {
  # Pick the time frame and pick valid ETF and stocks by the thresholds
  # The threshold is the ratio. stocks_th = 0.8 means that
  #   only stocks with more than 0.8 * current_full_length valid 
  #   records will be selected.
  # 
  # Normally set etf_th = 1. 
  # 
  # The first 5 elements of the data "dat" should be: 
  # ff5, etf, stocks, stocks_bar, time_all
  
  if (!identical(colnames(dat$stocks), colnames(dat$stocks_bar))) {
    stop("Error: stocks and stocks_bar colnames do not match!")  
  }
  
  # dat$time_all = as.character(dat$time_all)
  if (!is.character(dat$time_all)) {
    stop("Error: time_all should be characters!")
  }
  time_all = dat$time_all
  
  start = as.character(start)
  end = as.character(end)
  time_all = time_all[(time_all >= start) & (time_all <= end)]
  time_len = length(time_all)
  
  ff5 = dat$ff5[time_all, , drop=F]
  if (any(is.na(ff5))) {
    stop("NA appears in FF5!")
  }
  
  etf = dat$etf[time_all, ]
  good_cols = apply(etf, 2, function(x) { 
    sum(!is.na(x)) >= etf_th * time_len 
  })
  etf = etf[, good_cols, drop=F]
  
  stocks = dat$stocks[time_all, ]
  good_cols = apply(stocks, 2, function(x) { 
    sum(!is.na(x)) >= stocks_th * time_len
  })
  stocks = stocks[, good_cols, drop=F]
  
  stocks_bar = dat$stocks_bar[time_all, good_cols, drop=F]
  
  list(ff5=ff5, etf=etf, stocks=stocks, stocks_bar=stocks_bar,
       time_all=time_all)
}




proj_out = function(y, x) { 
  # Project out x from y. (equivalent to get residual of y ~ x)
  # y, x have to be data frames!
  # y_new = (I - x (x' x) ^ (-1) x') y
  # Note that using solve() is slower than lm()
  
  if (length(x) == 0) {
    return(y)
  }
  
  if (!is.data.frame(y) | !is.data.frame(x)) {
    stop("Error: y or x is not a data frame!")
  }
  
  formula_str = get_formula(
    y=colnames(y), x=colnames(x), intercept=F, x_coef=NULL)
  resid = data.frame(lm(formula_str, data=cbind(y, x))$residuals)
  names(resid) = colnames(y)
  resid
}






proj_xy = function(y, x, cols) {
  # Project out x[, cols, drop=F] from x and y
  y = proj_out(y=y, x=x[, cols, drop=F])
  x = proj_out(y=x[, setdiff(colnames(x), cols), drop=F], 
               x=x[, cols, drop=F])
  list(y=y, x=x)
}






proj_data = function(dat, core, still=NULL, change=NULL){
  #########################################################################
  # Function to project out the "core" columns from all "change" columns
  #   while keeping the "still" columns unchanged (if "still" is in dat). 
  # The column "core" should not be changed, so add it to "still".
  # If change is NULL, set it to all columns except "still" & "core".
  # If core is not in colnames(dat), then return the same data
  #########################################################################
  
  core = intersect(core, colnames(dat))
  still = intersect(union(still, core), colnames(dat))
  if (length(change) == 0) {
    change = setdiff(colnames(dat), still)
  }
  if (length(change) == 0 | length(core) == 0) {
    return(dat)
  }
  
  fac_proj = dat[, change, drop=F]
  fac_proj = proj_out(y=fac_proj, x=dat[, core, drop=F])
  
  as.data.frame(cbind(dat[, still, drop=F], fac_proj))
}






rm_const_col = function(x) {
  # Remove constant columns of x
  
  if (nrow(x) == 1) {
    stop("Error: Only 1 row, of course constant!")
  }
  
  suspects = which(x[1, ] == x[nrow(x), ])
  # Suspect constant columns. Do this to speed up the procedure.
  const_cols = NULL
  
  for (col in suspects) { 
    if (all(x[, col, drop=T] == x[1, col, drop=T])) { 
      const_cols = c(const_cols, col)
    }
  }
  
  if (!is.null(const_cols)){
    x[, const_cols] = NULL
  }
  x
}






rm_high_corr_col = function(x, y, threshold) {
  # Remove columns in data frame x that with absolute
  # correlation higher than threshold to any columns 
  # in the data frame y.
  
  indicator_matrix = (abs(cor(x, y)) >= threshold)
  good_cols = apply(indicator_matrix, 1, function(x) { sum(x) == 0 })
  good_cols = names(which(good_cols))
  
  x = x[good_cols]
  x
}






###########################################################################
# The end 
###########################################################################
