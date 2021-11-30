
###########################################################################
###########################################################################
###########################################################################
# 
#  Example Study for the AMF model
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()
pt = proc.time()

source("../00_initial_code/01_func_help.R")
libs_local()

source("../04_gibs_algo/01_func_gibs_help.R")
source("../04_gibs_algo/02_func_gibs_cluster.R")
source("../04_gibs_algo/03_func_gibs_prep.R")
source("../04_gibs_algo/04_func_gibs.R")

load(file="../02_id_class/etf_info.RData")
load(file="../02_id_class/stocks_info.RData")
load(file="../03_customized_data/time_all_week.RData")
load(file="../03_customized_data/ex_ret_week.RData")






###########################################################################
# Example tickers (search in WRDS-CRSP)
###########################################################################

# GOOG (GOOGL) FB MSFT AMZN ADBE AAPL HPQ 
# INTC IBM NVDA ORCL BAC JPM MS C AXP

eg_tic = c("GOOG", "GOOGL", "FB", "MSFT", "AMZN", "ADBE", "AAPL",
           "HPQ", "INTC", "IBM", "NVDA",
           "ORCL", "BAC", "JPM", "MS", "C", "AXP")
subset(stocks_info$stocks_class, ticker %in% eg_tic, 
       select=c("permno", "ticker", "comnam"))
rm(eg_tic)

eg_per = c(90319, 10107, 84788, 75510, 10104, 12490, 13407, 
           14593, 27828, 59328, 86580, 
           59408, 47896, 59176, 69032)
eg_info = subset(stocks_info$stocks_class, 
                 permno %in% eg_per)
eg_info = eg_info[match(eg_per, eg_info$permno), ]
rownames(eg_info) = NULL
eg_info[, c("permno", "ticker", "comnam", "category_no", "hsiccd", "naics")]


# Now google is "GOOGL"
# 
#    permno ticker                           comnam category_no hsiccd  naics
# 1   90319  GOOGL                     ALPHABET INC          73   7375 334419
# 2   10107   MSFT                   MICROSOFT CORP          73   7370 511210
# 3   84788   AMZN                   AMAZON COM INC          73   7370 454113
# 4   75510   ADBE                        ADOBE INC          73   7372 511210
# 5   10104   ORCL                      ORACLE CORP          73   7372 511210
# 6   12490    IBM INTERNATIONAL BUSINESS MACHS COR          73   7379 541512
# 7   13407     FB                     FACEBOOK INC          99   9999 519130
# 8   14593   AAPL                        APPLE INC          35   3571 334220
# 9   27828    HPQ                          H P INC          35   3571 334111
# 10  59328   INTC                       INTEL CORP          36   3679 334413
# 11  86580   NVDA                      NVIDIA CORP          36   3670 334413
# 12  59408    BAC             BANK OF AMERICA CORP          60   6021 522110
# 13  47896    JPM              JPMORGAN CHASE & CO          60   6021 522110
# 14  59176    AXP              AMERICAN EXPRESS CO          60   6029 522291
# 15  69032     MS  MORGAN STANLEY DEAN WITTER & CO          62   6211 523120






###########################################################################
# Example study parameters and data prepare
###########################################################################

library(xtable)

ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
start_year = 2014
end_year = 2016
vld_len=52
pred_lag=0
pred_len=4
th_prefer=0.85
method="gibs"
alpha=NA
fac_add=NULL
pmax=20
cv_choice="cv1se"
etf_th=1
stocks_th=0.8

start_week = as.integer(start_year) * 100
end_week = (as.integer(end_year) + 1) * 100 + pred_lag

data_1period = prep_1period(
  start=start_week, end=end_week, pred_len=pred_len, 
  ret=ex_ret_week, prc=NULL, prc_diff=NULL, 
  reps_by="ret", etf_info=etf_info, time_all=time_all_week, 
  etf_th=etf_th, stocks_th=stocks_th)






###########################################################################
# Example study 
###########################################################################

eg_study = function(i){
  # AMF analysis for example study. 
  # Report summaries here.
  stock = data_1period$ret$stocks[, i, drop=F]
  stock_bar = data_1period$ret$stocks_bar[, i, drop=F]
  if (!identical(colnames(stock), colnames(stock_bar))) {
    stop("Error: stock and stock_bar name do not match!")
  }
  factors = data_1period$ret$factors[
    , c(ff5_names, data_1period$etf_reps_info$etf_per_keep), drop=F]
  # result = list(permno=colnames(stock))
  
  # AMF fitting -----------------------------------------------------------
  dat = prep_1secu(
    stock=stock, stock_bar=stock_bar, factors=factors, predictive=F,
    time_train=data_1period$time_train,
    time_pred=NULL,
    check_time_stock=T, check_time_factors=F)
  
  result = gibs(
    dat_select=dat, dat=dat,
    fac_base=NULL, coef_base=NULL, fac_in=NULL, fac_pri=ff5_names,
    vld_len=0, overlap=NULL, th_prefer=NULL, yt_base=F, 
    valid_simplify=NULL, try_intercept=NULL,
    fac_add=fac_add,
    method=method, alpha=alpha, pmax=pmax, cv_choice=cv_choice,
    f_test=T, lr_test=F, verbose=T)
  result
}



summary_to_table = function(summ, extra_int=NULL) {
  # Change summaries to tables
  tab = as.data.frame(summ$coefficients)
  if (!is.null(extra_int)) {
    extra_int = as.data.frame(
      t(extra_int), row.names="(Intercept)")
    tab = rbind(extra_int, tab)
  }
  tab = round(tab, 3)
  tab = subset(tab, select=c("Estimate", "Std. Error", 
                             "t value", "Pr(>|t|)"))
  colnames(tab) = c("beta", "SE", "t value", "p value")
  rownames(tab) = name_match[rownames(tab), ]
  latex = xtable(tab, digits=3)
  
  list(tab=tab, latex=latex)
}



eg_table = function(per) { 
  # Form tables for examples
  if (substr(per, start=1, stop=3) != "per") {
    per = paste("per", per, sep="")
  }
  
  i = which(colnames(data_1period$ret$stocks) == per)
  res = eg_study(i)
  
  s_ff5 = summary(res$fit_ff5)
  s_gibs = summary(res$fit_gibs)
  tab1 = summary_to_table(s_ff5, extra_int=res$int_ff5)
  tab2 = summary_to_table(s_gibs, extra_int=res$int_gibs)
  
  sgn_etf = setdiff(rownames(coef(s_gibs))[coef(s_gibs)[, 4] < 0.05], 
                    c("(Intercept)", "mkt", "smb", "hml", "rmw", "cma"))
  tab3 = info_match[sgn_etf, ]
  colnames(tab3) = c("ETF Name", "Category", "Big Class")
  
  list(others=res,
       tab_ff5=tab1$tab, lat_ff5=tab1$latex, 
       tab_amf=tab2$tab, lat_amf=tab2$latex, 
       tab_sgn_amf=tab3, lat_sgn=xtable(tab3))
}



# Give info of each ETF and FF5
info_match = subset(etf_info$etf_class, 
                    select=c("permno", "etf_name", 
                               "category", "big_class"))
rownames(info_match)=paste("per", info_match$permno, sep="")
info_match$permno = NULL
ff5_match = data.frame(etf_name=c("(Intercept)", "Market Return", 
                                    "SMB", "HML", "RMW", "CMA"), 
                       category=c("(Intercept)", rep("FF5 factors", 5)), 
                       big_class=c("(Intercept)", rep("FF5 factors", 5)))
rownames(ff5_match) = c("(Intercept)", "mkt", "smb", "hml", "rmw", "cma")
info_match = rbind(ff5_match, info_match)
name_match = subset(info_match, select=c("etf_name"))



# Adobe
result = eg_table("per75510")
print(result$lat_ff5, include.rownames=T, 
      hline.after=0:nrow(result$lat_ff5))
print(result$lat_amf, include.rownames=T, 
      hline.after=0:nrow(result$lat_amf))
print(result$lat_sgn, include.rownames=F, 
      hline.after=0:nrow(result$lat_sgn))
result$others$ars_ff5
result$others$ars_gibs



# Bank of America
result = eg_table("per59408")
print(result$lat_ff5, include.rownames=T, 
      hline.after=0:nrow(result$lat_ff5))
print(result$lat_amf, include.rownames=T, 
      hline.after=0:nrow(result$lat_amf))
print(result$lat_sgn, include.rownames=F, 
      hline.after=0:nrow(result$lat_sgn))
result$others$ars_ff5
result$others$ars_gibs



# INTEL
result = eg_table("per59328")
print(result$lat_ff5, include.rownames=T, 
      hline.after=0:nrow(result$lat_ff5))
print(result$lat_amf, include.rownames=T, 
      hline.after=0:nrow(result$lat_amf))
print(result$lat_sgn, include.rownames=F, 
      hline.after=0:nrow(result$lat_sgn))
result$others$ars_ff5
result$others$ars_gibs



# per = "per86580"    # NVIDIA
# i = 3406 
# eg_table("per86580")


# Now google is "GOOGL"
# 
#    permno ticker                           comnam category_no hsiccd  naics
# 1   90319  GOOGL                     ALPHABET INC          73   7375 334419
# 2   10107   MSFT                   MICROSOFT CORP          73   7370 511210
# 3   84788   AMZN                   AMAZON COM INC          73   7370 454113
# 4   75510   ADBE                        ADOBE INC          73   7372 511210
# 5   10104   ORCL                      ORACLE CORP          73   7372 511210
# 6   12490    IBM INTERNATIONAL BUSINESS MACHS COR          73   7379 541512
# 7   13407     FB                     FACEBOOK INC          99   9999 519130
# 8   14593   AAPL                        APPLE INC          35   3571 334220
# 9   27828    HPQ                          H P INC          35   3571 334111
# 10  59328   INTC                       INTEL CORP          36   3679 334413
# 11  86580   NVDA                      NVIDIA CORP          36   3670 334413
# 12  59408    BAC             BANK OF AMERICA CORP          60   6021 522110
# 13  47896    JPM              JPMORGAN CHASE & CO          60   6021 522110
# 14  59176    AXP              AMERICAN EXPRESS CO          60   6029 522291
# 15  69032     MS  MORGAN STANLEY DEAN WITTER & CO          62   6211 523120



###########################################################################
# The end 
###########################################################################
