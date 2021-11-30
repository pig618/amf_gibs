
###########################################################################
###########################################################################
###########################################################################
# 
#  Adaptive Multi-Factor (AMF) Analysis (fitting and prediction)
#  and Fitting by other methods (models) for comparison
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
load(file="../03_customized_data/time_all_week.RData")
load(file="../03_customized_data/ex_ret_week.RData")







###########################################################################
# Parallel computing set up
###########################################################################

amf_1secu = function(i, data_1period, vld_len, th_prefer, 
                     method="gibs", alpha=NA, fac_add=NULL,
                     pmax=20, cv_choice="cv1se") {
  # Function to do AMF analysis for 1 security
  
  ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
  stock = data_1period$ret$stocks[, i, drop=F]
  stock_bar = data_1period$ret$stocks_bar[, i, drop=F]
  if (!identical(colnames(stock), colnames(stock_bar))) {
    stop("Error: stock and stock_bar name do not match!")
  }
  factors = data_1period$ret$factors[
    , c(ff5_names, data_1period$etf_reps_info$etf_per_keep), drop=F]
  result = list(permno=colnames(stock))
  
  # AMF fitting -----------------------------------------------------------
  dat = prep_1secu(
    stock=stock, stock_bar=stock_bar, factors=factors, predictive=F,
    time_train=data_1period$time_train,
    time_pred=NULL,
    check_time_stock=T, check_time_factors=F)

  result$fit = gibs(
    dat_select=dat, dat=dat,
    fac_base=NULL, coef_base=NULL, fac_in=NULL, fac_pri=ff5_names,
    vld_len=0, overlap=NULL, th_prefer=NULL, yt_base=F, 
    valid_simplify=NULL, try_intercept=NULL,
    fac_add=fac_add,
    method=method, alpha=alpha, pmax=pmax, cv_choice=cv_choice,
    f_test=T, lr_test=F, verbose=F)
  
  # AMF predicting --------------------------------------------------------
  dat = prep_1secu(
    stock=stock, stock_bar=stock_bar, factors=factors, predictive=T, 
    time_train=data_1period$time_train, 
    time_pred=data_1period$time_pred, 
    check_time_stock=T, check_time_factors=F)

  result$pred = gibs(
    dat_select=dat, dat=dat, 
    fac_base=c("y_bar"), coef_base=c("1"), fac_in=NULL, fac_pri=NULL, 
    vld_len=vld_len, overlap=F, th_prefer=th_prefer, yt_base=F,
    valid_simplify=T, try_intercept=T, 
    fac_add=fac_add, 
    method=method, alpha=alpha, pmax=pmax, cv_choice=cv_choice, 
    f_test=T, lr_test=F, verbose=F)
  
  result
}






amf_1period = function(folder_name, nmach, start_year, end_year, 
                       vld_len, pred_lag, pred_len, th_prefer,
                       method="gibs", alpha=NA, fac_add=NULL, 
                       pmax=20, cv_choice="cv1se",
                       etf_th=1, stocks_th=0.9) {
  # Function to do AMF model analysis for all stocks in 1 period:
  # For time period [start_year, end_year]
  # Save results in folder: folder_name
  
  pt_iter = proc.time()
  cat("\nStart processing ", start_year, " to ", end_year, 
      ": \nWill save results in ", folder_name, ".\n", sep="")
  
  # Preprocessing for all stocks in the time period -----------------------
  start_week = as.integer(start_year) * 100
  end_week = (as.integer(end_year) + 1) * 100 + pred_lag
  
  data_1period = prep_1period(
    start=start_week, end=end_week, pred_len=pred_len, 
    ret=ex_ret_week, prc=NULL, prc_diff=NULL, 
    reps_by="ret", etf_info=etf_info, time_all=time_all_week, 
    etf_th=etf_th, stocks_th=stocks_th)
  
  save(data_1period, file=paste(
    folder_name, "/data_1period.RData", sep=""))
  
  # Try to make n_per_loop around 3000 to avoid too big files.
  n_per_loop = 3000 %/% nmach * nmach
  nloop = ceiling(length(data_1period$stocks_valid) / n_per_loop)
  # n_per_loop = 5  # Debug use
  # nloop = 2       # Debug use
  
  # Export the data and functions.
  sfExport("data_1period")
  
  # AMF analysis for each stock -------------------------------------------
  
  for (i in 1:nloop) {
    results = sfClusterApplyLB(
      x=((i - 1) * n_per_loop + 1) :
        min(i * n_per_loop, length(data_1period$stocks_valid)),
      fun=amf_1secu,
      data_1period=data_1period, vld_len=vld_len, th_prefer=th_prefer,
      method=method, alpha=alpha, fac_add=fac_add,
      pmax=pmax, cv_choice=cv_choice)
    save(results, file=paste(
      folder_name, "/results_", i, ".RData", sep=""))
    rm(results)
    gc()
  }
  
  # # Debug use
  # for (i in 1:nloop) {
  #   js = ((i - 1) * n_per_loop + 1) : min(
  #     i * n_per_loop, length(data_1period$stocks_valid))
  #   for (j in js) {
  #     cat(j, "  ")
  #     amf_1secu(i=j, data_1period=data_1period, 
  #               vld_len=vld_len, th_prefer=th_prefer,
  #               method=method, alpha=alpha, fac_add=fac_add, 
  #               pmax=pmax, cv_choice=cv_choice)
  #   }
  # }
  
  # Save parameters -------------------------------------------------------
  num_etf = length(data_1period$etf_reps_info$etf_per_keep)
  para = list(folder_name=folder_name, 
              start_year=start_year, end_year=end_year, 
              num_etf=num_etf, 
              num_stocks=length(data_1period$stocks_valid), 
              n=nrow(data_1period$ret), p=num_etf + 5, 
              nloop=nloop, n_per_loop=n_per_loop, 
              time_iter=proc.time() - pt_iter)
  save(para, file=paste(folder_name, "/para.RData", sep=""))
  
  cat("Time used for this time period:", para$time_iter[3], "seconds.\n")
  cat(start_year, "to", end_year, "finished!\n\n")
  
  para
}






# Init cluster 
nmach = init_parallel()
sfExport(
  "date_change", "cum_mean", "cum_prod", "get_formula", "pred_gf",  
  "pick_time_frame", "proj_out", "proj_xy", "proj_data", 
  "rm_const_col", "rm_high_corr_col", "prep_1secu", 
  "pre_select", "gibs_select", "valid_select", "lm_report",
  "test_f", "test_lr", "gibs")

cat("Initialization time: ", (proc.time() - pt)[3], "seconds.\n")
# 16 seconds






###########################################################################
# Adaptive Multi-Factor model fit and prediction
###########################################################################

time_periods = list()
i = 1
for (start_year in 2007:2016) {
  for (end_year in (start_year + 2):2018) {
    time_periods[[i]] = c(start_year, end_year)
    i = i + 1
  }
}
time_periods



# AMF on all time periods -------------------------------------------------

parent_folder = "amf_results"
if (!dir.exists(parent_folder)) {
  dir.create(parent_folder)
}

for (i in 1:length(time_periods)) {

  start_year = time_periods[[i]][1]
  end_year = time_periods[[i]][2]
  folder_name = paste(parent_folder, "/results_",
                      start_year, "_", end_year, sep="")

  if (dir.exists(folder_name)) {
    next
  } else {
    dir.create(folder_name)
    ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
    para = amf_1period(
      folder_name=folder_name, nmach=nmach,
      start_year=start_year, end_year=end_year,
      vld_len=52, pred_lag=0, pred_len=4, th_prefer=0.85,
      method="gibs", alpha=NA, fac_add=NULL,
      pmax=20, cv_choice="cv1se",
      etf_th=1, stocks_th=0.8)
  }
}

cat("Total time: ", (proc.time() - pt)[3], "seconds.\n")
# Around 5.7 hours (4.2 hours on red cloud)





 
###########################################################################
# Method Comparison
###########################################################################

parent_folder = "method_comparison"
if (!dir.exists(parent_folder)) {
  dir.create(parent_folder)
}

start_year = 2014 
end_year = 2016 

folder_names = c("gibs_ff5", "enet0", "enet0.25", "enet0.5", 
                 "enet0.75", "enet1")
folder_names = paste(parent_folder, "/", folder_names, sep="")
alphas = c(NA, seq(0, 1, 0.25))
ff5_names = c("mkt", "smb", "hml", "rmw", "cma")

for (i in 1:length(folder_names)) {
  folder_name = folder_names[i]
  alpha = alphas[i]
  dir.create(folder_name)
  
  if (i == 1) {
    para = amf_1period(
      folder_name=folder_name, nmach=nmach, 
      start_year=start_year, end_year=end_year, 
      vld_len=52, pred_lag=0, pred_len=4, th_prefer=0.85, 
      method="gibs", alpha=NA, fac_add=ff5_names, 
      pmax=20, cv_choice="cv1se",
      etf_th=1, stocks_th=0.8)
  } else {
    para = amf_1period(
      folder_name=folder_name, nmach=nmach,
      start_year=start_year, end_year=end_year, 
      vld_len=52, pred_lag=0, pred_len=4, th_prefer=0.85, 
      method="enet", alpha=alpha, fac_add=NULL, 
      pmax=20, cv_choice="cv1se",
      etf_th=1, stocks_th=0.8) 
  }
  
}

cat("Total time: ", (proc.time() - pt)[3], "seconds.\n")
# Around 1 hour(s)



sfStop()






###########################################################################
# The end 
###########################################################################
