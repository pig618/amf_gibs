
###########################################################################
###########################################################################
###########################################################################
# 
#  Dynamic portfolio arbitrage test
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
source("../04_gibs_algo/05_func_report.R")

load(file="../02_id_class/etf_info.RData")
load(file="../03_customized_data/time_all_week.RData")
load(file="../03_customized_data/ex_ret_week.RData")






###########################################################################
# Parallel computing set up
###########################################################################

amf_intercept = function(i, data_1period, 
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
  
  result = gibs(
    dat_select=dat, dat=dat,
    fac_base=NULL, coef_base=NULL, fac_in=NULL, fac_pri=ff5_names,
    vld_len=0, overlap=NULL, th_prefer=NULL, yt_base=F, 
    valid_simplify=NULL, try_intercept=NULL,
    fac_add=fac_add,
    method=method, alpha=alpha, pmax=pmax, cv_choice=cv_choice,
    f_test=T, lr_test=F, verbose=F)
  result[c("permno", "int_ff5", "int_gibs")]
}



arbitrage_1period = function(folder_name, nmach, start_week, end_week, 
                             method="gibs", alpha=NA, fac_add=NULL, 
                             pmax=20, cv_choice="cv1se",
                             etf_th=1, stocks_th=0.9) {
  # Function to do AMF model analysis for all stocks in 1 period:
  # For time period [start_week, end_week]
  # Save results in folder: folder_name
  
  pt_iter = proc.time()
  cat("\nStart processing ", start_week, " to ", end_week, 
      ": \nWill save results in ", folder_name, ".\n", sep="")
  
  # Preprocessing for all stocks in the time period -----------------------
  start_week = as.integer(start_week) 
  end_week = as.integer(end_week)
  
  data_1period = prep_1period(
    start=start_week, end=end_week, pred_len=0, 
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
  
  # AMF intercept for each stock ------------------------------------------
  for (i in 1:nloop) {
    results = sfClusterApplyLB(
      x=((i - 1) * n_per_loop + 1) :
        min(i * n_per_loop, length(data_1period$stocks_valid)),
      fun=amf_intercept,
      data_1period=data_1period,
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
  #     amf_intercept(i=j, data_1period=data_1period,
  #                   method=method, alpha=alpha, fac_add=fac_add,
  #                   pmax=pmax, cv_choice=cv_choice)
  #   }
  # }
  
  # Save parameters -------------------------------------------------------
  num_etf = length(data_1period$etf_reps_info$etf_per_keep)
  para = list(folder_name=folder_name, 
              start_week=start_week, end_week=end_week, 
              num_etf=num_etf, 
              num_stocks=length(data_1period$stocks_valid), 
              n=nrow(data_1period$ret), p=num_etf + 5, 
              nloop=nloop, n_per_loop=n_per_loop, 
              time_iter=proc.time() - pt_iter)
  save(para, file=paste(folder_name, "/para.RData", sep=""))
  
  cat("Time used for this time period:", para$time_iter[3], "seconds.\n")
  cat(start_week, "to", end_week, "finished!\n\n")
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
# Derive dynamic intercept p value results
###########################################################################

parent_folder = "arbitrage_results"
if (!dir.exists(parent_folder)) {
  dir.create(parent_folder)
}

start = 2014
end = 2017
start_date = start * 100
end_date = (end + 1) * 100
all_dates = time_all_week[time_all_week >= start_date]
all_dates = all_dates[all_dates <= end_date]
train_len = sum(all_dates < end * 100)

pt = proc.time()
pos = train_len
while (pos < length(all_dates)) {
  start_week = all_dates[pos - train_len + 1]
  end_week = all_dates[pos]
  folder_name = paste(parent_folder, "/results_",
                      start_week, "_", end_week, sep="")
  pos = pos + 1
  
  if (dir.exists(folder_name)) {
    next
  } else {
    dir.create(folder_name)
    ff5_names = c("mkt", "smb", "hml", "rmw", "cma")
    para = arbitrage_1period(
      folder_name=folder_name, nmach=nmach,
      start_week=start_week, end_week=end_week,
      method="gibs", alpha=NA, fac_add=NULL,
      pmax=20, cv_choice="cv1se",
      etf_th=1, stocks_th=0.8)
  }

}

cat("Total time: ", (proc.time() - pt)[3], "seconds.\n")
# Around 5.7 hours (4.2 hours on red cloud)

sfStop()     # Stop the cluster






###########################################################################
# The end 
###########################################################################