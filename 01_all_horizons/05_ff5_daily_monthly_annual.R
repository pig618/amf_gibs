
###########################################################################
# Clean daily, monthly and annual Fama-French 5-factor data.   
# 
# 
# 
# Clean the daily, monthly and annual ff5 data. Change percentage 
# to real values.  Save to "ff5_daily_clean.RData", 
# "ff5_monthly_clean.RData." and "ff5_annual_clean.RData".
# 
# 
# 
# Input files:
# "../00_data/02_ff5_data/ff5_daily.txt"
# "../00_data/02_ff5_data/ff5_monthly.txt"
# "../00_data/02_ff5_data/ff5_annual.txt"
# 
# 
# 
# Output files:
# "ff5_daily_clean.RData"
# "ff5_monthly_clean.RData."
# "ff5_annual_clean.RData."
# 
# 
# 
# Notes: 
# The ff5 returns are in percentage in the original data file downloaded.
#   I changed them into real values (not percentage) for further use.
###########################################################################

rm(list=ls()); gc()
source("../00_initial_code/01_func_help.R")


if (memory.limit() < 64 * 1024) {
  memory.limit(size = 64 * 1024)
}    # Expand memory limit to be 64 GB.

if (!require(data.table)) {
  install.packages('data.table')
}
library(data.table)



# Read the daily, monthly and annual ff5 data -----------------------------
ff5 = fread("../00_data/02_ff5_data/ff5_daily.CSV", 
            data.table=F)
ff5_mon = fread("../00_data/02_ff5_data/ff5_monthly.CSV", 
                data.table=F)
ff5_ann = fread("../00_data/02_ff5_data/ff5_annual.CSV", 
                data.table=F)
dim(ff5); head(ff5); check_bad_col(ff5)
dim(ff5_mon); head(ff5_mon); check_bad_col(ff5_mon)
dim(ff5_ann); head(ff5_ann); check_bad_col(ff5_ann)



# Change all columns names into lowercase ---------------------------------
# This is for coding style correctness.
# Reorder the columns and change name MktRf to mkt_rf.
old_names_reorder = c("date", "RF", "MktRf", "SMB", "HML", "RMW", "CMA")
col_names = c("date", "rf", "mkt_rf", "smb", "hml", "rmw", "cma")
ret_cols = setdiff(col_names, "date")

ff5 = subset(ff5, select=old_names_reorder)
names(ff5) = col_names

ff5_mon = subset(ff5_mon, select=old_names_reorder)
names(ff5_mon) = col_names

ff5_ann = subset(ff5_ann, select=old_names_reorder)
names(ff5_ann) = col_names

dim(ff5); head(ff5); check_bad_col(ff5)
dim(ff5_mon); head(ff5_mon); check_bad_col(ff5_mon)
dim(ff5_ann); head(ff5_ann); check_bad_col(ff5_ann)
# it is clear that annual returns are in percentage.



# # Check if the values are percentage ------------------------------------
# # Check this by comparing the actual monthly return and the monthly 
# #   return constructed asssuming real value or percentage respectively.
# month_index = as.integer(substr(ff5$date, 1, 6))
# ff5_list = split(ff5[ret_cols], month_index)
# 
# combine_real = function(x) {
#   # combine returns if returns are real values not percentage
#   # real to real
#   x = x + 1
#   y = apply(x, 2, prod) - 1
#   y
# }
# 
# combine_perc = function(x) {
#   # combine returns if returns are in percentage
#   # percentage to percentage
#   x = x / 100 + 1 
#   y = (apply(x, 2, prod) - 1) * 100
#   y
# }
# 
# pt = proc.time()
# ncores = detectCores() - 1 
# sfInit(parallel=T, cpus=ncores)
# ff5_mon_if_real = sfClusterApplyLB(ff5_list, combine_real)
# ff5_mon_if_perc = sfClusterApplyLB(ff5_list, combine_perc)
# sfStop()
# ff5_mon_if_real = as.data.frame(do.call(rbind, ff5_mon_if_real))
# ff5_mon_if_perc = as.data.frame(do.call(rbind, ff5_mon_if_perc))
# ff5_mon_if_real$date = sort(unique(month_index))
# ff5_mon_if_perc$date = sort(unique(month_index))
# proc.time() - pt      # 2 seconds.
# 
# head(ff5_mon)
# head(ff5_mon_if_perc)
# head(ff5_mon_if_real)
# # So every value in raw data is in PERCENTAGE !!!



# Change to real value ----------------------------------------------------
ff5[ret_cols] = ff5[ret_cols] / 100
ff5_mon[ret_cols] = ff5_mon[ret_cols] / 100
ff5_ann[ret_cols] = ff5_ann[ret_cols] / 100



# Add rf to mkt_rf to form mkt and reorder the columns --------------------
# Add risk-free rate to the mkt_rf to form market return (mkt) 
ff5$mkt = ff5$mkt_rf + ff5$rf
ff5_mon$mkt = ff5_mon$mkt_rf + ff5_mon$rf
ff5_ann$mkt = ff5_ann$mkt_rf + ff5_ann$rf

# Reorder the columns
col_names = c("date", "rf", "mkt", "smb", "hml", "rmw", "cma")
ff5 = ff5[col_names]
ff5_mon = ff5_mon[col_names]
ff5_ann = ff5_ann[col_names]



# Save the results --------------------------------------------------------
head(ff5)
head(ff5_mon)
head(ff5_ann)

pt = proc.time()
ff5_daily_clean = ff5
ff5_monthly_clean = ff5_mon
ff5_annual_clean = ff5_ann
save(ff5_daily_clean, file="ff5_daily_clean.RData")
save(ff5_monthly_clean, file="ff5_monthly_clean.RData")
save(ff5_annual_clean, file="ff5_annual_clean.RData")
proc.time() - pt        # 0.06 seconds.

# Note that now the returns in ff5_daily_clean and ff5_monthly_clean
#   are all in real value, not percentage.



# The end -----------------------------------------------------------------
