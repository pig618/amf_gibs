
###########################################################################
# Form customized data for multiple uses
#
# 1. Reform raw return, excess return, raw price, adjusted price, 
#    and market capital data to data frames with each row a date 
#    and each column a stock, ETF or factor.
# 
# 2. Pick universe by exchange code and share code and redo the above.
# 
# Read return data from 01_all_horizons folder. Separate the data for 
# stocks and ETFs based on id information in 02_id_class. All data created
# here should be in the form of a list with its first 3 elements being the
# stocks, ETFs, and FF5 (if included).
# 
# Each return with timestamp t is the return from (t - 1) to t.
# 
# All data here is of the whole time period. Specific time frame should be 
# picked in future analyses, not here. 
# 
###########################################################################

rm(list=ls())
gc()

source("../00_initial_code/01_func_help.R")
source("01_func_customized_data.R")






# #########################################################################
# # Check if returns are in percentage or not 
# #########################################################################
# 
# pt = proc.time() 
# 
# # Check security returns ------------------------------------------------
# load("../01_all_horizons/securities_monthly_clean.RData")
# is_perc(securities_monthly_clean$ret)
# rm(securities_monthly_clean)
# gc()
# 
# load("../01_all_horizons/securities_weekly_clean.RData")
# is_perc(securities_weekly_clean$ret)
# rm(securities_weekly_clean)
# gc()
# 
# load("../01_all_horizons/securities_daily_clean.RData")
# is_perc(securities_daily_clean$ret)
# rm(securities_daily_clean)
# gc()
# 
# # All securities returns are in real values, not percentage.
# 
# 
# 
# # Check FF5 returns -----------------------------------------------------
# ff5_names = c("rf", "mkt", "smb", "hml", "rmw", "cma")
# 
# load("../01_all_horizons/ff5_monthly_clean.RData")
# is_perc(unlist(ff5_monthly_clean[ff5_names]) * 100)
# is_perc(unlist(ff5_monthly_clean[ff5_names]))
# rm(ff5_monthly_clean)
# gc()
# 
# load("../01_all_horizons/ff5_weekly_clean.RData")
# is_perc(unlist(ff5_weekly_clean[ff5_names]) * 100)
# is_perc(unlist(ff5_weekly_clean[ff5_names]))
# rm(ff5_weekly_clean)
# gc()
# 
# load("../01_all_horizons/ff5_daily_clean.RData")
# is_perc(unlist(ff5_daily_clean[ff5_names]) * 100)
# is_perc(unlist(ff5_daily_clean[ff5_names]))
# rm(ff5_daily_clean)
# gc()
# 
# # Recall that in ff5 cleaning we changed all returns into real values, 
# # not percentage. So ff5 returns are in real values, not percentage.
#  
# proc.time() - pt      # 90 seconds






###########################################################################
# Read weekly data for all years and save the week counts
###########################################################################

pt = proc.time()
load("../01_all_horizons/securities_weekly_clean.RData")
proc.time() - pt                         # 46 seconds
load("../01_all_horizons/ff5_weekly_clean.RData")
load("../02_id_class/etf_info.RData")
proc.time() - pt                         # 1 second in addition



finite_or_na(dat=securities_weekly_clean, verbose=T, correct_if_not=F)
finite_or_na(dat=ff5_weekly_clean, verbose=T, correct_if_not=F)



# time_all_week is a character vector with all times available in data
end_before = "202000"
time_all = get_time_all(securities_weekly_clean, ff5_weekly_clean)
time_all = time_all[time_all < end_before]
time_all_week = time_all
save(time_all_week, file="time_all_week.RData")
rm(time_all_week)






###########################################################################
# Reform data (without restricting to trading universe)
###########################################################################

pt = proc.time()

raw_ret_week = reform_raw_ret(
  securities_weekly_clean, ff5_weekly_clean, time_all, verbose=T)
save(raw_ret_week, file="raw_ret_week.RData")

ex_ret_week = raw_to_ex_ret(raw_ret_week, verbose=T)
save(ex_ret_week, file="ex_ret_week.RData")

raw_prc_week = reform_raw_prc(
  securities_weekly_clean, time_all, verbose=T)
save(raw_prc_week, file="raw_prc_week.RData")

adj_prc_week = get_adj_prc(
  raw_ret=raw_ret_week, raw_prc=raw_prc_week, verbose=T)
save(adj_prc_week, file="adj_prc_week.RData")

prc_diff_week = get_prc_diff(prc=adj_prc_week, verbose=T)
save(prc_diff_week, file="prc_diff_week.RData")

mcap_week = reform_mcap(securities_weekly_clean, time_all, verbose=T)
save(mcap_week, file="mcap_week.RData")

proc.time() - pt              # 85 sec
rm(raw_ret_week, ex_ret_week, raw_prc_week, 
   adj_prc_week, prc_diff_week, mcap_week)
gc()






###########################################################################
# Reform data with restricting to trading universe
###########################################################################

load("../02_id_class/stocks_info.RData")
load("../02_id_class/etf_info.RData")
securities_weekly_universe = pick_universe(
  stocks_info, etf_info, securities_weekly_clean)

pt = proc.time()

time_all_universe = get_time_all(
  securities_weekly_universe, ff5_weekly_clean)
time_all = time_all[time_all < end_before]

raw_ret_week_universe = reform_raw_ret(securities_weekly_universe, 
                                       ff5_weekly_clean, 
                                       time_all_universe, verbose=T)
save(raw_ret_week_universe, file="raw_ret_week_universe.RData")

ex_ret_week_universe = raw_to_ex_ret(raw_ret_week_universe, verbose=T)
save(ex_ret_week_universe, file="ex_ret_week_universe.RData")

raw_prc_week_universe = reform_raw_prc(
  securities_weekly_universe, time_all_universe, verbose=T)
save(raw_prc_week_universe, file="raw_prc_week_universe.RData")

adj_prc_week_universe = get_adj_prc(
  raw_ret=raw_ret_week_universe, raw_prc=raw_prc_week_universe, verbose=T)
save(adj_prc_week_universe, file="adj_prc_week_universe.RData")

prc_diff_week_universe = get_prc_diff(
  prc=adj_prc_week_universe, verbose=T)
save(prc_diff_week_universe, file="prc_diff_week_universe.RData")

mcap_week_universe = reform_mcap(
  securities_weekly_universe, time_all_universe, verbose=T)
save(mcap_week_universe, file="mcap_week_universe.RData")

proc.time() - pt             # 50 sec
rm(raw_ret_week_universe, ex_ret_week_universe, 
   raw_prc_week_universe, adj_prc_week_universe, 
   mcap_week_universe)
gc()






###########################################################################
# The end 
###########################################################################
