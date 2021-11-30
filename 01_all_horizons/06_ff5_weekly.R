
###########################################################################
# Form weekly Fama-French 5-factor data.  
#
# 
# 
# Form weekly Fama-French 5 factor returns and save results in 
# the file "ff5_weekly_clean.RData". Note that here the returns are
# all in real values, not percentage.
# 
# 
# 
# Input files:
# "ff5_daily_clean.RData"
# 
# 
# 
# Output files:
# "ff5_weekly_clean.RData"
# 
# 
# 
# Notes: 
# Load the daily ff5 data to form weekly ff5 data. The returns are 
#   already in real value (not percentage) in the ff5_daily_clean.RData,
#   so weekly returns are still  in real value, not percentage.
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



# Load the ff5 daily data -------------------------------------------------
pt = proc.time() 
load("ff5_daily_clean.RData")
ff5 = ff5_daily_clean
proc.time() - pt     # almost 0 second.

combine_real = function(x) {
  # combine returns if returns are real values not percentage
  # real to real
  x = x + 1
  y = apply(x, 2, prod) - 1
  y
}



# Find row index of first day and last day of a week ----------------------
if (!require(lubridate)) {install.packages('lubridate')}
library(lubridate)
week_count = epiweek(as.Date(as.character(ff5$date), format="%Y%m%d"))
week_count = c(F, diff(week_count) != 0)
year_count = epiyear(as.Date(as.character(ff5$date), format="%Y%m%d"))
year_count = c(F, diff(year_count) != 0)

diff_day = which(week_count | year_count)
# 1 is not in diff_day by construction
start_day = c(1, diff_day) 
# start_day is the start day of each week
end_day = c(diff_day - 1, nrow(ff5))
# end_day is the end day of each week 

n_days = end_day - start_day + 1 
range(n_days)
table(n_days)






# Calculate the weekly ff5 return -----------------------------------------
col_names = c("date", "rf", "mkt", "smb", "hml", "rmw", "cma")
ret_names = setdiff(col_names, "date")

split_count = rep(0, nrow(ff5))
split_count[start_day] = 1 
split_count = cumsum(split_count)
# Note that split_count is different from the first week_count, in the
#   first week_count the n-th week of each year has the same number,
#   which will lead to big mistake if that is misused as split_count.

pt = proc.time()
ncores = detectCores() - 1
sfInit(parallel=TRUE, cpus=ncores)
ret_list = split(ff5[ret_names], split_count)
ret_cols = sfClusterApplyLB(ret_list, combine_real)
ret_cols = do.call(rbind, ret_cols)
n_days = sfClusterApplyLB(ret_list, function(x) { nrow(x) })
n_days = do.call(c, n_days)
sfStop()
proc.time() - pt        # 3 seconds

# check number of days in each week
range(n_days)
table(n_days)
nrow(ff5) / nrow(ret_cols)



# Add other columns and put week index in date column ---------------------
ff5_week = ff5[end_day, ]
ff5_week[ret_names] = ret_cols



# Form week index
date_date = as.Date(as.character(ff5_week$date), format="%Y%m%d")
week_count = epiweek(date_date)
week_char = formatC(week_count, width=2, format="d", flag="0")
# Add leading 0 to one digit integers
year = as.character(epiyear(date_date))
week_index = as.integer(paste(year, week_char, sep=""))
# Check if week_index is in order
week_index[1:100]
all(week_index == sort(week_index))



# Save the date of last date in column "last_day_date"
# Put week index in the column "date"
ff5_week$last_day_date = ff5_week$date
ff5_week$date = week_index

col_names = c("date", "last_day_date", "rf", "mkt", 
              "smb", "hml", "rmw", "cma")
ff5_week = ff5_week[col_names]
# reorder the columns

check_bad_col(ff5_week)
head(ff5)
head(ff5_week)
dim(ff5)
dim(ff5_week)
nrow(ff5) / nrow(ff5_week)    
all(ff5$date == sort(ff5$date))


# Save the cleaned weekly data --------------------------------------------
ff5_weekly_clean = ff5_week
pt = proc.time()
save(ff5_weekly_clean, file = "ff5_weekly_clean.RData")
proc.time() - pt       # 0.07 seconds

# Note that here the returns in ff5_weekly_clean are in !!PERCENTAGE!! to 
#   be consistent with other ff5 data downloaed!






# The end -----------------------------------------------------------------