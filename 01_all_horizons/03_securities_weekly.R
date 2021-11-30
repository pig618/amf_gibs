
###########################################################################
# Form weekly securities data.  
#
# 
# 
# Form weekly securities files from the daily securities data cleaned.
# Weekly returns are calculated from daily return. Use the last price
# of each week as the price of the week. Use the last value as the 
# week value for all other columns. Results are saved in
# "securities_weekly_clean.RData".
# 
# 
# 
# Input files:
# "securities_daily_clean.RData"
# 
# 
# 
# Output files:
# "securities_weekly_clean.RData"
# 
# 
# 
# Notes: 
# The Share outstanding is in thousands. 
# The market capital I calculated are transformed into
#   dollars, not thousands.
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



# Read in the securites daily cleaned data --------------------------------
pt = proc.time()
load(file="securities_daily_clean.RData")
secu = securities_daily_clean
proc.time()     # 212 seconds

# # Run this line when debug.
# secu = securities_daily_clean[1000:2000, ]    



# Find row index of first day and last day of a week ----------------------
if (!require(lubridate)) {install.packages('lubridate')}
library(lubridate)
date_date = as.Date(as.character(secu$date), format="%Y%m%d")
week_count = epiweek(date_date)
week_count = c(F, diff(week_count) != 0)
year_count = epiyear(date_date)
year_count = c(F, diff(year_count) != 0)
# Note that we can't use the year obtained by extracting the first 4 
#   digits of the date, since that will split edge week into two weeks!
# So we need to use epiyear(), which matches with epiweek(). 
# For example, epiyear("2013-12-31") is 2014, and 2014 is what we want.
# Weeks are merged to the year with majority of days.
permno_count = c(F, diff(secu$permno) != 0)

diff_day = which(week_count | year_count | permno_count)
# 1 is not in diff_day by construction
start_day = c(1, diff_day)
# start_day is the start day of each week
end_day = c(diff_day - 1, nrow(secu))
# end_day is the end day of each week

n_days = end_day - start_day + 1
range(n_days)
table(n_days)

# Check 6-day weeks
check_days = function(x) {
  cat(x, "\n")
  x_list = lapply(x, function(y) {
    as.Date(as.character(y), format="%Y%m%d")})
  cat(unlist(lapply(x_list, weekdays)), "\n")
  cat(sapply(x_list, epiweek), "\n\n")
}

start6 = start_day[which(n_days == 6)]
end6 = end_day[which(n_days == 6)]

for (i in 1:6) {
  check_days(secu$date[(start6[i] - 2) : (end6[i] + 2)])
}
# They all have Saturdays as trading day. These happens when 
#   the delisting days are on Saturdays. So these are correct.

# What we will use in the following are the start_day and end_day.



# Calculate the weekly return ---------------------------------------------
split_count = rep(0, nrow(secu))
split_count[start_day] = 1 
split_count = cumsum(split_count)

pt = proc.time()
ncores = detectCores() - 1
sfInit(parallel = TRUE, cpus=ncores)
ret_list = split(secu$ret, split_count)
ret_col = sfClusterApplyLB(ret_list, function(x){ 
  if (length(x) >= 7) {
    return(prod(unique(x + 1) - 1))
  } else {
    return(prod(x + 1) - 1)
  }})
ret_col = do.call(c, ret_col)
n_days = sfClusterApplyLB(ret_list, function(x) { length(x) })
n_days = do.call(c, n_days)
sfStop()
proc.time() - pt     
# 1570 seconds on 7 cpus for 2007 - 2018
# 4726 seconds on 7 cpus for 1987 - 2018
# 4843 seconds on 7 cpus for 1987 - 2019

# check number of days in each week
range(n_days)
table(n_days)  # As stated before, 6-day weeks are correct.



# Add other columns and week index ----------------------------------------
secu_week = secu[end_day, ]
secu_week$ret = ret_col

date_date = as.Date(as.character(secu_week$date), format="%Y%m%d")
week_count = epiweek(date_date)
week_char = formatC(week_count, width=2, format="d", flag="0")
# Add leading 0 to one digit integers
year = as.character(epiyear(date_date))
week_index = as.integer(paste(year, week_char, sep=""))
# Check if week_index is in order (locally)
week_index[500:700]



secu_week$last_day_date = secu_week$date
secu_week$date = week_index
col_names = c("permno", "date", "last_day_date", 
              setdiff(names(secu_week), c("permno", "date", "last_day_date")))
secu_week = secu_week[col_names]
# reorder the columns and put data and last_day_date at front.

check_bad_col(secu_week)
dim(secu)     # 59142941       39
dim(secu_week)     # 12268290       40
nrow(secu) / nrow(secu_week)    # 4.821 trading days in a week
head(secu_week)


# Save the cleaned weekly data --------------------------------------------
securities_weekly_clean = secu_week
pt = proc.time()
save(securities_weekly_clean, file="securities_weekly_clean.RData")
proc.time() - pt      # 127 seconds






# The end -----------------------------------------------------------------