
###########################################################################
# Clean daily securities data.  
# 
# 
# 
# Clean the daily securities data. Deal with NAs, correct prices, 
# add delising returns to returns, and add market capital column. 
# Save the cleaned data in "securities_daily_clean.RData".
#  
# 
# 
# Input files:
# "../00_data/01_securities_data/securities_daily.txt"
# 
# 
# 
# Output files:
# "securities_daily_clean.RData"
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


# Read in the daily securities data -----------------------------------------
pt = proc.time()
secu = fread(input="../00_data/01_securities_data/securities_daily.txt", 
             data.table=F)
proc.time() - pt    # 180 seconds

# Change all columns names into lowercase for coding style
names(secu) = tolower(names(secu))



# Smell check for the original data 
names(secu)
# [1] "permno"  "date"    "shrcd"   "exchcd"  "siccd"   "ncusip"  "ticker" 
# [8] "comnam"  "shrcls"  "naics"   "permco"  "issuno"  "hexcd"   "hsiccd" 
# [15] "cusip"   "dlamt"   "dlpdt"   "dlstcd"  "nwperm"  "dlprc"   "dlret"  
# [22] "nmsind"  "bidlo"   "askhi"   "prc"     "vol"     "ret"     "shrout" 
# [29] "openprc" "numtrd"  "retx"    "vwretd"  "ewretd"  "sprtrn" 
length(names(secu))           # 34 columns
length(unique(secu$permno))   # 12790 stocks
range(secu$date)              # 20070103 20180629
dim(secu)                     # 20156324       34
check_bad_col(secu)
# clean the data for each bad column as the following



# Correct the ticker (ticker) symbol, check NA value ----------------------
na_id = which(is.na(secu$ticker))

# (na_len = length(na_id))  # 1490
# max(na_id[-1] - na_id[-na_len])    # 1
# # so there is only 1 continue block of NAs
# range(na_id)
# unique(secu$permno[na_id])
# # Only one permno 81191, after searching in CRSP,
# #   we know its ticker is "NA", which is mis-understood as NA by R.

secu$ticker[na_id] = "NA"
check_bad_col(secu)
# ticker corrected.



# Clean the price (prc) column --------------------------------------------
secu$prc = as.numeric(secu$prc)
good_id = which(is.finite(secu$prc))
length(good_id) / nrow(secu)

secu = secu[good_id, ]
# remove rows without a valid price. 
# Interpolations (if there should be any) should be done in later stages.

# Since negative sign means no closing value and the average of 
#   the bid and ask price will be used instead in CRSP, 
#   we can just replace all negative prices by its opposite value.
secu$prc = abs(secu$prc)



# Clean return, delisting return and delisting price ----------------------
secu$ret = as.numeric(secu$ret)
secu$dlret = as.numeric(secu$dlret)

secu$dlprc = as.numeric(secu$dlprc)
secu$dlprc = abs(secu$dlprc)

dlret_id = which(is.finite(secu$dlret))
dlprc_id = which(is.finite(secu$dlprc))
length(dlret_id)
length(dlprc_id)
all(dlret_id %in% dlprc_id)
# So if there is a valid delisting return, there is a valid delisting
#   price. However, sometimes the dlret is NA when the dlprc is not. 

mean(secu$dlret, na.rm=T)
secu$dlret[setdiff(dlprc_id, dlret_id)] = 0 
# When there is a dlprc but no dlret, replace the NA in dlret with 0, 
#   since the mean delisting return is close to 0.



# Add dlret and dlprc to ret and prc --------------------------------------
dl_id = which(is.finite(secu$dlprc))
dl_date_appear = (secu$date[dl_id + 1] == secu$dlpdt[dl_id])
dl_date_appear = dl_date_appear[!is.na(dl_date_appear)]
any(dl_date_appear)   
# No delisting date appears. So add these rows as the following.



# Add rows for delisting days
split_data = function(i) {
  # Split the data frame into blocks, add the rows in
  #   then in later code combine them using the function do.call()
  if (i %% 2 == 0){
    block = secu[dl_id[i / 2], ]
    if (is.na(block$dlpdt)) {
      block$date = trade_days[which(trade_days == block$date) + 1]
    } else {
      block$date = block$dlpdt
    }
    block$ret = block$dlret
    block$prc = block$dlprc
    block[na_cols] = NA
    return(block)
  }
  
  if (i == 1){
    start_id = 1
  } else {
    start_id = dl_id[(i - 1) / 2] + 1
  }
  
  if (i == 2 * length(dl_id) + 1){
    end_id = nrow_secu
  } else {
    end_id = dl_id[(i + 1) / 2]
  }
  
  if (start_id <= end_id & end_id <= nrow_secu){
    block = secu[start_id : end_id, ]
  } else {
    block = NULL
  }
  
  return(block)
}

pt = proc.time()
sfInit(parallel=FALSE)
# sequentially computing, since secu is too large to export to nodes

trade_days = sort(unique(secu$date), na.last=NA)
last_day_next = as.Date(as.character(tail(trade_days, n=1)), 
                        format="%Y%m%d") + 3
trade_days = c(trade_days, 
               as.integer(as.character(last_day_next, format="%Y%m%d")))
# 2018-06-29 is Friday, 2018-07-02 is the next Monday.

na_cols = c("dlamt", "dlpdt", "dlstcd", "dlprc", "dlret",
            "bidlo", "askhi", "vol", "openprc", "numtrd",
            "retx", "vwretd", "ewretd", "sprtrn")
nrow_secu = nrow(secu)
# sfExport("secu", "dl_id", "trade_days", "na_cols", "nrow_secu")

if (length(dl_id) > 0) {
  secu_list = sfClusterApplyLB(1 : (2 * length(dl_id) + 1), split_data)
}
secu = do.call(rbind, secu_list)
sfStop()
proc.time() - pt
# 2221.32 seconds for 1980 - 2018, 126 seconds for 2007 -2018

# # This script block tests the above code block
# temp = secu
# temp_dl_id = dl_id
# secu = secu[1:20, ]
# dl_id = c(1, 5, 7, 20)
# secu$dlret[dl_id] = 1
# secu$dlpdt[dl_id] = 9999
# secu$dlprc[dl_id] = 8888
# secu
# # run the above code
# secu
# # recover back
# secu = temp
# dl_id = temp_dl_id

# # check of the results
# head(dl_id)
# secu[2660:2670, ]
# check_bad_col(secu)



# Clean the return column -------------------------------------------------
check_bad_col(secu)
# there is no NA in price, but still NAs in return.

# na_id = which(is.na(secu$ret))
# na_id = setdiff(na_id, 1)
# stock_create_id = na_id[which(secu$permno[na_id] != secu$permno[na_id - 1])]
# length(stock_create_id) / length(na_id)
# # Most NAs are created at the creation (first row) of a stock 
# #   since there are no previous values.
# # Remove these rows since there is no way to replace these NAs in return
# #   and it is acceptable to discard the first price.
# secu = secu[setdiff(1:nrow(secu), stock_create_id), ]
# if (1 %in% na_id) {
#   secu = secu[-1, ]
# }
# 
# na_id = which(is.na(secu$ret))
# # NAs due to share split or merge, remove these rows.
# # Not too many of them. Only 149 in 2007 - 2018.
# secu = secu[setdiff(1:nrow(secu), na_id), ]

# For the reason discussed above, remove all rows with NA return.
dim(secu)
secu = secu[which(is.finite(secu$ret)), ]
dim(secu)
check_bad_col(secu)



# Add market capital column -----------------------------------------------
secu$mcap = secu$shrout * secu$prc * 1000 
# The share outstanding (shrout) is in thousands in CRSP. See the website:  
# http://www.crsp.com/files/data_descriptions_guide_0.pdf
# So I multiply them by 1000 to make mcap with the unit of dollar.
check_bad_col(secu)



# Save the cleaned daily data ---------------------------------------------
dim(secu)     # 19977634 35   for 2007 - 2018
head(secu)
check_bad_col(secu)

securities_daily_clean = secu
pt = proc.time()
save(securities_daily_clean, file="securities_daily_clean.RData")
proc.time() - pt      # 627 seconds



# the end -----------------------------------------------------------------
