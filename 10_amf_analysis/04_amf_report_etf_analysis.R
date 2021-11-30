
###########################################################################
###########################################################################
###########################################################################
# 
#  ETF analysis:
#  1. ETF representatives report
#  2. Risk-factor determination by risk premium
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()

source("../00_initial_code/01_func_help.R")
libs_local()

source("../02_id_class/01_func_class_presentation.R")

load(file="../02_id_class/etf_info.RData")






# #########################################################################
# # Select etf by GIBS clustering
# # Can skip this section if analyzing existing data
# #########################################################################
# 
# load(file="../03_customized_data/ex_ret_week.RData")
# load(file="../03_customized_data/time_all_week.RData")
# source("../04_gibs_algo/01_func_gibs_help.R")
# source("../04_gibs_algo/02_func_gibs_cluster.R")
# source("../04_gibs_algo/03_func_gibs_prep.R")
# 
# # Pick data in the time period
# start = 2014
# end = 2016
# start_week = start * 100
# end_week = (end + 1) * 100
# 
# data_1period = prep_1period(
#   start=start_week, end=end_week, pred_len=0, 
#   ret=ex_ret_week, prc=NULL, prc_diff=NULL, 
#   reps_by="ret", etf_info=etf_info, time_all=time_all_week, 
#   etf_th=1, stocks_th=0.8)
# 
# folder_name = "reports"
# if (!dir.exists(folder_name)) {
#   dir.create(folder_name)
# }
# save(data_1period, 
#      file=paste(folder_name, "/data_1period.RData", sep=""))






###########################################################################
# Present the ETF representatives
###########################################################################

folder_name = "reports"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
load(file=paste(folder_name, "/data_1period.RData", sep=""))

etf_per_keep = as.integer(substring(
  data_1period$etf_reps_info$etf_per_keep, first=4))
class_presentation(etf_per_keep, etf_info$etf_class, etf_info$class_table)

# Save the selected ETF details into a csv file.
etf_keep_details = subset(etf_info$etf_class, permno %in% etf_per_keep)
etf_keep_details = etf_keep_details[order(etf_keep_details$category_no), ]
write.csv(etf_keep_details, file=paste(
  folder_name, "/etf_keep_details.csv", sep=""), row.names=F)

# Create latex readable code by xtable()
if (!require(xtable)) {
  install.packages("xtable")
}
library(xtable)

etf_tab = subset(etf_keep_details,
                 select=c("etf_name", "category"))
colnames(etf_tab) = c("ETF Names", "Category")

dim(etf_tab)
tail(etf_tab)

etf_tab = xtable(etf_tab, caption="Low-correlated ETF name list")
add.to.row = list(pos=list(0), command=NULL)
command = paste0("\\hline\n\\endhead\n",
                 "\\hline\n",
                 "\\multicolumn{", dim(etf_tab)[2], "}{l}",
                 "{\\footnotesize Continued on next page}\n",
                 "\\endfoot\n", "\\endlastfoot\n")
add.to.row$command = command
print(etf_tab, hline.after=1:nrow(etf_tab), add.to.row=add.to.row,
      include.rownames=F,
      tabular.environment="longtable",
      floating=F)
# Use "\multicolumn{2}{l}" instead of "\multicolumn{3}{l}" in
#   the latex code since we don't print rownames
# Then break long names manually.






###########################################################################
# Risk Premium Calculation
###########################################################################

load(file="../03_customized_data/adj_prc_week.RData")
source(file="../04_gibs_algo/01_func_gibs_help.R")

start = 2014
end = 2016
start_week = start * 100
end_week = (end + 1) * 100

end_week = end_week + 1
# To calculate the average annual excess return, need to add one week

dat = pick_time_frame(dat=adj_prc_week, start=start_week, end=end_week, 
                      etf_th=1, stocks_th=0.8)
fac = cbind(dat$ff5, dat$etf)

# Only focus on the basis assets
ff5_names = c("rf", "mkt", "smb", "hml", "rmw", "cma")
dim(fac)
fac = fac[, c(ff5_names, paste("per", etf_per_keep, sep=""))]
dim(fac)

# Calculate average yearly return
ret_yr = unlist(fac[nrow(fac), ] / fac[1, ] - 1)
ret_yr = ret_yr / (end - start + 1)

# Subtract the risk free rate
ret_yr = ret_yr - ret_yr["rf"]
ret_yr = ret_yr[2:length(ret_yr)]
ff5_names = ff5_names[ff5_names != "rf"]

ff5_ret = ret_yr[ff5_names]
ff5_ret
library(xtable)
print(xtable(as.data.frame(t(ff5_ret) * 100), digits=1),
      include.rownames=F)

ret_yr = ret_yr[!names(ret_yr) %in% ff5_names]

# Find ETFs with risk premium bigger than min of FF5
ind_good = abs(ret_yr) > min(abs(ff5_ret))
sum(ind_good)
(sum(ind_good) + 5) / (length(ind_good) + 5)

# Find ETFs with risk premium bigger than max of FF5
ind_big = abs(ret_yr) > max(abs(ff5_ret))
sum(ind_big)

length(ret_yr)

per_big = substring(names(ret_yr)[ind_big], first=4)
etf_tab = subset(etf_info$etf_class, permno %in% per_big)
etf_tab$"risk premium" = ret_yr[paste("per", etf_tab$permno, sep="")] * 100
etf_tab = subset(etf_tab, select=c("etf_name", "category", "risk premium"))
etf_tab = etf_tab[order(abs(etf_tab$"risk premium"), decreasing=T), ]
etf_tab

library(xtable)
print(xtable(etf_tab, digits=1), include.rownames=F, 
      hline.after=1:nrow(etf_tab))






###########################################################################
# The end 
###########################################################################
