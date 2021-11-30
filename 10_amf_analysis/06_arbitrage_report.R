
###########################################################################
###########################################################################
###########################################################################
# 
# Dynamic portfolio arbitrage test report
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()
source("../00_initial_code/01_func_help.R")
libs_local()
source("../04_gibs_algo/05_func_report.R")
par(mar=c(5, 6, 4, 3), lwd=1.5, cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

pt_all = proc.time()
load(file="../03_customized_data/raw_ret_week.RData")
load(file="../03_customized_data/time_all_week.RData")



###########################################################################
# Form intecept list
###########################################################################

parent_folder = "arbitrage_results"
folders = list.dirs(path=paste("./", parent_folder, sep=""))
# folders = paste("./", parent_folder, "/", folders, sep="")
folders = setdiff(folders, paste("./", parent_folder, sep=""))

folder = folders[1]
folder 

dates = sapply(folders, function(x) {
  substring(x, first=nchar(x) - 5)})
names(dates) = NULL
dates_next = date_change(
  cur_dates=dates, all_dates=time_all_week, 
  start_change=1, end_change=1)

int_list = list()
for (i in 1:length(folders)) {
  res = result_read(path_name=folders[i], result_name="results", 
                    var_names=NULL)
  int_list[[dates_next[i]]] = res
}
save(int_list, file=paste(
  "./", parent_folder, "/int_list.RData", sep=""))



###########################################################################
# Derive dynamic portfolio excess return data frame
###########################################################################

pt = proc.time()

load(file=paste("./", parent_folder, "/int_list.RData", sep=""))

q = c(0, 0.5)

dat = raw_ret_week$stocks[names(int_list), , drop=F]

port_ret_func = function(date) {
  # Give the high_ret and low_ret
  # high_ret is the return of portfolio with positive alpha
  # low_ret is the rutnr of portfolio with negative alpha
  date = as.character(date)
  
  res = do.call(rbind, lapply(int_list[[date]], function(x) { 
    data.frame(permno=x$permno, 
               estimate=x$int_gibs["Estimate"], 
               pval=x$int_gibs["Pr(>|t|)"])
  }))
  
  rownames(res) = NULL
  res$qval = p.adjust(res$pval, method="fdr")
  
  res = subset(res, pval < 0.05)
  res = res[order(res$estimate, decreasing=T), ]
  
  high_n = sum(res$estimate > 0)
  high_pers = as.vector(res[floor(q[1] * high_n) : ceiling(q[2] * high_n), 
                            "permno", drop=T])
  high_ret = mean(unlist(dat[date, high_pers]), na.rm=T)
  
  low_n = sum(res$estimate < 0)
  low_pers = as.vector(res[(floor((1 - q[2]) * low_n) + high_n) : 
                             (ceiling((1 - q[1]) * low_n) + high_n),       
                           "permno", drop=T])
  low_ret = mean(unlist(dat[date, low_pers]), na.rm=T)
  
  
  cat(date, " finished! \n")
  data.frame(date=date, high_ret=high_ret, low_ret=low_ret)
}

# sfExport("port_ret_func", "int_list", "q", "dat")
port_raw_ret = do.call(rbind, lapply(names(int_list), port_ret_func))
port_raw_ret = as.data.frame(port_raw_ret)

proc.time() - pt   # 130 seconds

save(port_raw_ret, file=paste("./", parent_folder, "/port_raw_ret.RData", sep=""))






###########################################################################
# Form Portfolios
###########################################################################

# Read in portfolio return data
load(file=paste("./", parent_folder, "/port_raw_ret.RData", sep=""))

port = port_raw_ret
colnames(port) = c("date", "high_raw_ret", "low_raw_ret")
rf = raw_ret_week$ff5[port_raw_ret$date, "rf", drop=T]
port$rf = rf
port$rf_cum = cumprod(port$rf + 1) - 1
rm(rf)

port$high_ex_ret = port$high_raw_ret - port$rf
port$long_cap = cumprod(port$high_raw_ret + 1)
port$high_cum_ex_ret = port$long_cap - 1 - port$rf_cum

port$low_ex_ret = port$low_raw_ret - port$rf
port$short_cap = cumprod(port$low_raw_ret + 1)
port$low_cum_ex_ret = port$short_cap - 1 - port$rf_cum

port$zero_inv_cap = port$long_cap - port$short_cap
port$zero_inv_val_change = diff(c(0, port$zero_inv_cap))

rownames(port) = port$date
port$date = NULL

port

write.csv(round(port, 3), row.names=T,
          file=paste("./", parent_folder, "/portfolio.csv", sep=""))






###########################################################################
# Draw Plots
###########################################################################

# Parameters of the plots
par(mar=c(5, 7, 5, 3), lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)



# Excess return of long only and short only portfolios
rg = range(c(port$high_ex_ret, port$low_ex_ret))
plot(1:nrow(port), port$high_ex_ret, 
     main=paste("Excess Return of ", 
                  "Long Only and Short Only portfolios", sep=""),
     type="l", col="blue", lty=1, 
     ylim=rg, ylab="Excess return", xlab="weeks", 
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(1:nrow(port), port$low_ex_ret, 
      type="l", col="red", 
      lty=2, lwd=3)
abline(h=0, col="yellow")
legend(40, 0.19, c("Long", "Short"),
       lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)



# Cumulative excess return of long only and short only portfolios 
rg = range(c(port$high_cum_ex_ret, port$low_cum_ex_ret))
plot(1:nrow(port), port$high_cum_ex_ret, 
     main=paste("Cumulative Excess Return of ",
                  "Long Only and Short Only portfolios", sep=""),
     type="l", col="blue", lty=1, 
     ylim=rg, ylab="Cumulative excess return", xlab="weeks", 
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(1:nrow(port), port$low_cum_ex_ret, 
      type="l", col="red", 
      lty=2, lwd=3)
abline(h=0, col="yellow")
legend(26, 0.23, c("Long", "Short"), 
       lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)



# Value change of 0-investment portfolio
plot(1:nrow(port), 100 * port$zero_inv_val_change, 
     main=paste("0-investment portfolio percentage of value change", sep=""),
     type="b", col="blue", lty=1, 
     ylim=c(-25, 15), ylab="Percantage of Value change", 
     xlab="weeks", 
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
abline(h=0, col="yellow")



# Capital Value of 0-investment portfolio
plot(0:nrow(port), c(0, port$zero_inv_cap), 
     main=paste("0-investment portfolio value", sep=""),
     type="b", col="blue", lty=1, 
     ylab="value", xlab="weeks", 
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
abline(h=0, col="yellow")






###########################################################################
# The end 
###########################################################################
