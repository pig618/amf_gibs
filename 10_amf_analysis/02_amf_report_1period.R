
###########################################################################
###########################################################################
###########################################################################
# 
#  Adaptive Multi-Factor (AMF) model results report for 1 period
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()

source("../00_initial_code/01_func_help.R")
libs_local()
source("../04_gibs_algo/05_func_report.R")

par(mar=c(5, 6, 4, 4), lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)






###########################################################################
# Read and reform results
###########################################################################

# Specify the parent folder, start year and end year
parent_folder = "amf_results"
start_year = 2014
end_year = 2016

# Read in all results to be used ------------------------------------------
path_name = paste("./", parent_folder, "/results_", 
                  start_year, "_", end_year, sep="")
result_name = "results"

scalar_names = c("permno", "f_pval", "resid_ff5", "resid_gibs", 
                 "rs_ff5", "rs_gibs", "ars_ff5", "ars_gibs", 
                 "mse_ff5", "mse_gibs", "osrs_ff5", "osrs_gibs")
list_names = c("select_gibs", "fac_sgn_ff5", "fac_sgn_gibs", 
               "cii_ff5", "cii_gibs", "int_ff5", "int_gibs",
               "lr_measures", "lr_pval", "pred_ff5", "pred_gibs",
               "stock_to_pred", "ybar_history")
var_names = c(scalar_names, list_names)

res = result_read(path_name, result_name, var_names=NULL)

# Remove outliers ---------------------------------------------------------
# Outliers are found in the following way:
#   1. Rerun the code with this block commented out.
#   2. In each section find extreme ones that we want to remove.
#   3. Find its permno and remove it here 
#      (also write the reason as comments).
length(res)
res[["per91560"]] = NULL  # its orsr_ff5 is -353008
length(res)

# Reform the results ------------------------------------------------------
res = result_reform(res)

res$fit = result_reform(res$fit, var_names)
res$fit = c(result_to_vec(res$fit, scalar_names), res$fit[list_names])
res$fit$n_select = sapply(
  res$fit$select_gibs, function(z) {length(z$x)} )
res$fit$n_sgn_gibs = sapply(res$fit$fac_sgn_gibs, length)
res$fit$n_sgn_ff5 = sapply(res$fit$fac_sgn_ff5, length)

res$pred = result_reform(res$pred, var_names)
res$pred = c(result_to_vec(res$pred, scalar_names), 
            res$pred[list_names])
res$pred$n_select = sapply(
  res$pred$select_gibs, function(z) {length(z$x)} )
res$fit$n_sgn_gibs = sapply(res$fit$fac_sgn_gibs, length)
res$fit$n_sgn_ff5 = sapply(res$fit$fac_sgn_ff5, length)

# Save the reports
reports = res 
folder_name = "reports"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
save(reports, file=paste(folder_name, "/reports.RData", sep=""))






###########################################################################
# Out of sample R Squared
###########################################################################

which(res$pred$osrs_ff5 < -100)

mean(res$pred$osrs_ff5)
mean(res$pred$osrs_gibs)
(mean(res$pred$osrs_gibs) / mean(res$pred$osrs_ff5) - 1) * 100

# kernal density plot -----------------------------------------------------
d1 = density(res$pred$osrs_ff5, kernel="gaussian", adjust=1,
             from=-1, to=1)
d2 = density(res$pred$osrs_gibs, kernel="gaussian", adjust=1,
             from=-1, to=1)
rg = range(0, 2.5)
plot(d1, ylim=rg,
     xlab=expression(paste("Out-of-Sample ", R^2)),
     ylab="Distribution Density",
     main=expression(paste("Distribution of Out-of-Sample ", R^2)),
     type="l", col="blue", lty=1,
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(d2, type="l", col="red", lty=2, lwd=3)
legend(-1, 2.5, c("FF5 model", "AMF Model"),
       lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)

# # Plot OSRS of gibs vs Number of factors (only for tuning) --------------
# plot(res$pred$n_select, res$pred$osrs_gibs, 
#      main="OSRS of gibs vs Number of factors")
# abline(h=0, col="green")






###########################################################################
# In-sample adjusted R squared
###########################################################################

mean(res$fit$ars_ff5)
mean(res$fit$ars_gibs)
(mean(res$fit$ars_gibs) / mean(res$fit$ars_ff5) - 1) * 100

range(c(res$fit$ars_ff5, res$fit$ars_gibs))

# Kernel density plot -----------------------------------------------------
d1 = density(res$fit$ars_ff5, kernel="gaussian", adjust=2,
             from=0, to=1)
d2 = density(res$fit$ars_gibs, kernel="gaussian", adjust=2,
             from=0, to=1)
range(c(d1$y, d2$y))
rg = c(0, 2.5)
plot(d1, ylim=rg,
     xlab=expression(paste("Adjusted ", R^2)),
     ylab="Distribution Density",
     main=expression(paste("Distribution of Adjusted ", R^2)),
     type="l", col="blue", lty=1,
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=2)
lines(d2, type="l", col="red", lty=2, lwd=3)
legend(0.7, 2.5, c("FF5 model", "AMF Model"),
       lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)






###########################################################################
# In-sample adjusted R squared
###########################################################################

# FDR on p values
f_pval = res$fit$f_pval
id_different = which(is.finite(f_pval))
n = length(res$fit$f_pval)
length(id_different)
n - length(id_different)
length(id_different) / n

p_val = f_pval[id_different]
bh_val = p.adjust(p_val, method="BH")
bhy_val = p.adjust(p_val, method="BY")


tab = NULL
rgs = list(c(0, 1e-2), c(0, 0.05), c(0.05, 1))
for (rg in rgs) {
  perc = sapply(list(p_val, bh_val, bhy_val), function(x){
    sum((x >= rg[1]) & (x <= rg[2])) / length(x) * 100
  })
  tab = rbind(tab, perc)
}
tab = as.data.frame(tab)
colnames(tab) = c("p-val", "bh-val", "bhy-val")
rg_col = sapply(rgs, function(x) {
  paste(x[1], " - ", x[2], sep="")
})
tab = cbind("value ranges"=rg_col, tab)
rownames(tab) = NULL
tab

library(xtable)
print(xtable(tab), include.rownames=F, 
      hline.after=seq(-1, nrow(tab), 1))






###########################################################################
# Selected and significant basis assets count
###########################################################################

# All basis assets that was selected at least once
Reduce(union, lapply(res$fit$select_gibs, function(z) { z$x }))

mean(res$fit$n_select)
mean(res$fit$n_sgn_gibs)
mean(res$fit$n_sgn_ff5)

# Histogram pair ----------------------------------------------------------
par(mar=c(5, 6, 4, 3), lwd=2, 
    cex.axis=1.5, cex.lab=1.5, cex.main=1.5)

par(mfrow=c(1, 2))
hist(res$fit$n_select, 
     breaks=seq(min(res$fit$n_select) - 0.5, 
                max(res$fit$n_select) + 0.5, 1),
     xlab="Number of selected basis assets", 
     main="Distribution of the number \n of selected basis assets", 
     lwd=2)
hist(res$fit$n_sgn_gibs, 
     breaks=seq(min(res$fit$n_sgn_gibs) - 0.5, 
                max(res$fit$n_sgn_gibs) + 0.5, 1),
     xlab="Number of significant basis assets", 
     main="Distribution of the number \n of significant basis assets", 
     lwd=2)
par(mfrow=c(1, 1))






###########################################################################
# Intercept test 
###########################################################################

p_ff5 = sapply(res$fit$int_ff5, function(x) { x["Pr(>|t|)"] })
p_gibs = sapply(res$fit$int_gibs, function(x) { x["Pr(>|t|)"] })

# kernal density plot ----------------------------------------------------
d1 = density(p_ff5, kernel="gaussian", adjust=1,
             from=0, to=1)
d2 = density(p_gibs, kernel="gaussian", adjust=1,
             from=0, to=1)
rg = range(c(d1$y, d2$y + 0.2))
plot(d1, ylim=rg,
     xlab="P-value for the intercept test",
     ylab="Distribution Density",
     main=paste("Distribution of intercept test p-values"),
     type="l", col="blue", lty=1,
     lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
lines(d2, type="l", col="red", lty=2, lwd=3)
abline(v=0.05, col="green")
legend(0.67, 1.3, c("FF5 model", 
                    "AMF Model"),
       lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)

# Q-threshold ------------------------------------------------------------
q_ff5 = p.adjust(p_ff5, method="BY")
q_gibs = p.adjust(p_gibs, method="BY")

all(q_ff5 > 0.1)
all(q_gibs > 0.1)

table(q_ff5)
table(q_gibs)

breaks = c(0, 0.05, 0.9, 1)
p_ff5 = as.data.frame(table(cut(
  p_ff5, breaks=breaks,
  labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
q_ff5 = as.data.frame(table(cut(
  q_ff5, breaks=breaks,
  labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
p_gibs = as.data.frame(table(cut(
  p_gibs, breaks=breaks,
  labels=paste(breaks[-length(breaks)], "-",breaks[-1]))))
q_gibs = as.data.frame(table(cut(
  q_gibs, breaks=breaks,
  labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
tab = do.call(cbind, lapply(list(p_ff5, p_gibs, q_ff5, q_gibs),
                            function(x) { x["Freq"] }))
tab = do.call(cbind, lapply(tab, function(x) {
  round(x / sum(x) * 100, 2) }))
tab = cbind(p_ff5["Var1"], tab)
colnames(tab) = c("Range", "FF5 p-val", "AMF p-val",
                  "FF5 FDR q-val", "AMF FDR q-val")
tab
library(xtable)
print(xtable(tab),
      include.rownames=F,
      hline.after=seq(-1, nrow(tab), 1))





###########################################################################
# The end 
###########################################################################




# 
# ###########################################################################
# # Significant factor count (In-sample test)
# # (A more detailed significant factor table is in the other R script
# #  04_sgn_table_heatmap.R)
# ###########################################################################
# 
# # Read in the results
# folder_name = "amf_results_2014_2016"
# file_name = "amf_fit_results"
# scalar_names = NULL
# vector_names = c("select_gibs", "sgn_gibs")
# var_names = c(scalar_names, vector_names)
# res = read_results(folder_name, file_name, scalar_names, vector_names)
# assign_batch(var_names, res[var_names])
# 
# # Length of all factors that are selected (or sgnificant) at least once
# length(unique(unlist(sgn_gibs)))  
# length(unique(unlist(select_gibs)))
# # all factors were selected at least once
# 
# # Create select_count and sgn_count
# select_count = sapply(select_gibs, function(x) { length(x) })
# sgn_count = sapply(sgn_gibs, function(x) { length(x) })
# range(c(select_count, sgn_count))
# 
# mean(sgn_count)
# mean(select_count)
# 
# 
# 
# # Histogram pair ----------------------------------------------------------
# par(mar=c(5, 6, 4, 3), lwd=2, 
#     cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
# 
# par(mfrow=c(1, 2))
# hist(select_count, 
#      breaks=seq(min(select_count) - 0.5, max(select_count) + 0.5, 1),
#      xlab="Number of selected basis assets", 
#      main="Distribution of the number \n of selected basis assets", 
#      lwd=2)
# hist(sgn_count, 
#      breaks=seq(min(sgn_count) - 0.5, max(sgn_count) + 0.5, 1),
#      xlab="Number of significant basis assets", 
#      main="Distribution of the number \n of significant basis assets", 
#      lwd=2)
# par(mfrow=c(1, 1))
# 
# # # kernal density plot
# # d1 = density(select_count, kernel="gaussian", adjust=2, 
# #              from=0, to=21)
# # d2 = density(sgn_count, kernel="gaussian", adjust=2, 
# #              from=0, to=21)
# # rg = range(c(d1$y, d2$y))
# # plot(d1, ylim=rg, 
# #      xlab=expression(paste("Adjusted ", R^2)), 
# #      ylab="Distribution Density", 
# #      main=paste("Distribution of factor count"), 
# #      type="l", col="blue", lty=1, 
# #      lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
# # lines(d2, type="l", col="red", lty=2, lwd=3)
# # legend(15, 0.10, c("Selected", "Significant"), 
# #        lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)
# 
# 
# 
# 
# 
# 
# ###########################################################################
# # Intercept test (In-sample test)
# ###########################################################################
# 
# # Read in the results
# folder_name = "amf_results"
# file_name = "amf_fit_results"
# scalar_names = NULL
# vector_names = c("int_ff5", "int_gibs")
# var_names = c(scalar_names, vector_names)
# res = read_results(folder_name, file_name, scalar_names, vector_names)
# assign_batch(var_names, res[var_names])
# 
# # Extract p-values
# int_pval_ff5 = sapply(int_ff5, function(x) { x[2] })
# int_pval_gibs = sapply(int_gibs, function(x) { x[2] })
# 
# 
# 
# # kernal density plot ----------------------------------------------------
# d1 = density(int_pval_ff5, kernel="gaussian", adjust=1,
#              from=0, to=1)
# d2 = density(int_pval_gibs, kernel="gaussian", adjust=1,
#              from=0, to=1)
# rg = range(c(d1$y, d2$y + 0.2))
# plot(d1, ylim=rg,
#      xlab="P-value for the intercept test",
#      ylab="Distribution Density",
#      main=paste("Distribution of intercept test p-values"),
#      type="l", col="blue", lty=1,
#      lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
# lines(d2, type="l", col="red", lty=2, lwd=3)
# abline(v=0.05, col="green")
# legend(0.67, 1.3, c("FF5 model", 
#                    "AMF Model"),
#        lty=c(1, 2), col=c("blue", "red"), cex=1.5, lwd=3)
# 
# 
# 
# # Form the table with FDR rate ------------------------------------------
# p_ff5 = int_pval_ff5
# q_ff5 = p.adjust(p_ff5, method="BY")
# p_gibs = int_pval_gibs
# q_gibs = p.adjust(p_gibs, method="BY")
# # hist(int_pval_ff5, breaks=seq(0, 1, 0.05))
# # hist(int_pval_gibs, breaks=seq(0, 1, 0.05))
# 
# breaks = c(0, 0.05, 0.9, 1)
# p_ff5 = as.data.frame(table(cut(
#   p_ff5, breaks=breaks, 
#   labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
# q_ff5 = as.data.frame(table(cut(
#   q_ff5, breaks=breaks, 
#   labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
# p_gibs = as.data.frame(table(cut(
#   p_gibs, breaks=breaks,
#   labels=paste(breaks[-length(breaks)], "-",breaks[-1]))))
# q_gibs = as.data.frame(table(cut(
#   q_gibs, breaks=breaks, 
#   labels=paste(breaks[-length(breaks)], "-", breaks[-1]))))
# tab = do.call(cbind, lapply(list(p_ff5, p_gibs, q_ff5, q_gibs), 
#                             function(x) { x["Freq"] }))
# tab = do.call(cbind, lapply(tab, function(x) {
#   round(x / sum(x) * 100, 2) }))
# tab = cbind(p_ff5["Var1"], tab)
# colnames(tab) = c("Range", "FF5 p-val", "AMF p-val", 
#                   "FF5 FDR q-val", "AMF FDR q-val")
# tab
# library(xtable)
# print(xtable(tab), 
#       include.rownames=F, 
#       hline.after=seq(-1, nrow(tab), 1))
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ###########################################################################
# # In-sample F test for Goodness-of-Fit
# ###########################################################################
# 
# # Read in the results
# folder_name = "method_gibs_NA_20_cv1se_TRUE_FALSE"
# # This is GIBS + FF5
# file_name = "amf_fit_results"
# scalar_names = NULL
# vector_names = c("f_pval")
# var_names = c(scalar_names, vector_names)
# res = read_results(folder_name, file_name, scalar_names, vector_names)
# assign_batch(var_names, res[var_names])
# 
# # FDR on p values
# f_pval = unlist(f_pval)
# p_val = f_pval[is.finite(f_pval)]
# bh_val = p.adjust(p_val, method="BH")
# bhy_val = p.adjust(p_val, method="BY")
# 
# (n_same = sum(!is.finite(f_pval)))
# length(f_pval)
# n_same / length(f_pval)
# 1 - n_same / length(f_pval)
# 
# # p_val = c(p_val, rep(1, n_same))
# # bh_val = c(bh_val, rep(1, n_same))
# # bhy_val = c(bhy_val, rep(1, n_same))
# 
# tab = NULL
# rgs = list(c(0, 1e-2), c(0, 0.05), c(0.05, 1))
# for (rg in rgs) {
#   perc = sapply(list(p_val, bh_val, bhy_val), function(x){
#     sum((x >= rg[1]) & (x <= rg[2])) / length(x) * 100
#   })
#   tab = rbind(tab, perc)
# }
# tab = as.data.frame(tab)
# colnames(tab) = c("p-val", "bh-val", "bhy-val")
# rg_col = sapply(rgs, function(x) {
#   paste(x[1], " - ", x[2], sep="")
# })
# tab = cbind("value ranges"=rg_col, tab)
# rownames(tab) = NULL
# tab
# 
# library(xtable)
# print(xtable(tab), include.rownames=F, 
#       hline.after=seq(-1, nrow(tab), 1))






