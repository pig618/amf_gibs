
###########################################################################
###########################################################################
###########################################################################
# 
#  Method (Model) Compare Results Report
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()
source("../00_initial_code/01_func_help.R")
libs_local()
source("../04_gibs_algo/05_func_report.R")
par(mar=c(5, 6, 4, 3), lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)

start = 2014 
end = 2016






###########################################################################
# Method Compare summary table and basis assets selected count
###########################################################################

# Find model names
# model_names = list.files(path="./method_comparison")
# cat(model_names, sep="\", \"")
model_names = c(
  "gibs_ff5", "enet1", "enet0.75", "enet0.5", "enet0.25", "enet0")

# Form the table and count
summ_tab = NULL
select_counts = list()
for (model_name in model_names) {
  res = result_read(
    path_name=paste("./method_comparison/", model_name, sep=""), 
    result_name="results", var_names=NULL)
  res[["per91560"]] = NULL  # its orsr_ff5 is -353008
  res = result_reform(res)
  
  res$fit = result_reform(
    result=res$fit, var_names=c(
      "select_gibs", "fac_sgn_gibs", 
      "rs_gibs", "ars_gibs"))
  res$fit = c(result_to_vec(
    result=res$fit, var_names=c("rs_gibs", "ars_gibs")), 
    res$fit["select_gibs"], 
    res$fit["fac_sgn_gibs"])
  
  if (model_name == "gibs_ff5") {
    res$fit$n_select = sapply(
      res$fit$select_gibs, function(z) {length(z$x)} )
    res$fit$n_sgn = sapply(
      res$fit$fac_sgn_gibs, function(z) {length(z)})
  } else {
    res$fit$n_select = sapply(
      res$fit$select_gibs, function(z) {length(z)} )
    res$fit$n_sgn = NA
  }
  
  select_counts[[model_name]] = res$fit$n_select
  
  res$pred = result_reform(result=res$pred, var_names="osrs_gibs")
  res$pred = result_to_vec(result=res$pred, var_names="osrs_gibs")
  
  cur_tab = list(n_select=res$fit$n_select, 
                 n_sgn=res$fit$n_sgn,
                 rs=res$fit$rs_gibs, 
                 ars=res$fit$ars_gibs, 
                 osrs=res$pred$osrs_gibs)
  cur_tab = data.frame(lapply(
    cur_tab, function(z) { mean(z, na.rm=T) }))
  rownames(cur_tab) = model_name
  summ_tab = rbind(summ_tab, cur_tab)
}
# summ_tab[, c("n_select", "n_sgn", "rs", "ars", "osrs")]
summ_tab

# Add FF5 and GIBS
load("reports/reports.RData")
res = reports

# GIBS
cur_tab = list(n_select=res$fit$n_select, 
               n_sgn=sapply(res$fit$fac_sgn_gibs, length),
               rs=res$fit$rs_gibs, 
               ars=res$fit$ars_gibs, 
               osrs=res$pred$osrs_gibs)
cur_tab = data.frame(lapply(
  cur_tab, function(z) { mean(z, na.rm=T) }))
rownames(cur_tab) = "GIBS"
summ_tab = rbind(cur_tab, summ_tab)
select_counts[["gibs"]] = res$fit$n_select

# FF5
cur_tab = list(n_select=5, 
               n_sgn=sapply(res$fit$fac_sgn_ff5, length),
               rs=res$fit$rs_ff5, 
               ars=res$fit$ars_ff5, 
               osrs=res$pred$osrs_ff5)
cur_tab = data.frame(lapply(
  cur_tab, function(z) { mean(z, na.rm=T) }))
rownames(cur_tab) = "FF5"
summ_tab = rbind(cur_tab, summ_tab)


# Summary table -----------------------------------------------------------
rownames(summ_tab) = c("FF5", "GIBS", "GIBS + FF5", "LASSO", 
                       "E-Net (a=0.75)", "E-Net (a=0.50)",
                       "E-Net (a=0.25)", "Ridge")
summ_tab$osrs_change = 
  (summ_tab$osrs / summ_tab$osrs[1] - 1) * 100
summ_tab$ars_change = 
  (summ_tab$ars / summ_tab$ars[1] - 1) * 100
cols = c("n_select", "n_sgn", "ars", "ars_change", "osrs", "osrs_change")
summ_tab = summ_tab[, cols]
summ_tab

# Print the table
library(xtable)
print(xtable(summ_tab, digits=c(0, 1, 1, 3, 0, 3, 0)), 
      hline.after=1:nrow(summ_tab), include.rownames=T, floating=F)
# add.to.row = list(pos=list(0), command=NULL) 
# command = paste0("\\hline\n\\endhead\n", 
#                  "\\hline\n", 
#                  "\\multicolumn{", dim(etf_tab)[2] + 1, "}{l}", 
#                  "{\\footnotesize Continued on next page}\n", 
#                  "\\endfoot\n", "\\endlastfoot\n") 
# add.to.row$command = command 
# add.to.row=add.to.row, 
# tabular.environment="longtable",



# Selected factor cound density plot --------------------------------------
select_counts = select_counts[c("gibs", model_names)]

d = list()
for (i in 1:length(select_counts)) {
  d[[i]] = density(select_counts[[i]], kernel="gaussian", adjust=5, from=0)
}

# Draw the plot
y_rg = c(0, 0.4)
x_rg = c(0, 40)
ltys = 1:length(select_counts)
colors = c("black", "blue", "red", "green", "orange", "purple", "yellow")
plot(NULL, ylim=y_rg, xlim=x_rg,
     xlab="Number of basis assets selected", 
     ylab="Density",
     main="Distribution of number of basis assets selected", 
     type="l", lwd=3, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
for (i in 1:length(select_counts)){
  lines(d[[i]], type="l", col=colors[i], lty=ltys[i], lwd=3)
}
abline(h=0, col="gray")

legend(20, 0.4, rownames(summ_tab)[-1], 
       lty=ltys, col=colors, cex=1.5, lwd=3)
# First drag the plot to have larger height, 
# rerun, then drag the plot to have less height.






###########################################################################
# The end 
###########################################################################
