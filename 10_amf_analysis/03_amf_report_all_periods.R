
###########################################################################
###########################################################################
###########################################################################
# 
#  Adaptive Multi-Factor (AMF) model results report for all periods
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()
source("../00_initial_code/01_func_help.R")
libs_local()
source("../04_gibs_algo/05_func_report.R")
par(mar=c(5, 6, 4, 3), lwd=1.5, cex.axis=1.2, cex.lab=1.2, cex.main=1.2)

# parent_folders = list.dirs(recursive=F)
# results_id = substr(parent_folders, start=1, stop=4) != "./00"
# parent_folders = parent_folders[results_id] 
# parent_folders = substring(parent_folders, first=3)
# parent_folders 

parent_folders = c("amf_results")
parent_folder = c("amf_results")






###########################################################################
# Out-of-sample goodness of fit:
# Mean Square Error (MSE) and Out-of-sample R squared (OSRS)
###########################################################################

# Calculate and save osrs.RData
for (parent_folder in parent_folders) {
  pt = proc.time()
  if (exists("osrs")) {
    rm(osrs)
  }

  osrs = tab_periods_compare(
    first_year=2007, last_year=2018, min_len=3,
    path_prefix=paste(parent_folder, "/results_", sep=""),
    result_name="results",
    var_path_gibs=c("pred", "osrs_gibs"),
    var_path_ff5=c("pred", "osrs_ff5"),
    value_min=-100, value_max=Inf, include_na=F,
    func=sum)
  osrs

  cat(parent_folder, "finished in", (proc.time() - pt)[3], "seconds.\n")
  save(osrs, file=paste(parent_folder, "/osrs.RData", sep=""))
}

for (i in 1:length(parent_folders)) {
  load(file=paste(parent_folders[i], "/osrs.RData", sep=""))
  cat("\nfolder:", parent_folders[i], "\n")
  print(round(osrs$tab_gibs, 3))
}

load(file=paste(parent_folder, "/osrs.RData", sep=""))
osrs



# Draw heatmaps -----------------------------------------------------------
breaks = seq(from=0, to=0.6, by=0.01)
colors = colorRampPalette(c('white','red'))(length(breaks))
legend_breaks = seq(0, 0.6, 0.2)
perc = F

mat = round(osrs$tab_gibs, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Out-of-Sample R^2 for AMF", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

mat = round(osrs$tab_ff5, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Out-of-Sample R^2 for FF5", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

range(osrs$tab_diff, na.rm=T)
breaks = seq(from=-0.4, to=0.4, by=0.01)
colors = colorRampPalette(c("blue", "white", "red"))(length(breaks))
legend_breaks = seq(-0.4, 0.4, 0.2)
perc = F

mat = round(osrs$tab_diff, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Difference of Out-of-Sample R^2 (AMF - FF5)", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))





###########################################################################
# Adjusted R squared
###########################################################################

# Calculate and save ars.RData
for (parent_folder in parent_folders) {
  pt = proc.time()
  if (exists("ars")) {
    rm(ars)
  }

  ars = tab_periods_compare(
    first_year=2007, last_year=2018, min_len=3,
    path_prefix=paste(parent_folder, "/results_", sep=""),
    result_name="results",
    var_path_gibs=c("fit", "ars_gibs"),
    var_path_ff5=c("fit", "ars_ff5"),
    value_min=-100, value_max=Inf, include_na=F,
    func=sum)
  ars

  cat(parent_folder, "finished in", (proc.time() - pt)[3], "seconds.\n")
  save(ars, file=paste(parent_folder, "/ars.RData", sep=""))
}

for (i in 1:length(parent_folders)) {
  load(file=paste(parent_folders[i], "/ars.RData", sep=""))
  cat("\nfolder:", parent_folders[i], "\n")
  print(round(ars$tab_gibs, 3))
}

load(file=paste(parent_folder, "/ars.RData", sep=""))
ars



# Draw heatmaps -----------------------------------------------------------
breaks = seq(from=0, to=0.5, by=0.01)
colors = colorRampPalette(c('white','red'))(length(breaks))
legend_breaks = seq(0, 0.5, 0.2)
perc = F

mat = round(ars$tab_gibs, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Adjusted R^2 for AMF", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

mat = round(ars$tab_ff5, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Adjusted R^2 for FF5", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

range(ars$tab_diff, na.rm=T)
breaks = seq(from=-0.2, to=0.2, by=0.01)
colors = colorRampPalette(c("blue", "white", "red"))(length(breaks))
legend_breaks = seq(-0.2, 0.2, 0.1)
perc = F

mat = round(ars$tab_diff, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Difference of Adjusted R^2 (AMF - FF5)", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))






###########################################################################
# Intercept Test
###########################################################################

# Calculate and save int_pval.RData
for (parent_folder in parent_folders) {
  pt = proc.time()
  if (exists("int_pval")) {
    rm(int_pval)
  }

  int_pval = tab_periods_compare(
    first_year=2007, last_year=2018, min_len=3,
    path_prefix=paste(parent_folder, "/results_", sep=""),
    result_name="results",
    var_path_gibs=c("int_gibs", "int", "Pr(>|t|)"),
    var_path_ff5=c("int_ff5", "int", "Pr(>|t|)"),
    value_min=-Inf, value_max=Inf, include_na=T,
    func=get_signif, q_th=0.05)
  int_pval

  cat(parent_folder, "finished in", (proc.time() - pt)[3], "seconds.\n")
  save(int_pval, file=paste(parent_folder, "/int_pval.RData", sep=""))
}

for (i in 1:length(parent_folders)) {
  load(file=paste(parent_folders[i], "/int_pval.RData", sep=""))
  cat("\nfolder:", parent_folders[i], "\n")
  print(round(int_pval$tab_gibs, 3))
}

load(file=paste(parent_folder, "/int_pval.RData", sep=""))
int_pval



# Draw heatmaps -----------------------------------------------------------
breaks = seq(from=0, to=0.8, by=0.01)
colors = colorRampPalette(c('white','red'))(length(breaks))
legend_breaks = seq(0, 0.8, 0.2)
perc = T

mat = round(int_pval$tab_gibs, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Percent of stocks with non-0 intercepts in AMF", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

mat = round(int_pval$tab_ff5, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Percent of stocks with non-0 intercepts in FF5", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))

range(int_pval$tab_diff, na.rm=T)
breaks = seq(from=-0.4, to=0.4, by=0.01)
colors = colorRampPalette(c("blue", "white", "red"))(length(breaks))
legend_breaks = seq(-0.4, 0.4, 0.2)
perc = T

mat = round(int_pval$tab_diff, 2)
draw_heatmap(mat=mat, color=colors, breaks=breaks, 
             main="Difference of the percentage (AMF - FF5)", 
             display_numbers=get_label(tab=mat, perc=perc), 
             legend_breaks=legend_breaks, 
             legend_labels=get_label(tab=legend_breaks, perc=perc))


###########################################################################
# The end
###########################################################################
