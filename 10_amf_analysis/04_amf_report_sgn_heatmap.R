
###########################################################################
###########################################################################
###########################################################################
# 
#  Creat the significance table based on the constant beta test results
#  Draw the heatmap
# 
###########################################################################
###########################################################################
###########################################################################

rm(list=ls()); gc()

source("../00_initial_code/01_func_help.R")
libs_local()
source("../04_gibs_algo/05_func_report.R")

par(mar=c(5, 6, 4, 4), lwd=2, cex.axis=1.5, cex.lab=1.5, cex.main=1.5)

start = 2014 
end = 2016



to_perc = function(x){
  # Function to change the matrix into percentage
  # Each column add up to 100%
  tab_perc = round(100 * data.matrix(x) %*% diag(1 / apply(x, 2, sum)), 2)
  colnames(tab_perc) = colnames(x)
  rownames(tab_perc) = rownames(x)
  tab_perc
}






###########################################################################
# Load the company and ETF classes 
###########################################################################
load("../02_id_class/etf_info.RData")
load("../02_id_class/stocks_info.RData")

fac_cate_no = etf_info$etf_class$category_no
names(fac_cate_no) = paste("per", etf_info$etf_class$permno, sep="")
ff5_cate_no = range(fac_cate_no)[2] + 1:6
names(ff5_cate_no) = c("cma", "rmw", "hml", "smb", "mkt", "rf_int")
fac_cate_no = c(fac_cate_no, ff5_cate_no)

sto_cate_no = stocks_info$stocks_class$category_no
names(sto_cate_no) = paste("per", stocks_info$stocks_class$permno, 
                           sep="")

nrow = nrow(etf_info$class_table) + 6
ncol = nrow(stocks_info$class_table)

sgn_tab = as.data.frame(matrix(rep(0, nrow * ncol), ncol=ncol))
rownames(sgn_tab) = c(etf_info$class_table$category, names(ff5_cate_no))
colnames(sgn_tab) = paste(
  "sic_", sprintf("%02d", stocks_info$class_table$category_no), sep="")






###########################################################################
# Form the significance table 
###########################################################################

# Read in the results
folder_name = "reports"
if (!dir.exists(folder_name)) {
  dir.create(folder_name)
}
load(file=paste(folder_name, "/reports.RData", sep=""))

fac_sgn = reports$fit$fac_sgn_gibs

length(unique(unlist(fac_sgn)))
# all factors are selected by at least one stock in GIBS

permno = names(reports$fit$fac_sgn_gibs)

# folder_name = "amf_results"
# file_name = "amf_fit_results"
# scalar_names = c("permno")
# vector_names = c("fac_sgn")
# var_names = c(scalar_names, vector_names)
# res = read_results(folder_name, file_name, scalar_names, vector_names)
# assign_batch(var_names, res[var_names])



# Form the column of significance count for each company
count_sgn = function(i) {
  # if (is.na(sto_cate_no[permno[[i]]])) {
  #   return(NULL)
  # }
  count = table(fac_cate_no[fac_sgn[[i]]])
  col_to_add = rep(0, nrow)
  col_to_add[as.integer(names(count))] = count
  col_to_add = as.data.frame(col_to_add)
  names(col_to_add) = paste(
    "sic_",  sprintf("%02d", sto_cate_no[permno[[i]]]), sep="")
  col_to_add
}
cols_to_add = lapply(1:length(permno), count_sgn)



# Add count columns to sgn_tab
for (col in cols_to_add) {
  # if (is.null(col)) {
  #   next
  # }
  sgn_tab[names(col)] = sgn_tab[names(col)] + col
}



# Change rownames to be more readable
rownames(sgn_tab)[which(rownames(sgn_tab) == "rf_int")] = "risk-free rate"
rownames(sgn_tab)[which(rownames(sgn_tab) == "mkt")] = "market return"
tail(sgn_tab)


# Write to csv
if (!dir.exists("sgn_table_record")) {
  dir.create("sgn_table_record")
}
tab_perc = to_perc(sgn_tab)
write.csv(sgn_tab, row.names=T, 
          file=paste(folder_name, "/sgn_count_table_original.csv", sep=""))
write.csv(tab_perc, row.names=T,
          file=paste(folder_name, "/sgn_perc_table_original.csv", sep=""))






###########################################################################
# Sgnificance table cleaning 
###########################################################################

# Note that every cleaning is done on tab, not tab_perc!
ff5_names = c("market return", "smb", "hml", "rmw", "cma")
tab = sgn_tab

# Remove columns with too less (<= 1) significant factors
good_cols = colnames(tab)[apply(data.matrix(tab), 2, sum) > 1]
setdiff(colnames(tab), good_cols)
tab = sgn_tab[, good_cols, drop=F]
dim(tab)

# Remove rows hardly picked as significant
tab = tab[apply(tab, 1, sum) > 1, , drop=F]
dim(tab)

# Remove rows not very significant in percentage
tab_perc = to_perc(tab)
good_rows = rownames(tab_perc)[apply(data.matrix(tab_perc), 1, max) > 10]
good_rows = union(ff5_names, good_rows)
tab = tab[good_rows, , drop=F]
dim(tab)

# # Heatmap
# tab_perc = to_perc(tab)
# heatmap(main="Table of percentage of significance count",
#         data.matrix(tab_perc), 
#         Colv=NA, Rowv=NA, revC=F, scale="none",
#         col=paste("gray", 100:1, sep=""),
#         margins=c(10, 12))



# Combine some columns together
# Note that here we go back to orginal tab first. Then to percentage.
combine_cols = function(x, sic_no) {
  id = which(colnames(x) %in% paste("sic_", sep="",
                                    sprintf("%02d", sic_no)))
  
  if (length(id) <= 0) {
    return(x)
  }
  
  x[, id[1]] = apply(data.matrix(x[, id]), 1, sum)
  x[, id[-1]] = NULL
  colnames(x)[id[1]] = paste("sic_", sep="",
                             paste(sprintf("%02d", sic_no), 
                                   collapse="_", sep=""))
  x
}

tab = combine_cols(tab, 25:28)

tab = combine_cols(tab, 29:31)
tab = combine_cols(tab, 33:38)
# tab = combine_cols(tab, 44:46)
tab = combine_cols(tab, 47:51)
# tab = combine_cols(tab, 60:63)
tab = combine_cols(tab, 70:73)  # important

# Heatmap
tab_perc = to_perc(tab)

par(mar=c(5 ,10, 10, 3), lwd=2, 
    cex.axis=1, cex.lab=1, cex.main=1)


library(RColorBrewer)
heatmap(main="Heatmap of the percentage of the significance count",
        data.matrix(tab_perc), 
        Colv=NA, Rowv=NA, revC=F, scale="none",
        col=colorRampPalette(brewer.pal(9, "Blues"))(100),
        margins=c(10, 6))

# heatmap(main="Heatmap of the percentage of the significance count",
#         data.matrix(tab_perc), 
#         Colv=NA, Rowv=NA, revC=F, scale="none",
#         col=paste("grey", 100:1, sep=""),
#         margins=c(10, 12))



write.csv(tab, row.names=T,
          file=paste(folder_name, "/sgn_count_table_clean.csv", sep=""))
write.csv(tab_perc, row.names=T, 
          file=paste(folder_name, "/sgn_perc_table_clean.csv", sep=""))






###########################################################################
# latex code
###########################################################################
if (!require(xtable)) {
  install.packages("xtable")
}
library(xtable)

dim(tab_perc)
# 27 53

i = which(rownames(tab_perc) == "Preferred Stock/Convertible Bonds")
rownames(tab_perc)[i] = "Pref. Stock/Convrtbl. Bonds"
i = which(rownames(tab_perc) == "Consumer Discretionary Equities")
rownames(tab_perc)[i] = "Consumer Discrtnry. Equities"

ncol(tab_perc)
tab1 = round(tab_perc[, 1:18], 0)
colnames(tab1) = substr(colnames(tab1), start=5, stop=6)
tab1
print(xtable(tab1, digits=0),
      type="latex", tabular.environment="tabular", 
      hline.after=seq(-1, nrow(tab1), 1))
colnames(tab_perc)

ncol(tab_perc)
tab1 = round(tab_perc[, 19:36], 0)
colnames(tab1) = substr(colnames(tab1), start=5, stop=6)
tab1
print(xtable(tab1, digits=0),
      type="latex", tabular.environment="tabular", 
      hline.after=seq(-1, nrow(tab1), 1))
colnames(tab_perc)

ncol(tab_perc)
tab1 = round(tab_perc[, 37:53], 0)
colnames(tab1) = substr(colnames(tab1), start=5, stop=6)
tab1
print(xtable(tab1, digits=0),
      type="latex", tabular.environment="tabular", 
      hline.after=seq(-1, nrow(tab1), 1))
colnames(tab_perc)
# Correct the column names for the merged column.






###########################################################################
# The end 
###########################################################################
