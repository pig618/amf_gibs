
###########################################################################
# Form identifier information and save   
# 
# 
# 
# Extract identifier information from the monthly security file and save
# them in "id_info.RData" for further use. Use the last available 
# identifier for each security. The primary id for a stock is its permno.
# 
# 
# 
# Input files:
# "securities_monthly_clean.RData"
# 
# 
# 
# Output files:
# "id_info.RData"
# 
# 
# 
# Notes: 
# If the last value of a column is NA, but there are some non-NA values
#   before, then we replace the NA by that value.
###########################################################################

rm(list=ls()); gc()
source("../00_initial_code/01_func_help.R")




# Read in the securites monthly cleaned data ------------------------------
pt = proc.time()
load(file="securities_monthly_clean.RData")
secu = securities_monthly_clean
proc.time()     # 200 seconds



# Form identifier information and save ------------------------------------
id_names = c("permno", "ticker", "comnam", "hsiccd", "siccd", "naics", 
             "exchcd", "hexcd", "shrcd", "shrcls", "cusip", "ncusip",  
             "permco", "nwperm")

last_row = function(x) {
  last = tail(x, n=1) 
  bad_col = which(is.na(as.vector(last)))
  for (i in bad_col){
    replace = tail(x[, i][!is.na(x[, i])], n=1)
    if (length(replace) != 0) {
      last[, i] = replace
    }
  }
  last
}

pt = proc.time()
ncores = detectCores() - 1
sfInit(parallel=TRUE, cpus=ncores)

id_info = secu[id_names]
id_list = split(id_info, secu$permno)
id_list = sfClusterApplyLB(id_list, last_row)
id_info = do.call(rbind, id_list)

sfStop()
proc.time() - pt      #  66 seconds



# Remove duplicated rows if there are any 
dim(id_info)
id_info = unique(id_info)
dim(id_info)
# No replicated rows 



# check id_info
length(unique(secu$permno))     # 30088
dim(id_info)                    # 30088    14
head(id_info)
check_bad_col(id_info)



# save the data
pt = proc.time()
save(id_info, file="id_info.RData")
proc.time() - pt      #  1 second.



# The end -----------------------------------------------------------------