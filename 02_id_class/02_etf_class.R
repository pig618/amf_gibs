
###########################################################################
# Build category list for all ETFs in securities data.
#
# 
# 
# Form a detailed ETF list, a list of all etf descriptions appearing 
# in both EtfList.txt and data from CRSP, including the permno, ticker,
# NAME and Class information.
# 
# 
# 
# Input files:
# "../01_all_horizons/id_info.RData"
# "../00_data/02_etf_id_data/etf_class.csv"
# 
# 
# 
# Output files:
# "etf_info.RData"
# 
# 
# 
# Notes: 
# The data frame with all ETFs listed are in etf_info$etf_class.  
# The other element in etf_info is etf_info$class_table, giving the 
#   matchings between big_class and category etc. 
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



# Read in id_info and etf_class -------------------------------------------
etf = fread(input="../00_data/03_etf_id_data/etf_class.csv", 
            data.table=F)
# names(etf)
col_names = c("Symbol", "Name", "Asset Class", "ETFdb Category", 
              "Issuer", "Inception", "Assets", "Standard Deviation")
etf = subset(etf, select=col_names)
names(etf) = c("ticker", "etf_name", "asset_class", "category", 
               "issuer", "inception", "assets", "std")
dim(etf); etf = unique(etf); dim(etf)    # 2257    8
tic = table(etf$ticker)
tic[tic > 1]     # No duplicated tickers.
rm(tic)

load(file="../01_all_horizons/id_info.RData")
dim(id_info)     # 29208    14

head(etf)
head(id_info)



# Remove records in id_info with non-73 share codes -----------------------
# since any ETF should have share code 73 based on the CRSP documents
id_info = subset(id_info, shrcd == 73)



# Remove ETNs by keywords -------------------------------------------------
keywords = c("ETN", "E T N", "Exchange Traded Notes", "EXCHANGE TRADED NOTES")

keyword_match = function(keyword, name_vector) {
  sapply(name_vector, function(x) { grepl(keyword, x)})
}

match_list = lapply(keywords, keyword_match, etf$etf_name)
etn_id = Reduce("|", match_list)
sum(etn_id)
length(etn_id)
# 152 ETNs out of 2257 records
dim(etf)
etf = etf[!etn_id, ]
dim(etf)

match_list = lapply(keywords, keyword_match, id_info$comnam)
etn_id = Reduce("|", match_list)
sum(etn_id)
length(etn_id)
dim(id_info)
id_info = id_info[!etn_id, ]
dim(id_info)



# Match ticker in id_info and etf -----------------------------------------
tic_match = unique(intersect(id_info$ticker, etf$ticker))
length(tic_match)     # 2035

id = subset(id_info, ticker %in% tic_match)
# select = c("permno", "ticker", "comnam")

etf = subset(etf, ticker %in% tic_match)

length(etf$ticker)             # 2035
length(unique(etf$ticker))     # 2035
# No replicated tickers in etf. So the "etf" is already clean.

length(id$ticker)              # 2134
length(unique(id$ticker))      # 2035
# There are replicated tickers in id. Clean "id" as the following.



# Remove the bad rows in id -----------------------------------------------
tic_tab = table(id$ticker) 
good_tic = names(tic_tab[tic_tab == 1])
bad_tic = names(tic_tab[tic_tab > 1])
data_clean = subset(id, ticker %in% good_tic)
data_dirty = subset(id, ticker %in% bad_tic)



# merge data from id with etf
dim(data_clean)
data_clean = merge(data_clean, etf, by="ticker")
dim(data_clean)

dim(data_dirty)
data_dirty = merge(data_dirty, etf, by="ticker")
dim(data_dirty)
# In the following, add correct rows from data_dirty to data_clean and 
#   remove unwanted rows in data_dirty, until data_dirty are empty.



# If these key words appear in only one of the repeated ticker rows, 
#   then just select that row.
clean_by_keywords = function(keywords, data_clean, data_dirty) {
  
  keywords_appear = function(comp_names) {
    # Return TRUE if any of the keywords appear in comp_names 
    any(sapply(keywords, function(kw) { grepl(kw, comp_names) }))
  }
  
  block_by_keywords = function(block) {
    # Clean a block with one ticker by the keywords
    appear = sapply(block$comnam, keywords_appear)
    if (sum(appear) == 1) {
      block = block[appear, ]
    }
    block
  }
  
  data_list = split(data_dirty, data_dirty$ticker)
  data_list = lapply(data_list, block_by_keywords)
  data = do.call(rbind, data_list)
  
  # Add good rows to data_clean, undecided rows to data_dirty
  tic_tab = table(data$ticker) 
  good_tic = names(tic_tab[tic_tab == 1])
  bad_tic = names(tic_tab[tic_tab > 1])
  
  data_clean = rbind(data_clean, subset(data, ticker %in% good_tic))
  cat("good :", length(good_tic), dim(data_clean)[1], "\n")
  
  data_dirty = subset(data, ticker %in% bad_tic)
  data_dirty = data_dirty[order(data_dirty$ticker), ]
  cat("bad : ", length(bad_tic), dim(data_dirty)[1], "\n\n")
  
  return(list(data_clean=data_clean, data_dirty=data_dirty))
}

keywords = c("ETF", "E T F", "EXCHANGE TRADED FUND",
             "EXCHANGE TRADED FUNDS")
results = clean_by_keywords(keywords, data_clean, data_dirty)
data_clean = results$data_clean
data_dirty = results$data_dirty

unique(data_dirty$issuer)
keywords = toupper(unique(data_dirty$issuer))
results = clean_by_keywords(keywords, data_clean, data_dirty)
data_clean = results$data_clean
data_dirty = results$data_dirty

# Write to CSV file and remove the bad ones manually
write.csv(data_dirty, file="data_dirty.csv", row.names=F)
# Find out the correct permno matching the ticker
#   by finding the correct cusip for a ticker by searching online.

# Then copy the csv file and remove bad rows manually 
#   in Excel, save it in "data_corrected.csv".
data_corrected = fread(file="data_corrected.csv", 
                       data.table=F) 
data_clean = rbind(data_clean, 
                   subset(data_dirty, 
                          permno %in% data_corrected$permno))
dim(data_clean)
check_bad_col(data_clean)
id = data_clean



# Add columns to the cleaned data ----------------------------------------
# Subset id_info with rows in id
id_info = subset(id_info, permno %in% id$permno)



# Merge id_info with etf
dim(id_info)
dim(etf)
tic_match = intersect(id_info$ticker, etf$ticker)
length(tic_match)
etf_class = merge(subset(id_info, ticker %in% tic_match), 
                  subset(etf, ticker %in% tic_match), 
                  by="ticker")
dim(etf_class)
length(unique(etf_class$ticker))
length(unique(etf_class$permno))
# No replications in permno or ticker




# Add columns big_class, big_class_no, category_no -----------------------

# asset_list = split(etf_class, etf_class$asset_class)
# length(asset_list)
# for (i in 1:9) {
#   cat(i, '-----------------------------------', '\n')
#   cat(asset_list[[i]]$asset_class[1], '  ', 
#       nrow(asset_list[[i]]), '\n')
#   print(sapply(split(asset_list[[i]], asset_list[[i]]$category),
#                function(x) { nrow(x) }))
#   cat('\n')
# }
# # One category may appear in different asset_classes. 
# # So asset_class can not be used as primary classification criteria.

cate_names = c("California Munis", "Corporate Bonds", 
               "Emerging Markets Bonds", "Government Bonds", 
               "High Yield Bonds", "Inflation-Protected Bonds", 
               "International Government Bonds", "Money Market", 
               "Mortgage Backed Securities", "National Munis", 
               "New York Munis", "Preferred Stock/Convertible Bonds", 
               "Total Bond Market",
               
               "Agricultural Commodities", "Commodities", "Metals", 
               "Oil & Gas", "Precious Metals", 
               
               "Currency", 
               
               "Diversified Portfolio", "Target Retirement Date", 
               
               "All Cap Equities", "Alternative Energy Equities", 
               "Asia Pacific Equities", "Building & Construction", 
               "China Equities", "Commodity Producers Equities", 
               "Communications Equities", 
               "Consumer Discretionary Equities", 
               "Consumer Staples Equities", 
               "Emerging Markets Equities", "Energy Equities", 
               "Europe Equities", "Financials Equities", 
               "Foreign Large Cap Equities", 
               "Foreign Small & Mid Cap Equities", 
               "Global Equities","Health & Biotech Equities", 
               "Industrials Equities", "Japan Equities", 
               "Large Cap Blend Equities", 
               "Large Cap Growth Equities", 
               "Large Cap Value Equities", "Latin America Equities", 
               "MLPs", "Materials", "Mid Cap Blend Equities", 
               "Mid Cap Growth Equities", "Mid Cap Value Equities", 
               "Small Cap Blend Equities", "Small Cap Growth Equities", 
               "Small Cap Value Equities", "Technology Equities", 
               "Transportation Equities", "Utilities Equities", 
               "Volatility Hedged Equity", "Water Equities", 
               
               "Hedge Fund", "Long-Short",
               
               "Inverse Bonds", "Inverse Commodities", 
               "Inverse Equities", "Inverse Volatility", 
               
               "Leveraged Bonds", "Leveraged Commodities", 
               "Leveraged Currency", "Leveraged Equities", 
               "Leveraged Multi-Asset", "Leveraged Real Estate", 
               "Leveraged Volatility", 
               
               "Global Real Estate", "Real Estate", 
               
               "Volatility")       # length is 73

first_cate = c("California Munis", "Agricultural Commodities", 
               "Currency", "Diversified Portfolio", "All Cap Equities", 
               "Hedge Fund", "Inverse Bonds", "Leveraged Bonds",  
               "Global Real Estate", "Volatility")     # length is 10

big_class_names = c("Bond/Fixed Income", "Commodity", "Currency", 
                    "Diversified Portfolio", "Equity", 
                    "Alternative ETFs", "Inverse", "Leveraged", 
                    "Real Estate", "Volatility")    # length is 10

big_class_no = cumsum(cate_names %in% first_cate)

match = data.frame(category_no=1:length(cate_names), 
                   category=cate_names, 
                   big_class_no=big_class_no,
                   big_class=big_class_names[big_class_no])
match$category = as.character(match$category)
match$category_no = as.integer(match$category_no)
match$big_class = as.character(match$big_class)
match$big_class_no = as.integer(match$big_class_no)
match = match[order(match$category_no), ]

check_bad_col(match)
head(match)


setdiff(etf_class$category, cate_names)
etf_class = subset(etf_class, category != "n/a")
dim(etf_class)
etf_class = merge(etf_class, match, by="category", sort=F)
dim(etf_class)

col_names = c("permno", "ticker", "etf_name", "comnam",  
              "category_no", "category", 
              "big_class_no", "big_class",
              "issuer", "asset_class", 
              "hsiccd", "siccd", "naics", "exchcd", "hexcd", 
              "shrcd", "shrcls", "cusip", "ncusip", "permco", 
              "nwperm", "inception", "std", "assets")
etf_class = etf_class[col_names]

check_bad_col(etf_class)
head(etf_class)



# Save the results -------------------------------------------------------
etf_info = list(etf_class=etf_class, 
                class_table=match)
save(etf_info, file="etf_info.RData")



# Results presentation ---------------------------------------------------
source("01_func_class_presentation.R")
class_presentation(etf_info$etf_class$permno, 
                   etf_info$etf_class, 
                   etf_info$class_table)



#-- The End -------------------------------------------------------------------
