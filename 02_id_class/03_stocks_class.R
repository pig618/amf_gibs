
###########################################################################
# Build category columns for all stocks in securities data.
# 
# 
# 
# Separate stocks from ETFs in securities data. Class the stocks by 
# their Header SIC code, which is approximately the last effective
# Standard Industrial Classification code. 
# 
# 
# 
# Input:
# "../01_all_horizons/id_info.RData"
# "etf_info.RData"
# 
# 
# 
# Output:
# "stocks_info.RData"
# 
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



# Load identifier data ---------------------------------------------------
load("../01_all_horizons/id_info.RData")
load("etf_info.RData")
sto = subset(id_info, !(permno %in% etf_info$etf_class$permno))
# rm(etf_info)
gc()

check_bad_col(sto)
length(unique(sto$permno))
nrow(sto)
# No replicate permno, so each permno corresponds to one hsiccd and 
#   one siccd. Here we mainly use hsiccd for more accuracy.



# Deal with NAs in hsiccd, siccd, naics ----------------------------------
hsic = as.integer(sto$hsiccd)
sic = as.integer(sto$siccd)
naics = as.integer(sto$naics)

na_hsic = which(is.na(hsic) | hsic < 100 | hsic > 9999 |
                   (hsic >= 1800 & hsic <= 1999))
na_sic = which(is.na(sic) | sic < 100 | sic > 9999 |
                 (sic >= 1800 & sic <= 1999))
na_naics = which(is.na(naics))

length(na_hsic)
length(na_sic)
length(na_naics)
setdiff(na_hsic, na_sic)
# So using hsiccd as the primary classification code is the best choice

# Change to integer and replace NAs by 0 in hsiccd column 
sto[c("hsiccd", "siccd", "naics")] = 
  lapply(sto[c("hsiccd", "siccd", "naics")], as.integer)
sto$hsiccd[na_hsic] = 0
sto$siccd[na_sic] = 0 
sto$naics[na_naics] = 0 
check_bad_col(sto)


# Create match table of category, category_no, big_class, big_class_no ---
category = 
  c("Not Available", 
    "Agricultural Production Crops",
    "Agriculture production livestock and animal specialties",
    "Agricultural Services",
    "Forestry",
    "Fishing, hunting, and trapping",
    "Metal Mining",
    "Coal Mining",
    "Oil And Gas Extraction",
    "Mining And Quarrying Of Nonmetallic Minerals, Except Fuels",
    "Building Construction General Contractors And Operative Builders",
    "Heavy Construction Other Than Building Construction Contractors",
    "Construction Special Trade Contractors",
    "Food And Kindred Products",
    "Tobacco Products",
    "Textile Mill Productsv",
    "Apparel And Other Finished Products Made From Fabrics And Similar Materials",
    "Lumber And Wood Products, Except Furniture",
    "Furniture And Fixtures",
    "Paper And Allied Products",
    "Printing, Publishing, And Allied Industries",
    "Chemicals And Allied Products",
    "Petroleum Refining And Related Industries",
    "Rubber And Miscellaneous Plastics Products",
    "Leather And Leather Products",
    "Stone, Clay, Glass, And Concrete Products",
    "Primary Metal Industries",
    "Fabricated Metal Products, Except Machinery And Transportation Equipment",
    "Industrial And Commercial Machinery And Computer Equipment",
    "Electronic And Other Electrical Equipment And Components, Except Computer Equipment",
    "Transportation Equipment",
    "Measuring, Analyzing, And Controlling Instruments; Photographic, Medical And Optical Goods; Watches And Clocksv",
    "Miscellaneous Manufacturing Industries",
    "Railroad Transportation",
    "Local And Suburban Transit And Interurban Highway Passenger Transportation",
    "Motor Freight Transportation And Warehousing",
    "United States Postal Service",
    "Water Transportation",
    "Transportation By Air",
    "Pipelines, Except Natural Gas",
    "Transportation Services",
    "Communications",
    "Electric, Gas, And Sanitary Services",
    "Wholesale Trade-durable Goods",
    "Wholesale Trade-non-durable Goods",
    "Building Materials, Hardware, Garden Supply, And Mobile Home Dealers",
    "General Merchandise Stores",
    "Food Stores",
    "Automotive Dealers And Gasoline Service Stations",
    "Apparel And Accessory Stores",
    "Home Furniture, Furnishings, And Equipment Stores",
    "Eating And Drinking Places",
    "Miscellaneous Retail",
    "Depository Institutions",
    "Non-depository Credit Institutionsv",
    "Security And Commodity Brokers, Dealers, Exchanges, And Services",
    "Insurance Carriers",
    "Insurance Agents, Brokers, And Service",
    "Real Estate",
    "Holding And Other Investment Offices",
    "Hotels, Rooming Houses, Camps, And Other Lodging Places",
    "Personal Services",
    "Business Services",
    "Automotive Repair, Services, And Parking",
    "Miscellaneous Repair Services",
    "Motion Pictures",
    "Amusement And Recreation Services",
    "Health Services",
    "Legal Services",
    "Educational Services",
    "Social Services",
    "Museums, Art Galleries, And Botanical And Zoological Gardens",
    "Membership Organizations",
    "Engineering, Accounting, Research, Management, And Related Services",
    "Private Households",
    "Miscellaneous Services",
    "Executive, Legislative, And General Government, Except Finance",
    "Justice, Public Order, And Safety",
    "Public Finance, Taxation, And Monetary Policy",
    "Administration Of Human Resource Programs",
    "Administration Of Environmental Quality And Housing Programs",
    "Administration Of Economic Programs",
    "National Security And International Affairs",
    "Nonclassifiable Establishments")
category_no = c(0, 1, 2, 7:9, 10, 12:14, 15:17, 20:65, 67, 70, 72:73, 75:76, 
                78:84, 86:89, 91:97, 99)

big_class = c("Not Available", 
              "Agriculture, Forestry, And Fishing", 
              "Mining", 
              "Construction", 
              "Manufacturing", 
              "Transportation, Communications, Electric, Gas, And Sanitary Services", 
              "Wholesale Trade", 
              "Retail Trade", 
              "Finance, Insurance, And Real Estate", 
              "Services", 
              "Public Administration",
              "Nonclassifiable")
big_class_no = rep(0 : (length(big_class) - 1), 
                   c(1, 5, 4, 3, 20, 10, 2, 8, 7, 16, 7, 1))
big_class = rep(big_class, c(1, 5, 4, 3, 20, 10, 2, 8, 7, 16, 7, 1))

match = data.frame(category=category, category_no=category_no, 
                   big_class=big_class, big_class_no=big_class_no)
check_bad_col(match)
match$category = as.character(match$category)
match$category_no = as.integer(match$category_no)
match$big_class = as.character(match$big_class)
match$big_class_no = as.integer(match$big_class_no)
check_bad_col(match)



# Add columns to sto -----------------------------------------------------
sto$category_no = sto$hsiccd %/% 100
check_bad_col(sto)
head(sto)

no_show = setdiff(unique(sto$category_no), unique(match$category_no))
# Header SIC starting with 11, 66, 71 appears in stocks, 
#   but not appear in SIC classes these may be already abandoned. 
# So set the category_no of them to 0.
sto$category_no[sto$category_no %in% no_show] = 0

dim(sto)
sto = merge(sto, match, by="category_no", sort=F, all=F)
dim(sto)
col_names = c("permno", "ticker", "comnam", "hsiccd", "siccd", "naics",
              "category_no", "category", "big_class_no", "big_class",
              "exchcd", "hexcd", "shrcd", "shrcls",
              "cusip", "ncusip", "permco", "nwperm")
sto = sto[col_names]
dim(sto)
check_bad_col(sto)



# Save results
stocks_info = list(stocks_class=sto, class_table=match)
save(stocks_info, file="stocks_info.RData")



# Results presentation
source("01_func_class_presentation.R")
class_presentation(stocks_info$stocks_class$permno, 
                   stocks_info$stocks_class, 
                   stocks_info$class_table)



# Codes end here. The folllowing is the appendix -------------------------

# Appendix 1 : SIC big class description:
# 
# 0  0         Not Available
# 1  0100-0999 Agriculture, Forestry and Fishing
# 2  1000-1499 Mining
# 3  1500-1799 Construction
#    1800-1999 not used
# 4  2000-3999 Manufacturing
# 5  4000-4999 Transportation, Communications, Electric, Gas and Sanitary service
# 6  5000-5199 Wholesale Trade
# 7  5200-5999 Retail Trade
# 8  6000-6799 Finance, Insurance and Real Estate
# 9  7000-8999 Services
# 10 9100-9729 Public Administration
# 11 9900-9999 Nonclassifiable



# Appendix 2 : SIC small categories description:
# 
# A.  Division A: Agriculture, Forestry, And Fishing
# Major Group 01: Agricultural Production Crops
# Major Group 02: Agriculture production livestock and animal specialties
# Major Group 07: Agricultural Services
# Major Group 08: Forestry
# Major Group 09: Fishing, hunting, and trapping
# 
# B.  Division B: Mining
# Major Group 10: Metal Mining
# Major Group 12: Coal Mining
# Major Group 13: Oil And Gas Extraction
# Major Group 14: Mining And Quarrying Of Nonmetallic Minerals, Except Fuels
# 
# C.  Division C: Construction
# Major Group 15: Building Construction General Contractors And Operative Builders
# Major Group 16: Heavy Construction Other Than Building Construction Contractors
# Major Group 17: Construction Special Trade Contractors
# 
# D.  Division D: Manufacturing
# Major Group 20: Food And Kindred Products
# Major Group 21: Tobacco Products
# Major Group 22: Textile Mill Products
# Major Group 23: Apparel And Other Finished Products Made From Fabrics And Similar Materials
# Major Group 24: Lumber And Wood Products, Except Furniture
# Major Group 25: Furniture And Fixtures
# Major Group 26: Paper And Allied Products
# Major Group 27: Printing, Publishing, And Allied Industries
# Major Group 28: Chemicals And Allied Products
# Major Group 29: Petroleum Refining And Related Industries
# Major Group 30: Rubber And Miscellaneous Plastics Products
# Major Group 31: Leather And Leather Products
# Major Group 32: Stone, Clay, Glass, And Concrete Products
# Major Group 33: Primary Metal Industries
# Major Group 34: Fabricated Metal Products, Except Machinery And Transportation Equipment
# Major Group 35: Industrial And Commercial Machinery And Computer Equipment
# Major Group 36: Electronic And Other Electrical Equipment And Components, Except Computer Equipment
# Major Group 37: Transportation Equipment
# Major Group 38: Measuring, Analyzing, And Controlling Instruments; Photographic, Medical And Optical Goods; Watches And Clocks
# Major Group 39: Miscellaneous Manufacturing Industries
# 
# E.  Division E: Transportation, Communications, Electric, Gas, And Sanitary Services
# Major Group 40: Railroad Transportation
# Major Group 41: Local And Suburban Transit And Interurban Highway Passenger Transportation
# Major Group 42: Motor Freight Transportation And Warehousing
# Major Group 43: United States Postal Service
# Major Group 44: Water Transportation
# Major Group 45: Transportation By Air
# Major Group 46: Pipelines, Except Natural Gas
# Major Group 47: Transportation Services
# Major Group 48: Communications
# Major Group 49: Electric, Gas, And Sanitary Services
# 
# F.  Division F: Wholesale Trade
# Major Group 50: Wholesale Trade-durable Goods
# Major Group 51: Wholesale Trade-non-durable Goods
# 
# G.  Division G: Retail Trade
# Major Group 52: Building Materials, Hardware, Garden Supply, And Mobile Home Dealers
# Major Group 53: General Merchandise Stores
# Major Group 54: Food Stores
# Major Group 55: Automotive Dealers And Gasoline Service Stations
# Major Group 56: Apparel And Accessory Stores
# Major Group 57: Home Furniture, Furnishings, And Equipment Stores
# Major Group 58: Eating And Drinking Places
# Major Group 59: Miscellaneous Retail
# 
# H.  Division H: Finance, Insurance, And Real Estate
# Major Group 60: Depository Institutions
# Major Group 61: Non-depository Credit Institutions
# Major Group 62: Security And Commodity Brokers, Dealers, Exchanges, And Services
# Major Group 63: Insurance Carriers
# Major Group 64: Insurance Agents, Brokers, And Service
# Major Group 65: Real Estate
# Major Group 67: Holding And Other Investment Offices
# 
# I.  Division I: Services
# Major Group 70: Hotels, Rooming Houses, Camps, And Other Lodging Places
# Major Group 72: Personal Services
# Major Group 73: Business Services
# Major Group 75: Automotive Repair, Services, And Parking
# Major Group 76: Miscellaneous Repair Services
# Major Group 78: Motion Pictures
# Major Group 79: Amusement And Recreation Services
# Major Group 80: Health Services
# Major Group 81: Legal Services
# Major Group 82: Educational Services
# Major Group 83: Social Services
# Major Group 84: Museums, Art Galleries, And Botanical And Zoological Gardens
# Major Group 86: Membership Organizations
# Major Group 87: Engineering, Accounting, Research, Management, And Related Services
# Major Group 88: Private Households
# Major Group 89: Miscellaneous Services
# 
# J.  Division J: Public Administration
# Major Group 91: Executive, Legislative, And General Government, Except Finance
# Major Group 92: Justice, Public Order, And Safety
# Major Group 93: Public Finance, Taxation, And Monetary Policy
# Major Group 94: Administration Of Human Resource Programs
# Major Group 95: Administration Of Environmental Quality And Housing Programs
# Major Group 96: Administration Of Economic Programs
# Major Group 97: National Security And International Affairs
# Major Group 99: Nonclassifiable Establishments



# The end -----------------------------------------------------------------
