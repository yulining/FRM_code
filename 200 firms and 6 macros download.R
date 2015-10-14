# --------------------------------------------------------
# Project:     FRM
#---------------------------------------------------------
# QuantLet : FRM_download_data
#---------------------------------------------------------
# Published in : FRM: Financial Risk Meter
#---------------------------------------------------------
# Description : download the data of 206 variables
# from Yahoo finance and Feferal reserve board.
# --------------------------------------------------------
# Keywords : automatic, VaR, returns, Yahoo Finance,
# Feferal Reserve Board, quantile regression, lasso, risk
#---------------------------------------------------------
# See also :
#---------------------------------------------------------
# Author : Thijs Benschop, Lukas Borke, Lining Yu 
#---------------------------------------------------------
# Submitted :
#---------------------------------------------------------
# Datafile : 
#----------------------------------------------------------

# clear all variables
rm(list                      = ls(all = TRUE))
graphics.off()
#######################################################################
###################### Part 1 download 200 firms ######################
########################2##############################################
## set the working directory
setwd("//clapton.wiwi.hu-berlin.de/frm/codes")

companylist                  = read.csv("companylist 2015.csv") # for symbols in Yahoo finance

# companylist2015 - downloaded from this source
# http://www.nasdaq.com/screening/companies-by-industry.aspx?industry=Finance&sortname=country&sorttype=1

# Array with firm names
firm_names                   = as.character(companylist[, 1]) # sorterd by market capitalization

# Now, Yahoo Finance
library(quantmod)

a                            = "2006-12-29" #### starting date for 200 firms
a_m                          = "2006-12-28" #### starting date for macro variables, 1 day lag in data retrieval
b                            = Sys.Date() #### date for 200 firms
b_m                          = Sys.Date()-1 #### date for macro variables, 1 day lag

example                      = getSymbols(firm_names[1], src = "yahoo", from = a, to = b, auto.assign = FALSE) # calls Yahoo Finance through internet
example_time                 = as.matrix(example[-1, 1])
time_points                  = nrow(as.data.frame(example))

# setting parameters for the loop
n                            = length(firm_names) # number of firms
s                            = 1                  # counter (initial value is 1)
max_num                      = 200                # number of firms used

# creating initial matrix for all firms and all time points
firms_closed_price           = matrix(0, time_points, n)
colnames(firms_closed_price) = firm_names

# sometimes data for a company can not be captured (bad internet connection, problems with correct name)
# in this case extend this list
# the list can vary from time to time
bad_list                      = c("ZIONZ", "SNFCA", "KMPA", "CATYW", "MBFIP","HAWKB","JLL") # cannot connect to Yahoo Finance

# Main loop : data from yahoo to firms_closed_price
for (i in 1: n) {
  # 1) Check whether contained in bad list, if then skip
	if (firm_names[i] %in% bad_list) {next}
	
  # 2) check whether the complete time series is available, if not skip and print name
  prices                     = getSymbols(firm_names[i], src = "yahoo", from = a, to = b, auto.assign = FALSE)
	if (nrow(prices)           != time_points) {
		# skip this firm if different length of time points
		print(firm_names[i])
		next
	}
  
	# 3) get data and save in firms_closed_price
	prices_data                = as.data.frame(prices)
	firms_closed_price[, i]    = as.matrix(prices_data[, 6])
	print(s)
	s                          = s + 1 # increase counter
	
	# 4) stop if max_num achieved
	if (s > max_num) break
}

# checking which firms were filled
cs                           = colSums(firms_closed_price)  # sum over 2201 daily observations to check for zero columns
#cs[cs > 0]        
length(cs[cs > 0])                                          # number of firms for whch the sum is larger than 0

# taking submatrix from firms_closed_price with 200 companies
collected_firms              = names(cs[cs > 0])
data_firms                   = firms_closed_price[, collected_firms]

# transform the company_prices_200 into log returns
returns_final                = diff(log(data_firms))
rownames(returns_final)      = rownames(example_time)

#######################################################################
################### Part 2 download macro variables ###################
#######################################################################
macro_names                  = c("^VIX","^GSPC","IYR","DGS3MO","DGS10","DBAA")
#### Part 2.1: download VIX, GSPC (S&P500) and IYR (iShares Dow Jones US Real Estate) from yahoo finance 
VIX                          = as.matrix(getSymbols(macro_names[1], src = "yahoo", from = a_m, to = b_m, auto.assign = FALSE)[, 6])
GSPC                         = as.matrix(getSymbols(macro_names[2], src = "yahoo", from = a_m, to = b_m, auto.assign = FALSE)[, 6])
IYR                          = as.matrix(getSymbols(macro_names[3], src = "yahoo", from = a_m, to = b_m, auto.assign = FALSE)[, 6])

# data of first three variables
data_ft                      = as.matrix(cbind(VIX, GSPC, IYR))

### transform GSPC and IYR into log returns ###
returns_m                    = diff(log(data_ft[, -1])) # without VIX
First_three_macro_in         = cbind(data_ft[-1, 1],returns_m)
First_three_macro            = First_three_macro_in[-nrow(First_three_macro_in), ] # remove last row

#### Part 2.2: download the other 3 macro from Federal reserve Bank #####
# measure the length of first three macro variables, so that the last three variables have the same length with them.
c                            = nrow(First_three_macro)

### 3 month Treasury change####
## download 3 month Treasury maturities ###
## 20150713 can not use getSymbol to download from Federal any more, only can use the following links.
ThreeMT                      = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DGS3MO/downloaddata/DGS3MO.csv", na.strings =".")[,-1]))
True_ThreeMT                 = na.omit(ThreeMT)
lt                           = as.matrix(True_ThreeMT)
# set the length of this variable the same as the first three variables, take last c values
output_ThreeMT               = as.matrix(lt[(length(lt)-c):length(lt), 1])
# calculate the 3 month Treasury change
change_ThreeMT               = diff(output_ThreeMT)

### Slope of yield curve ###
## download 10 year Treasury maturities ###
Tenyield                     = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DGS10/downloaddata/DGS10.csv", na.strings =".")[,-1]))
True_Tenyield                = na.omit(Tenyield)
lyc                          = as.matrix(True_Tenyield)
output_Tenyield              = as.matrix(lyc[(length(lyc)-c):length(lyc), 1])
# calculate Slope of yield curve
slope_yield                  = as.matrix(output_Tenyield-output_ThreeMT)[-1]

### credit spread ###
## download BAA ###
DayBAA                       = as.numeric(as.matrix(read.csv("https://research.stlouisfed.org/fred2/series/DBAA/downloaddata/DBAA.csv", na.strings =".")[,-1]))
True_DayBAA                  = na.omit(DayBAA)
lc                           = as.matrix(True_DayBAA)
output_True_DayBAA           = as.matrix(lc[(length(lc)-c):length(lc), 1])
# calculate credit spread
credit_spread                = as.matrix(output_True_DayBAA-output_Tenyield)[-1]

### combine all the macro variables ###
rest_three_macro             = cbind(change_ThreeMT, slope_yield, credit_spread)
six_macro                    = cbind(First_three_macro, rest_three_macro)

### scale variables to [0,1] ###
scale_macro                  = six_macro
nnrow                        = nrow(scale_macro)
nncol                        = ncol(scale_macro)
m                            = matrix(0, nnrow, nncol)
for(i in 1:nncol)
  {
    m[, i]                   = (scale_macro[, i] - min(scale_macro[, i]))/(max(scale_macro[, i])-min(scale_macro[, i]))
  }
colnames(m)                  = c("^VIX", "^GSPC", "IYR", "3MTCM", "Yield", "Credit")

#######################################################################
########### Part 3 combine 200 firms and 6 macro variables ############
#######################################################################

firms_data                   = returns_final
macro_data                   = m
full_data                    = cbind(firms_data, macro_data)
full_data                    = round(full_data, digits = 9)
Date_wf                      = strptime(as.character(rownames(full_data)),  "%Y-%m-%d")
Date_rf                      = format(Date_wf,"%d/%m/%Y")
Date                         = as.data.frame(Date_rf)
names(Date)                  = "Date"
rownames(full_data)          = NULL
final_data                   = cbind(Date, full_data)
setwd("//clapton.wiwi.hu-berlin.de/frm/data")
write.csv(format(final_data, scientific = FALSE), file = paste("200_firms_returns_and_scaled_macro_", b, ".csv", sep = ""), row.names = FALSE, quote = FALSE)
