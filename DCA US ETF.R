# loading stock data from Yahoo Finance
library(quantmod)
# look up stock symbol from Yahoo Finance
# 10 years of data
getSymbols("IVV", from='2010-01-01',to='2020-07-26')
IVV = na.omit(IVV)
# dataset spans between 4 Jan 2010 to 24 July 2020
head(IVV)
tail(IVV)

# save into a dataframe
df = data.frame(date=index(IVV), coredata(IVV))
sapply(df, class)

# DCA WITH NO REINVESTMENT OF DIVIDENDS
# simplify by excluding dividends (returns solely come from share price appreciation)
library(tidyr)
library(dplyr)
library(bizdays)
library(timeDate)
# https://stackoverflow.com/questions/43595457/alternate-compiler-for-installing-r-packages-clang-error-unsupported-option
library(RQuantLib)

# completed with reference to: https://stackoverflow.com/questions/32253412/index-an-xts-object-by-the-date-nearest-to-the-nth-of-every-month
# load business day calendar for NYSE
load_quantlib_calendars('UnitedStates', from='2010-01-01', to='2020-07-26')
min = as.Date(df$date[5])
max = as.Date(df$date[length(df$date)])
# get dataframe with a list of the 8th of every month
rng = data.frame(date = seq.Date(min,max,by="month"))
# adjust to next business day if 8th is a holiday/weekend in Singapore
rng$date = adjust.next(rng$date, cal = "QuantLib/UnitedStates")
# generate buy signal for these dates
rng$signal = 1

# merge with  original dataframe
df2 = merge(df, rng, by=c("date"), all=TRUE)
# check for NA
sum(is.na(df2$ES3.SI.Close))
# insert no buy signal for other dates
df2$signal[is.na(df2$signal)] = 0
# just to check, should have 127 buy signals in total
print(sum(df2$signal))

# monthly investment amount
# in US$, so roughly equal to S$1300
df2$rsp_amt = 1000

# calculate number of units purchased each time (round down to nearest whole number)
# takes into account commissions and other charges
# https://secure.fundsupermart.com/fsm/new-to-fsm/pricing-structure
# 0.08% of US$1000 is less than US$1
df2$units_purchased = floor(df2$signal*df2$rsp_amt/(df2$IVV.Close+1.07))
# add up all units purchased
df2$total_units = cumsum(df2$units_purchased)
# find amount paid for each purchase (includes commissions and other charges)
df2$amt_paid_for_units = df2$IVV.Close*df2$units_purchased+df2$signal*1.07
# add up all amounts paid on units
df2$total_amt_paid = cumsum(df2$amt_paid_for_units)

# total portfolio value excluding dividends
df2$total_value = df2$total_units*df2$IVV.Close

# plot chart to compare portfolio value against amount invested (includes commissions)
library(ggplot2)
theme_set(theme_minimal())

library("tidyverse")
df3 = df2 %>%
  select(date, total_value, total_amt_paid) %>%
  gather(key = "Legend", value = "value", -date)
head(df3)

ggplot(df3, aes(x = date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(labels = c("Total amount paid", "Total value of portfolio"), values = c("darkred", "steelblue")) +
  labs(title= "Performance of dollar cost averaging into the S&P500 without dividends", y="Value (USD)")

# total returns
df2$total_return = round((df2$total_value-df2$total_amt_paid)/df2$total_amt_paid*100, digits=2)
df2$total_return[is.na(df2$total_return)] = 0
# plot chart
ggplot(data = df2, aes(x = date, y = total_return))+
  geom_line()+
  labs(title= "Total returns from dollar cost averaging into the S&P500 without dividends", y="Returns (%)")

# xirr
yr = strftime(df2$date, "%Y")
amt = df2$amt_paid_for_units
div = 0
dd = data.frame(yr, amt, div)

dd.agg = aggregate(. ~ yr, dd, FUN=sum)

dd.agg$cashflow = dd.agg$div-dd.agg$amt

# add in final portfolio value as cashflow
dd.agg$cashflow[11] = dd.agg$cashflow[11]+tail(df2, 1)$total_value

dd.agg$num_years = as.numeric(dd.agg$yr)-1

dd.agg$num_years[11] = 9.6

library(jrvFinance)
round(irr(cf=dd.agg$cashflow, cf.t=dd.agg$num_years)*100, digits=2)

# CAGR
print(((tail(df2, 1)$total_value/tail(df2, 1)$total_amt_paid)^(1/9.6)-1)*100)

# plot price chart
ggplot(df2, aes(x = date, y = IVV.Close)) +
  geom_line() +
  labs(title= "S&P500 prices over 10 years", y="Price (USD)")