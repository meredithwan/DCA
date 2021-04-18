# loading stock data from Yahoo Finance
library(quantmod)
# look up stock symbol from Yahoo Finance
# 10 years of data
getSymbols("ES3.SI", from='2010-01-01',to='2020-07-26')
STI = na.omit(ES3.SI)
# dataset spans between 4 Jan 2010 to 24 July 2020
head(STI)
tail(STI)

# save into a dataframe
df = data.frame(date=index(STI), coredata(STI))
sapply(df, class)

# load dividends data
# data from https://www.dividends.sg/view/ES3
dividends = read.csv("dividends.csv", header=TRUE)
# check variable class
sapply(dividends, class)

dividends$Ex.Date = as.Date(dividends$Ex.Date, format = '%d/%m/%y')
dividends$Pay.Date = as.Date(dividends$Pay.Date, format = '%d/%m/%y')
# check variable class (especially the date)
sapply(dividends, class)

# sum up multiple dividend payments on the same date
dividends2 = aggregate(x= dividends$Amount,
                       by= list(dividends$Ex.Date, dividends$Pay.Date),
                       FUN=sum)
sapply(dividends2, class)

# renaming columns in dataframe
colnames(dividends2)[1] = "date"
colnames(dividends2)[2] = "Pay_date"
colnames(dividends2)[3] = "dpu_ex"

#  merge dividends dataframe and price dataframe
df2 = merge(df, dividends2, by=c("date"), all=TRUE)

# renaming columns in dataframe
colnames(dividends2)[1] = "ex_date"
colnames(dividends2)[2] = "date"
colnames(dividends2)[3] = "dpu_p"

#  merge dividends dataframe and price dataframe
df2 = merge(df2, dividends2, by=c("date"), all=TRUE)

# export as csv
write.csv(df2,"sti.csv")

# load csv file
df3 = read.csv("sti.csv", header=TRUE)
# somehow date is a factor variable
df3$date = as.Date(df3$date)
sapply(df3, class)

# reference code in case ever need to extract certain elements of a date
# library(lubridate)
# df3$day = mday(df3$date)

library(tidyr)
library(dplyr)
library(bizdays)
library(timeDate)
# https://stackoverflow.com/questions/43595457/alternate-compiler-for-installing-r-packages-clang-error-unsupported-option
library(RQuantLib)

# completed with reference to: https://stackoverflow.com/questions/32253412/index-an-xts-object-by-the-date-nearest-to-the-nth-of-every-month
# load business day calendar for SGX
load_quantlib_calendars('Singapore', from='2010-01-01', to='2020-07-26')
min = as.Date(df3[5, 2])
max = as.Date(df3$date[length(df3$date)])
# get dataframe with a list of the 8th of every month
rng = data.frame(date = seq.Date(min,max,by="month"))
# adjust to next business day if 8th is a holiday/weekend in Singapore
rng$date = adjust.next(rng$date, cal = "QuantLib/Singapore")
# generate buy signal for these dates
rng$signal = 1

# merge with  original dataframe
df5 = merge(df3, rng, by=c("date"), all=TRUE)
# check for NA
sum(is.na(df5$ES3.SI.Close))
# fix error and reassign signal to next business day
df5 = df5[-c(1528),]
df5$signal[1528] = 1
# insert no buy signal for other dates
df5$signal[is.na(df5$signal)] = 0
# just to check, should have 127 buy signals in total
print(sum(df5$signal))

# reference code just in case ever need to fill in missing dates
# df4 = df3 %>% mutate(date = as.Date(date)) %>% complete(date = seq.Date(min(date), max(date), by="day"))
# lol <- function(x) {
#   max_dpu <- max(which(x!=0))
#   return(max_dpu)
# }

# replace with 0 for days where there are no dividends announced
df5$dpu_ex[is.na(df5$dpu_ex)] = 0

# create a signal for dates where dividends are paid
df5$dpu_psig = df5$dpu_p
df5$dpu_psig[!is.na(df5$dpu_psig)] = 1
df5$dpu_psig[is.na(df5$dpu_psig)] = 0

# PART 1: DCA WITH NO REINVESTMENT OF DIVIDENDS

# monthly investment amount
df5$rsp_amt = 1300

# calculate number of units purchased each time (round down to nearest whole number)
# takes into account commissions and other charges
# https://secure.fundsupermart.com/fsm/new-to-fsm/pricing-structure
df5$units_purchased = floor(df5$signal*df5$rsp_amt/(df5$ES3.SI.Close*1.001284))
# add up all units purchased
df5$total_units = cumsum(df5$units_purchased)
# find amount paid for each purchase (includes commissions and other charges)
df5$amt_paid_for_units = df5$ES3.SI.Close*1.001284*df5$units_purchased
# add up all amounts paid on units
df5$total_amt_paid = cumsum(df5$amt_paid_for_units)

# find total amount of dividends to be received each time
df5$total_dpuex = df5$dpu_ex*df5$total_units

# this column reflects the total amount of dividends to be received based on the previous ex-date
# for example, if 20 Jan 2010 and 28 July 2010 ex-date
# rows from 20 Jan to 27 Jan will reflect the total amount of dividends to be received based on the DPU for 20 Jan
# this column is created in order to log the amount of dividends that will be received on the payment date
# this is slightly tricky because the total amount of dividends received is calculated based on holdings on ex-date
# which may not be the holdings on payment date (another investment may have been made in between ex-date and payment date)
for(i in 1:nrow(df5)){
  if(i!=1){    
    if(df5$total_dpuex[i]==0){
      df5$run[i]= df5$run[i-1]
    }else{
      df5$run[i]=df5$total_dpuex[i]  
    }
  
  # for the first row in dataframe
  }else{
    df5$run[i]=df5$total_dpuex[i]
  }
}

# total dividends received on payment date
df5$total_dpup = df5$run*df5$dpu_psig
# add up the total amount of dividends received
df5$total_dpup_cumulative = cumsum(df5$total_dpup)

# total portfolio value without reinvestment of dividends
df5$total_value_no_reinvest = df5$total_units*df5$ES3.SI.Close + df5$total_dpup_cumulative

# plot chart to compare portfolio value against amount invested (includes commissions)
library(ggplot2)
theme_set(theme_minimal())

library("tidyverse")
df6 = df5 %>%
  select(date, total_value_no_reinvest, total_amt_paid) %>%
  gather(key = "Legend", value = "value", -date)
head(df6)

ggplot(df6, aes(x = date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(labels = c("Total amount paid", "Total value of portfolio"), values = c("darkred", "steelblue")) +
  labs(title= "Performance of dollar cost averaging into the STI without reinvestment of dividends", y="Value (SGD)")

# total returns
df5$total_return_no_reinvest = round((df5$total_value_no_reinvest-df5$total_amt_paid)/df5$total_amt_paid*100, digits=2)
df5$total_return_no_reinvest[is.na(df5$total_return_no_reinvest)] = 0
# plot chart
ggplot(data = df5, aes(x = date, y = total_return_no_reinvest))+
  geom_line()+
  labs(title= "Total returns from dollar cost averaging into the STI without reinvestment of dividends", y="Returns (%)")

# xirr
yr = strftime(df5$date, "%Y")
amt = df5$amt_paid_for_units
div = df5$total_dpup
dd = data.frame(yr, amt, div)

dd.agg = aggregate(. ~ yr, dd, FUN=sum)

dd.agg$cashflow = dd.agg$div-dd.agg$amt

# add in final portfolio value as cashflow
dd.agg$cashflow[11] = dd.agg$cashflow[11]+166675.72

dd.agg$num_years = as.numeric(dd.agg$yr)-1

dd.agg$num_years[11] = 9.6

library(jrvFinance)
round(irr(cf=dd.agg$cashflow, cf.t=dd.agg$num_years)*100, digits=2)

# CAGR
print(((tail(df5, 1)$total_value_no_reinvest/tail(df5, 1)$total_amt_paid)^(1/9.6)-1)*100)

# PART 2: DCA WITH REINVESTMENT OF DIVIDENDS

# this columns records the dividends received each time from payment date until the next investment date
for(i in 1:nrow(df5)){
  df5$runp[i]=df5$total_dpup[i]
  if(i!=1){
    if(df5$total_dpup[i]==0){
      if(df5$signal[i-1]==0){
        df5$runp[i]= df5$runp[i-1]
      }else{
        df5$runp[i]= 0}
    }
   }
}

# add dividends to monthly investment amount
df5$rsp_amt_reinvest = df5$rsp_amt + df5$runp

# calculate number of units purchased each time (round down to nearest whole number)
# takes into account commissions and other charges
# https://secure.fundsupermart.com/fsm/new-to-fsm/pricing-structure
df5$units_purchased_reinvest = floor(df5$signal*df5$rsp_amt_reinvest/(df5$ES3.SI.Close*1.001284))
# add up all units purchased
df5$total_units_reinvest = cumsum(df5$units_purchased_reinvest)
# find amount paid for each purchase (includes commissions and other charges)
df5$amt_paid_for_units_reinvest = df5$ES3.SI.Close*1.001284*df5$units_purchased_reinvest
# add up all amounts paid on units
df5$total_amt_paid_reinvest = cumsum(df5$amt_paid_for_units_reinvest)

# total portfolio value with reinvestment of dividends
# leftover dividend amounts that are not reinvested are included in portfolio value
df5$total_value_reinvest = df5$total_units_reinvest*df5$ES3.SI.Close + df5$total_dpup_cumulative

# plot chart to compare portfolio value against amount invested (includes commissions)
library(ggplot2)
theme_set(theme_minimal())

library("tidyverse")
df7 = df5 %>%
  select(date, total_value_reinvest, total) %>%
  gather(key = "Legend", value = "value", -date)
head(df7)

ggplot(df7, aes(x = date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(labels = c("Total amount paid", "Total value of portfolio"), values = c("darkred", "steelblue")) +
  labs(title= "Performance of dollar cost averaging into the STI with reinvestment of dividends", y="Value (SGD)")

# total returns
df5$total_return_reinvest = round((df5$total_value_reinvest-df5$total_amt_paid_reinvest)/df5$total_amt_paid_reinvest*100, digits=2)
df5$total_return_reinvest[is.na(df5$total_return_reinvest)] = 0
# plot chart
ggplot(data = df5, aes(x = date, y = total_return_reinvest))+
  geom_line()+
  labs(title= "Total returns from dollar cost averaging into the STI with reinvestment of dividends", y="Returns (%)")

# xirr
yr = strftime(df5$date, "%Y")
amt_reinvest = df5$amt_paid_for_units_reinvest
div = df5$total_dpup
dd = data.frame(yr, amt_reinvest, div)

dd.agg = aggregate(. ~ yr, dd, FUN=sum)

dd.agg$cashflow = dd.agg$div-dd.agg$amt_reinvest

# add in final portfolio value as cashflow
dd.agg$cashflow[11] = dd.agg$cashflow[11]+189994.7

dd.agg$num_years = as.numeric(dd.agg$yr)-1

dd.agg$num_years[11] = 9.6

library(jrvFinance)
round(irr(cf=dd.agg$cashflow, cf.t=dd.agg$num_years)*100, digits=2)

# CAGR
print(((tail(df5, 1)$total_value_reinvest/tail(df5, 1)$total_amt_paid_reinvest)^(1/9.6)-1)*100)

# compare amount paid and portfolio value for reinvesting vs no reinvesting of dividends
library(ggplot2)
theme_set(theme_minimal())

library("tidyverse")
df8 = df5 %>%
  select(date, total_value_reinvest, total_amt_paid_reinvest, total_value_no_reinvest, total_amt_paid) %>%
  gather(key = "Legend", value = "value", -date)
head(df8)

ggplot(df8, aes(x = date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(labels = c("Total amount paid (no reinvestment)", "Total amount paid (reinvestment)", "Portfolio value (no reinvestment)", "Portfolio value (reinvestment)"), values = c("darkred", "steelblue", "green", "purple")) +
  labs(title= "Reinvestment of dividends vs no reinvestment of dividends", y="Value (SGD)")

# compare total returns for reinvesting vs no reinvesting of dividends
library(ggplot2)
theme_set(theme_minimal())

library("tidyverse")
df9 = df5 %>%
  select(date, total_return_reinvest, total_return_no_reinvest) %>%
  gather(key = "Legend", value = "value", -date)
head(df9)

ggplot(df9, aes(x = date, y = value)) + 
  geom_line(aes(color = Legend)) + 
  scale_color_manual(labels = c("No reinvestment", "Reinvestment"), values = c("darkred", "steelblue")) +
  labs(title= "Total returns of reinvestment of dividends vs no reinvestment of dividends", y="Returns (%)")

# plot price chart
ggplot(df5, aes(x = date, y = ES3.SI.Close)) +
  geom_line() +
  labs(title= "STI prices over 10 years", y="Price (SGD)")