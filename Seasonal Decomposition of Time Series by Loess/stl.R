# Seasonal Decomposition of Time Series by Loess
# SQL
# select o.oh_created_date_key as date_key
# , sum(o.mv) as mv
# , count(o.oh_order_id) as order
# , count(distinct o.customer_key) as customer
# from agg.order_fact_totals o
# where o.ignore = 0
# and o.oh_checkout_status = 'GREEN'
# and o.oh_merch_id = 2561 -- J.CREW
# and o.oh_created_date_key between 20150101 and 20170313
# group by 1
# order by 1

getwd()
df <- read.table("clipboard", 
                       sep = '\t', 
                       header = T, 
                       stringsAsFactors = F, 
                       comment.char = "", 
                       allowEscape = T, 
                       # fill = T allows rows to have unequal length
                       quote = "\n")

?suppressMessages
options(warn = -1)  # suppress warning messages for loading below packages
library(ggplot2); library(lubridate)
options(warn = 0)  # restore default

head(df); dim(df)
df$date <- ymd(df$date_key)
df$month <- month(df$date)
df$year <- year(df$date)
df$year_month <- substr(df$date_key, 1, 6)

#############################
### testing data with df2 ###
df2 <- df
windows()

###########################################################
## convert daily number of customers into time series with ts() ##
## Create a daily Date object - helps my work on dates
## read <- http://align-alytics.com/seasonal-decomposition-of-time-series-by-loessan-experiment/
## read2 <- http://stackoverflow.com/questions/33128865/starting-a-daily-time-series-in-r
## read3 <- https://www.otexts.org/fpp/6/5

#### MUST READ !!! ####
## read4 <- https://anomaly.io/seasonal-trend-decomposition-in-r/

inds <- seq(as.Date("2015-01-01"), as.Date("2016-12-31"), by = "day")
time.series <- ts(subset(df2$customer, df2$year %in% c(2015, 2016)), 
                  frequency = 365,  
                  # frequency is the number of observations per unit time 
                  # (1=annual, 4=quartly, 12=monthly, etc.)
                  start = c(2015, 
                            as.numeric(format(inds[1], "%j")))
                  )  # create time series ts()
plot(time.series)

########## stl() "seasonal and trend decomposition using Loess" ##########
decomposed <- stl(time.series, 
                  s.window = "periodic")
seasonal <- decomposed$time.series[, 1]
trend <- decomposed$time.series[, 2]
remainder <- decomposed$time.series[, 3]

plot(trend + remainder, 
     main = "Widget Sales over Time, Seasonally Adjusted",
     ylab = "sales")

plot(decomposed)

# How stl() Works
# http://align-alytics.com/seasonal-decomposition-of-time-series-by-loessan-experiment/

# When calling stl() with s.window="periodic", the seasonal component for January is simply the mean of all January values. Similarly, the seasonal component for February is simply the mean of all February
# values, etc. Otherwise, the seasonal component is calculated using loess smoothing (discussed below).
# 
# Having calculated the seasonal component, the seasonally-adjusted data (the original data minus the seasonal component) is loess-smoothed to determine the trend.
# 
# The remainder/noise is then the original data minus the seasonal and trend components.
# 
# The stl() function is quite flexible:
#         
#         The seasonality does not have to run across a year. Any period may be used for this.
# The decomposition process can accommodate seasonality that changes over time.
# A robust decomposition process is available that is less affected by outliers than is the default.



