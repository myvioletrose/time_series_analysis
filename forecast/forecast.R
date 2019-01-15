library(ggplot2)
library(reshape2)
library(tidyverse)
library(plyr)
library(dplyr)
library(lubridate)
library(forecast)

## SQL
# select md.merch_id 
# , md.merch_name as merchant
# , o.oh_created_date_key as date_key
# , sum(o.mv) as mv
# , count(o.oh_order_id) as order
# , count(distinct o.customer_key) as customer
# from agg.order_fact_totals o
# join dw.merchant_dim md on o.oh_merch_id = md.merch_id and md.date_to = '2199-12-31' and md.ignore = 0
# where o.ignore = 0
# and o.oh_checkout_status = 'GREEN'
# and o.oh_merch_id in (2181, 1300, 1620, 2561, 2180)
# and o.oh_created_date_key between 20130101 and 20170319
# group by 1, 2, 3
# order by 2, 3

df <- read.table("clipboard", sep = "\t", header = T, 
                 stringsAsFactors = F, allowEscapes = T, fill = T, 
                 comment.char = "", quote = "\n")
df$date <- ymd(df$date_key)
df$year <- year(df$date)
df$month <- month(df$date)

merch <- distinct(select(df, merchant, merch_id))

df.agg <- aggregate(cbind(mv, order, customer) ~ merch_id + merchant + year + month, 
                    data = df, sum)
head(df.agg, 50)
df.agg <- arrange(df.agg, year, month)

################## J.CREW ##################
jcrew <- subset(df.agg, merch_id == 2561 & year != 2017)
jcrew.ts <- ts(jcrew$customer, frequency = 12, start = c(2013, 1))
jcrew.stl <- stl(jcrew.ts, s.window = "periodic")
windows(); dev.set(2)
plot(jcrew.stl)

windows(); dev.set(3)
ggplot(data = subset(df, merch_id == 2561), aes(x = date, y = customer)) +
        geom_line() + 
        geom_smooth(aes(col = factor(year)))

################## MACYS.COM ##################
macys <- subset(df.agg, merch_id == 2181 & year != 2017)
macys.ts <- ts(macys$customer, frequency = 12, start = c(2013, 1))
macys.stl <- stl(macys.ts, s.window = "periodic")
windows(); dev.set(5)
plot(macys.stl)

windows(); dev.set(4)
ggplot(data = subset(df, merch_id == 2181), aes(x = date, y = customer)) +
        geom_line() + 
        geom_smooth(aes(col = factor(year)))

################## NORDSTROM ##################
nord <- subset(df.agg, merch_id == 1620 & year != 2017)
nord.ts <- ts(nord$customer, frequency = 12, start = c(2013, 1))
nord.stl <- stl(nord.ts, s.window = "periodic")
windows(); dev.set(4)
plot(nord.stl)

windows(); dev.set(5)
ggplot(data = subset(df, merch_id == 1620), aes(x = date, y = customer)) +
        geom_line() + 
        geom_smooth(aes(col = factor(year)))

################## SAKS.COM ##################
saks <- subset(df.agg, merch_id == 1300 & year != 2017)
saks.ts <- ts(saks$customer, frequency = 12, start = c(2013, 1))
saks.stl <- stl(saks.ts, s.window = "periodic")
windows(); dev.set(4)
plot(saks.stl)

windows(); dev.set(3)
ggplot(data = subset(df, merch_id == 1300), aes(x = date, y = customer)) +
        geom_line() + 
        geom_smooth(aes(col = factor(year)))

################## BLOOMINGDALES.COM ##################
bloom <- subset(df.agg, merch_id == 2180 & year != 2017)
bloom.ts <- ts(bloom$customer, frequency = 12, start = c(2013, 1))
bloom.stl <- stl(bloom.ts, s.window = "periodic")
windows(); dev.set(4)
plot(bloom.stl)

windows(); dev.set(5)
ggplot(data = subset(df, merch_id == 2180), aes(x = date, y = customer)) +
        geom_line() + 
        geom_smooth(aes(col = factor(year)))
########################################################
#######################################################################













