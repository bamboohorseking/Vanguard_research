## Set directory and load libraries
library(tidyverse)
library(pracma)
library(forecast)
library(splines)
setwd("C:/Users/Galle/Desktop/RProjects/Internship files/Vanguard_research-main")

## Load in data 
AMZN <- read_csv("Amazon1_demo.csv")
AMZN$BarTime <- as.POSIXct(AMZN$BarTime, tz = "UTC")
head(AMZN)

ggplot(data = AMZN) + aes(x = BarTime, y = Volume) + geom_line() +
  xlim(c(as.POSIXct('2019-08-06 08:00:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct('2019-08-09 23:59:59', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))) 

AMZN$EWMA <- movavg(AMZN$Volume, 9, 'e')

ggplot(data = AMZN) + geom_line(aes(x = BarTime, y = Volume, color = "Volume"), size = 1) +
  geom_line(aes(x = BarTime, y = EWMA, color = "EWMA"), size = 1) +
  xlim(c(as.POSIXct('2019-08-05 08:00:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"),
         as.POSIXct('2019-08-05 23:59:59', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))) 

etsmodel <- ets(AMZN$Volume[between(AMZN$BarTime,
                                    as.POSIXct('2019-08-05 13:00:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"),
                                    as.POSIXct('2019-08-05 19:45:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))])

test <- auto.arima(AMZN$Volume[between(AMZN$BarTime,
                                       as.POSIXct('2019-08-05 13:00:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"),
                                       as.POSIXct('2019-08-05 19:45:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))])
forecast(test,  h = 100) %>%
  autoplot()

## parabolic regression
test <- AMZN %>%
  filter(between(BarTime,
                 as.POSIXct('2019-08-05 13:30:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"),
                 as.POSIXct('2019-08-05 20:00:00', tz = 'UTC', format = "%Y-%m-%d %H:%M:%S"))) %>%
  mutate(NumericDate = as.numeric(BarTime)-1565011800, NumericDate2 = NumericDate^2) %>%
  select(BarTime, Volume, EWMA, NumericDate, NumericDate2)

summary(lm(EWMA ~ NumericDate + NumericDate2,data = test))


ggplot(test) + aes(x = NumericDate, y = EWMA) + geom_point() +
  stat_smooth(formula = y ~ x + I(x^2), method = 'lm', data = test)

numericdate <- test$NumericDate
volume <- test$Volume

lin.mod <- lm(volume ~ numericdate)
summary(lin.mod)

summary(segmented(lin.mod, seg.Z = ~ numericdate))

ggplot(test) + aes(x = NumericDate, y = EWMA) + geom_point() +
  stat_smooth(formula = y ~ ns(x,df = 4), method = 'lm', data = test)

summary(lm(Volume ~ bs(NumericDate, df = 15), data = test))[1:15]
summary(lm(Volume ~ ns(NumericDate, df = 15), data = test))
predict(lm(Volume ~ bs(NumericDate, df = 15), data = test))[1:15]


plot(diff(AMZN$Volume))
