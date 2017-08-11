library(jsonlite)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)

# get data from raspberry pi
data <- fromJSON('http://sogangds.com:3000/api/dust/all')
data <- tbl_df(data)
data <- data[,-1]
data %>% write_csv("data-2017-08-10.csv")

data %>% select(PM25, PM1, PM10) %>%
  sample_n(1000) %>%
  pairs()

# 2017-07-06T11:15:03.460Z

df <- data %>%
  mutate(date=lubridate::parse_date_time(date, "YmdHMS")) %>%
  # select(-`_id`)
  select(-1) %>%
  select(date, alt, atm, humid, temp, PM1, PM10, PM25) %>%
  mutate(yy=year(date),
         mm=month(date),
         dd=day(date),
         hh=hour(date))


df %>% glimpse() 

df %>%
  ggplot(aes(date, PM25)) + geom_point() +
  geom_hline(yintercept=100, col='red')

df %>%
  mutate(interval=c(0, diff(date))) %>%
  ggplot(aes(date, interval)) + geom_point() 

df %>%
  ggplot(aes(hh, PM25)) + geom_jitter(alpha=.1) 

df %>%
  ggplot(aes(hh, PM25)) + geom_jitter(alpha=.1) +
  scale_y_log10()

df %>%
  ggplot(aes(as.factor(hh), PM25)) + geom_boxplot() +
  scale_y_log10()

df %>%
  ggplot(aes(as.factor(hh), temp)) + geom_boxplot() +
  scale_y_log10()

df %>%
  ggplot(aes(as.factor(hh), temp)) + geom_boxplot() +
  scale_y_log10() + facet_wrap(~mm)

df %>%
  ggplot(aes(as.factor(hh), humid)) + geom_boxplot() +
  facet_wrap(~mm)

df %>%
  ggplot(aes(as.factor(hh), atm)) + geom_boxplot() +
  facet_wrap(~mm)

df %>%
  summarize(humid=mean(humid),
            temp=mean(temp))

df %>%
  group_by(mm) %>%
  summarize(humid=mean(humid),
            temp=mean(temp),
            atm=mean(atm, na.rm=TRUE))



glimpse(df)
df %>% 
  # select(atm, humid, temp, PM1, PM10, PM25) %>%
  # select(atm, humid, temp, PM25) %>%
  transmute(atm, humid, temp, log10(PM25)) %>%
  sample_n(1000) %>%
  pairs()

df %>% filter(PM25<=0)

df %>% 
  transmute(atm, humid, temp, log10(PM25+1))  %>%
  cor(use="pairwise.complete.obs")

df %>%
  ggplot(aes(PM25)) + geom_histogram()

df %>%
  ggplot(aes(PM25)) + geom_histogram() +
  scale_x_log10()
