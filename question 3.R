library(tidyverse)
library(plyr)
library(dplyr)
library(chron)
library(forecast)
options(scipen=999)

summer.camp.data %>%
  group_by(source) %>%
  summarise(revenue = sum(revenue)) %>%
  arrange(desc(revenue))

covid_dates <- split(summer.camp.data, 
                     summer.camp.data$date<as.Date("2020-03-01"))

before_cov <- ldply(covid_dates[2], data.frame)
after_cov <- ldply(covid_dates[1], data.frame)

before_cov %>%
  summarise(sum(revenue) / sum(transactions))

after_cov_small <- after_cov %>%
  filter(after_cov$date >= '2020-03-01' & 
           after_cov$date <= '2020-05-31')
after_cov_small %>%
  summarise(sum(revenue) / sum(transactions))

after_cov %>%
  summarise(sum(revenue) / sum(transactions))


summer.camp.data %>%
  group_by(promo_activated) %>%
  dplyr::summarize(sum(revenue) / sum(transactions))

CR_testing <- summer.camp.data %>%
  mutate(CR = transactions / visits * 100) %>%
  mutate(weekend = chron::is.weekend(summer.camp.data$date)) %>%
  group_by(weekend)

CR_weekend <- CR_testing %>%
  filter(weekend == TRUE) %>%
  select(CR)
CR_weekday <- CR_testing %>%
  filter(weekend ==FALSE) %>%
  select(CR)
t.test(CR_weekend$CR, CR_weekday$CR)

