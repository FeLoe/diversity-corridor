library(tidyverse)
library(lme4)
library(sjPlot)
library(runner)

e = read_csv('data/tmp/exposures.csv')
f = read_csv('data/tmp/article_features.csv')


d = f %>%
  select(news_id, topic) %>%
  inner_join(e) %>%
  select(user_id, exposure_id, session, timestamp, topic, position,
         selected, majority, rating_selected, rating2_selected) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  arrange(user_id, timestamp)

## add lag_selected (i.e. how often was topic selected in past x hours). 
hours_window = 24*7
d = d %>%
  group_by(user_id, topic) %>%
  mutate(lag_selected = runner(selected, f=sum, k=paste(hours_window, 'hours'), lag='1 sec', idx=timestamp))

## add lag_n (i.e. how many selectiions made in past x hours)
d = d %>%
  group_by(user_id, exposure_id) %>%
  mutate(lag_n = sum(lag_selected[!duplicated(topic)]))

## add lag_pct (note that this is NaN if lag_n == 0. We should drop these cases in analysis)
d = mutate(d, lag_pct = lag_selected / lag_n)

## add selected_exp (i.e. times selected topic occurs in current exposure)
d = d %>%
  group_by(user_id, exposure_id, topic) %>%
  mutate(selected_exp = n())


############## ANALYSIS 

m = glmer(selected ~ lag_pct + selected_exp + (1 | topic) + (1 | user_id), data=d[d$lag_n > 0,], family=binomial('logit'))

tab_model(m)
