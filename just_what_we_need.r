library(lme4)
library(sjPlot)
library(tidyverse)
library(zoo)

d = readRDS('data/intermediate/analysis_backup.rds')

u = read_csv('data/tmp/users_new.csv') %>%
  select(user_id, exp_group=group, gender, age, edu, pol_knowledge, overconfidence) %>%
  mutate(exp_group = as.factor(paste('group', exp_group)))

d = left_join(d, u) %>%
  mutate(exp_group = recode(exp_group,
                            `group 1` = "1: random",
                            `group 2` = "2: recommender A",
                            `group 3` = '2: recommender B',
                            `group 4` = "3: custom"))


### What predicts whether a topic is selected when it is an option?
ds = d %>% 
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), selected=sum(selected)) %>%
  ungroup() %>%
  filter(long_window_n >=5 & short_window_n >= 1)


## use alternative optimizer
optim = control=glmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e5))


lm1a = glmer(selected ~ position + short_selected + long_selected + topic_in_exp + (1 | user_id) + (1 | topic), 
             data=ds, family=binomial, control=optim)
lm1b = glmer(selected ~ position + short_selected + topic_pref + topic_in_exp + (1 | user_id) + (1 | topic), 
             data=ds, family=binomial, control=optim)
lm2 = glmer(selected ~ position + short_selected + long_selected + topic_pref + topic_in_exp + (1 | user_id) + (1 | topic), 
            data=ds, family=binomial, control=optim)
tab_model(lm1a, lm1b, lm2, show.ci = F, p.style = 'stars')
