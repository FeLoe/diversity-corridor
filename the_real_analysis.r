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
### This serves a more explorative goal. 
ds = d %>% 
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), min_position=min(position), selected=sum(selected)) %>%
  ungroup() %>%
  filter(long_window_seen >= 5 & short_window_seen >= 1)

optim = control=glmerControl(optimizer="bobyqa",
  optCtrl=list(maxfun=2e5))

m = list()
for (topic in unique(ds$topic)) {
  message(topic)
  m[[topic]] = glmer(selected ~ position + long_selected + short_selected + topic_pref + topic_in_exp + (1 | user_id), 
    data=ds[ds$topic == topic,], family=binomial, control=optim)
}

pooled = glmer(selected ~ position + short_selected + long_selected + topic_pref + topic_in_exp + (1 | user_id) + (1 | topic), 
  data=ds, family=binomial, control=optim)

#tab_model(m, file = 'somewhere/per_topic.html', dv.labels = names(m), show.ci = F, p.style = 'stars', show.re.var=F)
#tab_model(pooled, file = 'somewhere/pooled.html', dv.labels ='Pooled', show.ci = F, p.style = 'stars', show.re.var=F)
#tab_model(c(m, pooled), file = 'somewhere/combined.html', dv.labels =c(names(m), 'Pooled'), show.ci = F, p.style = 'stars', show.re.var=F)

tab_model(c(m, pooled), dv.labels =c(names(m), 'Pooled'), show.aic=T, show.r2 = F, show.ci = F, p.style = 'stars', show.re.var=T)
