library(tidyverse)
library(lme4)
library(sjPlot)
library(runner)

e = read_csv('data/tmp/exposures.csv')
f = read_csv('data/tmp/article_features.csv')
u = read_csv('data/tmp/users_new.csv')

tp = u %>%
  select(user_id, starts_with('t_')) %>%
  mutate(t_other = 8 - (t_buit + t_bin + t_econ + t_crime + t_sport + t_ent + t_mil + t_imm) / 8) %>%
  pivot_longer(-user_id, names_to = 'topic', values_to='topic_pref') %>%
  mutate(topic = recode(topic, 
                       t_econ = 'Economie',
                       t_buit = 'Buitenland',
                       t_bin = 'Binnenland',
                       t_crime = 'Justitie',
                       t_sport = 'Sport',
                       t_ent = 'Entertainment',
                       t_mil = 'Milieu',
                       t_imm = 'Immigratie',
                       t_other = 'Anders')) 

d = f %>%
  select(news_id, topic) %>%
  inner_join(e) %>%
  select(user_id, session, exposure_id, timestamp, topic, position, 
         selected, majority, rating_selected, rating2_selected) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  arrange(user_id, timestamp) %>%
  left_join(tp)

#saveRDS(d, file = 'data/tmp/analysis_backup.rds')

##
d = readRDS('data/tmp/analysis_backup.rds')

## add lag_selected (i.e. how often was topic selected in past x hours). 
hours_window = 24*7
d = d %>%
  group_by(user_id, topic) %>%
  mutate(lag_selected = runner(selected, f=sum, k=paste(hours_window, 'hours'), lag='1 sec', idx=timestamp))

## add lag_n (i.e. how many selectiions made in past x hours)
d = d %>%
  group_by(user_id, exposure_id) %>%
  mutate(lag_n = sum(lag_selected[!duplicated(topic)]))

## add lag_window (note that this is NaN if lag_n == 0. We should drop these cases in analysis)
d = mutate(d, lag_window = lag_selected / lag_n)


## add lag_1_selected (i.e. only previous topic)
lag_topic = d %>%
  filter(selected == 1) %>%
  arrange(user_id, exposure_id) %>%
  group_by(user_id) %>%
  mutate(lag_topic = lag(topic, n = 1)) %>%
  select(user_id, exposure_id, lag_topic)

d = d %>%
  left_join(lag_topic) %>%
  mutate(lag_1_selected = as.numeric(topic == lag_topic))


## add lag_session (i.e. how many times was topic selected before in current session)
d = d %>%
  group_by(user_id, session, topic) %>%
  arrange(user_id, session, exposure_id, -selected) %>%
  mutate(lag_session = cumsum(selected)) 
  
exp_id_topic = d %>%
  filter(selected==T) %>%
  select(user_id, exposure_id, selected_topic=topic)

d = d %>%
  left_join(exp_id_topic) %>%
  mutate(lag_session = lag_session - as.numeric(topic == selected_topic))
  

## add selected_exp (i.e. times selected topic occurs in current exposure)
d = d %>%
  group_by(user_id, exposure_id, topic) %>%
  mutate(selected_exp = n())


## add real_topic_pref
ref_size = 0.5

d = d %>%
  group_by(user_id) %>%
  mutate(reference = 1:n() < floor(n()*ref_size))

d_ref = filter(d, reference)
d = filter(d, !reference)

real_topic_pref = d_ref %>%
  arrange(user_id, exposure_id, -selected) %>%
  distinct(user_id, exposure_id, topic, .keep_all = T) %>%
  group_by(user_id, topic) %>%
  summarize(topic_n = mean(selected)) %>%
  mutate(topic_pct = topic_n / sum(topic_n)) %>%
  select(user_id, topic, real_topic_pref = topic_pct)

d = d %>%
  left_join(real_topic_pref) %>%
  mutate(real_topic_pref = ifelse(is.na(real_topic_pref), 0, real_topic_pref))
  
#topic_pref = d %>%
#  distinct(user_id, topic, topic_pref, real_topic_pref) %>%
#  filter(topic != 'Wetenschap')

#topic_pref_agg = topic_pref %>%
#  group_by(user_id) %>%
#  summarize(cor = cor(topic_pref, real_topic_pref))

#hist(topic_pref_agg$cor, breaks = 20)
#topic_pref_agg %>%
#  arrange(cor)

#table(d_ref[d_ref$user_id == 586 & d_ref$selected == 1,]$topic)
#topic_pref[topic_pref$user_id == 586,]

## sidenote: real_topic_pref might simply be a better predictor of choice because it 
## it is itself influenced by the actual distribution of topics that people see in the experiments


## make topic_pref (from survey) a multinom dist
topic_pref = d %>%
  distinct(user_id, topic, topic_pref) %>%
  group_by(user_id) %>%
  mutate(topic_pref = topic_pref / sum(topic_pref, na.rm=T))
  
d = d %>%
  mutate(topic_pref = NULL) %>%
  left_join(topic_pref)
  


############## ANALYSIS 

## rm duplicate solution (bad solution)
#ds = d %>% 
#  arrange(-selected, position) %>%
#  distinct(user_id, exposure_id, topic, .keep_all = T) %>%
#  arrange(user_id, exposure_id, topic) 

## aggregate solution (mediocre solution)
ds = d %>% 
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), selected=sum(selected))

ds = filter(ds, lag_n > 5 & topic != 'Wetenschap')


m = glmer(selected ~ lag_1_selected + lag_session + position + selected_exp + topic_pref + real_topic_pref + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
tab_model(m)


m1 = glmer(selected ~ position + selected_exp + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
m2 = glmer(selected ~ lag_1_selected + position + selected_exp + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
m3 = glmer(selected ~ lag_1_selected + lag_window + position + selected_exp + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
m4 = glmer(selected ~ lag_1_selected + lag_window + position + selected_exp + topic_pref + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
m5 = glmer(selected ~ lag_1_selected + lag_window + position + selected_exp + topic_pref + real_topic_pref + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))

anova(m1,m2,m3,m4,m5)
tab_model(m1,m2,m3,m4,m5)

m6 = glmer(selected ~ lag_1_selected + lag_window + position + selected_exp + topic_pref + lag_1_selected * topic_pref + (1 | topic) + (1 | user_id), data=ds, family=binomial('logit'))
tab_model(m6)
