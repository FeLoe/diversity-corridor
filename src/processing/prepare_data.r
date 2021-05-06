library(tidyverse)
library(runner)

e = read_csv('data/intermediate/exposures.csv')
f = read_csv('data/intermediate/article_features.csv')
u = read_csv('data/intermediate/users_new.csv')

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
                        t_mil = 'Anders',
                        t_imm = 'Anders',
                        t_other = 'Anders')) %>%
  group_by(user_id, topic) %>%
  summarise(topic_pref = mean(topic_pref))

u1 = u %>%
  select(user_id, exp_group=group, Diversity_1:Diversity_4, div_trait, gender, age, edu, pol_knowledge, 
         overconfidence, political_efficacy, pol_interest) %>%
  mutate(exp_group = as.factor(paste('group', exp_group)))

d = f %>%
  select(news_id, topic) %>%
  inner_join(e) %>%
  select(user_id, session, exposure_id, timestamp, topic, position, 
         selected, rating_selected, rating2_selected, devices_group) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  arrange(user_id, timestamp) %>%
  left_join(tp)
  
d = left_join(d, u1) %>%
  mutate(exp_group = recode(exp_group,
                            `group 1` = "random",
                            `group 2` = "rec_A",
                            `group 3` = 'rec_B',
                            `group 4` = "custom"),
         device = recode(devices_group, 
                         `mobile` = 'mobile', 
                         `tablet` = 'PC', 
                         `PC` = "PC")) %>% 
  filter(user_id != 637) #Excluding one extreme user with 1232 selections

## set long and short window (in hours)
## the long window will not include the short window
long_window = 3*24
short_window = 1
max_n = Inf  ## include only n last articles within window
sum_selected <- function(x, n=max_n) sum(tail(x, n))   
count_seen <- function(x, n=max_n) min(length(unique(x)), n)

## add lag_selected (i.e. how often was topic selected in past x hours). 
d = d %>%
  group_by(user_id, topic) %>%
  mutate(long_window_selected = runner(selected, f=sum_selected, k=paste(long_window, 'hours'), lag=paste(short_window, 'hours'), idx=timestamp)) %>% 
  mutate(long_window_seen = runner(exposure_id, f=count_seen, k=paste(long_window, 'hours'), lag=paste(short_window, 'hours'), idx=timestamp)) %>% 
  mutate(short_window_selected = runner(selected, f=sum_selected, k=paste(short_window, 'hours'), lag='1 sec', idx=timestamp)) %>%
  mutate(short_window_seen = runner(exposure_id, f=count_seen, k=paste(short_window, 'hours'), lag='1 sec', idx=timestamp)) 

d = d %>%
  group_by(user_id) %>%
  mutate(long_window_n = runner(selected, f=sum, k=paste(long_window, 'hours'), lag=paste(short_window, 'hours'), idx=timestamp)) %>% 
  mutate(short_window_n = runner(selected, f=sum, k=paste(short_window, 'hours'), lag='1 sec', idx=timestamp))
  
## add lag_window (note that this is NaN if lag_n == 0. We should drop these cases in analysis)
d = d %>%
  mutate(long_selected = long_window_selected / long_window_seen,
         long_seen = long_window_seen / long_window_n,
         short_selected = short_window_selected / short_window_seen,
         short_seen = short_window_seen / short_window_n) %>%
  mutate(full_selected = (long_window_selected + short_window_selected) / (long_window_seen + short_window_seen))

## add topic_in_exp (i.e. times selected topic occurs in current exposure)
d = d %>%
  group_by(user_id, exposure_id, topic) %>%
  mutate(topic_in_exp = n())


## make topic_pref (from survey) a multinom dist
topic_pref_dist = d %>%
  ungroup() %>%
  distinct(user_id, topic, topic_pref) %>%
  group_by(user_id) %>%
  mutate(topic_pref_dist = topic_pref / sum(topic_pref, na.rm=T))

d = d %>%
  left_join(topic_pref_dist)

saveRDS(d, 'data/intermediate/analysis_backup.rds')
