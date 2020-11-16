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
### might be good to just use multiple models to showcase that you can do
### different things with this data

### What predicts whether a topic is selected when it is an option?
### This serves a more explorative goal. 
ds = d %>% 
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), selected=sum(selected)) %>%
  ungroup() %>%
  filter(long_window_n >=5 & short_window_n >= 1)

## use alternative optimizer
optim = control=glmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e5))

m = list()
for (topic in unique(ds$topic)) {
  message(topic)
  m[[topic]] = glmer(selected ~ position + long_selected + short_selected + topic_pref + topic_in_exp + (1 | user_id), 
                     data=ds[ds$topic == topic,], family=binomial, control=optim)
}
tab_model(m, dv.labels = names(m), show.ci = F, p.style = 'stars')

m = list()
for (topic in unique(ds$topic)) {
  message(topic)
  m[[topic]] = glmer(selected ~ position + long_selected + short_selected + topic_pref + topic_in_exp + 
                       exp_group + exp_group*topic_pref + (1 | user_id), 
                     data=ds[ds$topic == topic,], family=binomial, control=optim)
}
tab_model(m, dv.labels = names(m), show.ci = F, p.style = 'stars')

m = list()
for (topic in unique(ds$topic)) {
  message(topic)
  m[[topic]] = glmer(selected ~ position + long_selected + short_selected + topic_pref + topic_in_exp + 
                       exp_group + exp_group*long_selected + (1 | user_id), 
                     data=ds[ds$topic == topic,], family=binomial, control=optim)
}
tab_model(m, dv.labels = names(m), show.ci = F, p.style = 'stars')


#### Binnenland and Buitenland are predicted by previous choices in the past three days
#### but not by self-reported topic preference
#### the reverse is true for Sport and Economie
#### does this indicate that self-reported preference on these topics is more accurate?
#### or maybe binnenland buitenland is just more volatile over time?

######## click towards diversity 

## If we focus on exposures as cases, we'll need some exposure level variables
## we can measure the diversity of a choice as the cosine similarity of a choice and the topic preference
## similarly, we can measure whether an exposure matches a persons 
## perhaps also add entropy of an exposure as an indication of how diverse the selection was overall
## we can also add entropy of a person as a measure of whether he/she has a strong topic preference (there was something about this in the paper)

cossim <- function(x,y) sum(x*y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
#cossim <- function(x,y) sum(x*y)
entropy <- function(x) -sum(x * log2(x))

interpolated_window = d %>% 
  filter(!topic == 'Anders') %>%
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), selected=sum(selected)) %>%
  ungroup() %>%
  select(user_id, exposure_id, topic, long_selected) %>%
  pivot_wider(names_from = topic, values_from = long_selected) %>%
  mutate(across(Binnenland:Justitie, na.locf, na.rm=F)) %>%
  mutate(across(Binnenland:Justitie, function(x) ifelse(is.na(x), 0, x))) %>%
  ungroup() %>%
  pivot_longer(Binnenland:Justitie, names_to='topic', values_to='window_selected')

exposure_vec = d %>%
  arrange(user_id, exposure_id, -position) %>%
  distinct(user_id, exposure_id, topic, .keep_all = T) %>%
  select(user_id, exposure_id, topic, topic_in_exp) %>%
  pivot_wider(names_from = topic, values_from=topic_in_exp) %>%
  pivot_longer(-c(user_id,exposure_id), names_to='topic', values_to='topic_in_exp') %>%
  mutate(topic_in_exp = ifelse(is.na(topic_in_exp), 0, topic_in_exp)) %>%
  left_join(distinct(ungroup(d), user_id, topic, topic_pref)) %>%
  left_join(distinct(ungroup(d), user_id, exposure_id, topic, selected)) %>% 
  left_join(interpolated_window) %>%
  group_by(user_id, exposure_id) %>%
  summarize(sim_exp_window = cossim(topic_in_exp, window_selected),
            sim_select_window = cossim(ifelse(is.na(selected) | selected == 0, 0, 1), window_selected),
            sim_exp_tp = cossim(topic_in_exp, topic_pref),
            sim_select_tp = cossim(ifelse(is.na(selected) | selected == 0, 0, 1), topic_pref),
            sim_windowXpref = cossim(topic_pref, window_selected),
            window_entropy = entropy(window_selected))


dss = d %>%
  left_join(exposure_vec) %>%
  filter(selected==T & long_window_n >= 10 & short_window_n >= 1) %>%
  mutate(ctd_pref = 1 - sim_select_tp,
         ctd_window = 1 - sim_select_window)


dss = dss %>%
  mutate(exp_group = recode(exp_group,
                            `group 1` = "1: random",
                            `group 2` = "2: recommender A",
                            `group 3` = '2: recommender B',
                            `group 4` = "3: custom")) %>%
  group_by(user_id) %>%
  mutate(exposure_i = 1:n()) %>%
  mutate(exposure_i = exposure_i - mean(exposure_i))

m1 = lmer(ctd_pref ~ position + sim_exp_tp + (1 | user_id), data=dss)
m2 = lmer(ctd_pref ~ position + sim_exp_tp + exp_group + (1 | user_id), data=dss)
m3 = lmer(ctd_window ~ position + sim_exp_window + (1 | user_id), data=dss)
m4 = lmer(ctd_window ~ position + sim_exp_window + exp_group + (1 | user_id), data=dss)
anova(m1,m2)
anova(m3,m4)
tab_model(m1,m2,m3,m4, show.ci=F, p.style = 'stars')
## for some reason the effect of rec B REVERSES when the dot product is used
## instead of cosine similarity... This makes me uncomfortable

## what if position is dv?
## basically, when do people keep looking?
m1 = glmer(position ~ sim_exp_tp + sim_exp_window + sim_select_window + sim_select_tp + exp_group + (1 | user_id), data=dss, family=poisson)
tab_model(m1)


## are people happier if the exposure is similar to their preference?
dss = dss %>%
  arrange(user_id,exposure_id) %>%
  group_by(user_id) %>%
  mutate(lag_rating = lag(rating_selected), lag_rating2 = lag(rating2_selected))

m_self = lmer(rating_selected ~ lag_rating + position + sim_select_window + sim_select_tp +  (1 | topic) + (1 | user_id), data=dss)
m_soc = lmer(rating2_selected ~ lag_rating2 + position +  sim_select_window + sim_select_tp +  (1 | topic) + (1 | user_id), data=dss)
tab_model(m_self, m_soc, show.ci=F, p.style = 'stars')



## this probably just mainly shows the problem that window entropy will generally increase due to having more exposures
## (now added condition that only max 10 most recent articles are used. Does this help?)
rep_versus_obs_pref = d %>%
  filter(long_window_n >= 5) %>%
  left_join(exposure_vec) %>%
  ungroup() %>%
  distinct(user_id, exposure_id, sim_windowXpref, window_entropy) %>%
  group_by(user_id) %>%
  mutate(exposure_i = 1:n()) 

m1 = lmer(sim_windowXpref ~ exposure_i + (1|user_id), data=rep_versus_obs_pref)
m2 = lmer(window_entropy ~ exposure_i + (1|user_id), data=rep_versus_obs_pref)
tab_model(m1,m2)

cor.test(rep_versus_obs_pref$exposure_i, rep_versus_obs_pref$window_entropy)

### descriptives

ds = d %>% 
  filter(!topic == 'Anders') %>%
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), selected=sum(selected)) %>%
  ungroup()

dw = ds %>%
  select(user_id, exposure_id, topic, topic_pref, long_window_n, long_selected) %>%
  pivot_wider(names_from = topic, values_from = long_selected) %>%
  mutate(across(Binnenland:Justitie, na.locf, na.rm=F)) %>%
  mutate(across(Binnenland:Justitie, function(x) ifelse(is.na(x), 0, x)))

dl = dw %>%
  group_by(user_id) %>%
  mutate(exposure_i = 1:n()) %>%
  ungroup() %>%
  pivot_longer(Binnenland:Justitie, names_to='topic', values_to='prob')

m = dl %>%
  filter(long_window_n > 5) %>%
  lmer(formula = prob ~ topic_pref + exposure_i + topic_pref*exposure_i + (1 | user_id) + (1 | topic))

tab_model(m)
  
dl %>%
  filter(long_window_n > 5, topic == 'Binnenland', user_id %in% head(unique(user_id))) %>%
  ggplot() +
    geom_line(aes(x=exposure_i, y=prob, color=as.factor(user_id)))


## visualize for a user the topic choice over time (line per topic)
## does this show clusters?
  
## issue in measuring observed topic preference
## we can't simply look at topic frequencies, because this depends on exposures
## but we also can't just look at probability of selected a topic when exposured
## e.g. if a person likes sport, and sport occurs very rarely, he/she might click on it every time



############## ANALYSIS 




topic_pref = d %>%
  distinct(user_id, topic, topic_pref, real_topic_pref) %>%
  filter(topic != 'Wetenschap')

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



############ click towards diversity





dss = filter(d, selected==T & long_window_n > 5 & short_window_n > 1)
dss = mutate(dss, ctd = 1 - topic_pref_dist)



dss = dss %>%
  group_by(user_id) %>%
  mutate(exposure_i = 1:n()) %>%
  group_by(user_id, session) %>%
  mutate(session_i = 1:n())
mb = lmer(ctd ~ 1 + (1 | user_id), data=dss)
m = lmer(ctd ~ position + long_selected + short_selected + exposure_i + session_i + topic_in_exp + rating_selected + (1 | topic) + (1 | user_id), data=dss)
tab_model(mb,m)

m = lmer(ctd ~ position + lag_1_selected + exposure_i + session_i + selected_exp + rating_selected + (1 | user_id), data=dss)
tab_model(m)
dss


