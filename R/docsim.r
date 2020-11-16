library(tidyverse)
library(RNewsflow)
library(data.table)
library(quanteda)


f = read_csv('data/tmp/article_features.csv')
e = read_csv('data/tmp/exposures.csv')

## preprocess texts
corp = corpus(f, docid_field = 'news_id', text_field = 'text')
dtm = dfm(corp, stem=T, tolower=T, remove_punct=T, remove=stopwords('nl'))
#dtm = dfm_tfidf(dtm)

e$uid = 1:nrow(e)

## get lag document similarity
exposures = e %>%
  select(user_id, timestamp, news_id, selected, exposure_id, uid) %>%
  mutate(date = as.POSIXct(timestamp))

# dtm of all exposures per user 
dtm_exp = dtm[match(exposures$news_id, docnames(dtm)),]
docvars(dtm_exp, 'date') = exposures$date
docvars(dtm_exp, 'user_id') = exposures$user_id
docnames(dtm_exp) = exposures$uid

# dtm of only the seen articles
f = exposures$selected == 1
dtm_seen = dtm[match(exposures$news_id[f], docnames(dtm)),]
docvars(dtm_seen, 'date') = exposures$date[f]
docvars(dtm_seen, 'user_id') = exposures$user_id[f]
docnames(dtm_seen) = exposures$uid[f]


# how similar was each document exposed to the previous documents the user had seen?
# once of long term (window = 3 days to 15 min) and once for short (window = 15 min to now)
window_sim = compare_documents(dtm_exp, dtm_seen, date_var = 'date', group_var = 'user_id',
                              hour_window = c(-3*24,-0.2501), copy_meta=T)
e$window_sim = NA
agg = window_sim$d[,list(window_sim = mean(weight)), by='from']
e$window_sim[as.numeric(agg$from)] = agg$window_sim

lag_sim = compare_documents(dtm_exp, dtm_seen, date_var = 'date', group_var = 'user_id',
                               hour_window = c(-0.25,0), copy_meta=T)
e$lag_sim = NA
lag_sim$d = lag_sim$d[lag_sim$d$hourdiff < 0,]
agg = lag_sim$d[,list(lag_sim = max(weight)), by='from'] ## !!!! note that this uses max()!!!
e$lag_sim[as.numeric(agg$from)] = agg$lag_sim



#### ANALYSIS

library(lme4)
library(sjPlot)

u = read_csv('data/tmp/users_new.csv') %>%
  select(user_id, exp_group=group, gender, age, edu, pol_knowledge, overconfidence) %>%
  mutate(exp_group = as.factor(paste('group', exp_group)))

d = e %>%
  dplyr::filter(!is.na(window_sim) & !is.na(lag_sim)) %>%
  group_by(user_id, exposure_id) %>%
  mutate(window_div = 10-order(window_sim), lag_div = 10-order(lag_sim)) %>%
  mutate(window_sim_md = window_sim - mean(window_sim), lag_sim_md = lag_sim - mean(lag_sim)) %>%
  left_join(u)


m1 = glmer(selected ~ window_sim + lag_sim + position + (1 | user_id), data=d, family=binomial('logit'))
m2 = glmer(selected ~ window_sim + lag_sim + position + age*window_sim + age*lag_sim + (1 | user_id), data=d, family=binomial('logit'))
tab_model(m1)


m2 = glmer(selected ~ window_div + lag_div + position + (1 | user_id), data=d, family=binomial('logit'))
tab_model(m2)
p = plot_model(m2, type='pred', show.data = T)
plot_grid(p)

m3 = glmer(selected ~ window_sim_md + lag_sim_md + position + (1 | user_id), data=d, family=binomial('logit'))
tab_model(m3)
p = plot_model(m3, type='pred', show.data = T)
plot_grid(p)


###########


d = e %>%
  dplyr::filter(!is.na(window_sim)) %>%
  dplyr::filter(!is.na(lag_sim)) %>%
  group_by(user_id, exposure_id) %>%
  mutate(window_corridor = mean(window_sim), lag_corridor=mean(lag_sim)) %>%
  mutate(window_div = 10-order(window_sim), lag_div = 10-order(lag_sim)) %>%
  mutate(window_sim_md = window_sim - mean(window_sim), lag_sim_md = lag_sim - mean(lag_sim))

ds = d %>%
  dplyr::filter(selected == 1) %>%
  arrange(user_id, exposure_id) %>%
  group_by(user_id, session) %>%
  mutate(session_i = 1:n()) %>%
  left_join(u)


table(ds$topic_selected)

plot(table(ds$window_div[ds$position == 0]), type='l')
lines(table(ds$window_div[ds$position == 8]), type='l', lty=2)
legend('topright', legend=c('position = 1', 'position = 9'), lty=1:2)
plot(table(ds$lag_div[ds$position == 0]), type='l')
lines(table(ds$lag_div[ds$position == 8]), type='l', lty=2)
legend('topright', legend=c('position = 1', 'position = 9'), lty=1:2)

m1 = lmer(window_sim_md ~ position + session_i + window_corridor + lag_corridor + (1 | user_id), data=ds)
m2 = lmer(lag_sim_md ~ position + session_i + window_corridor + lag_corridor + (1 | user_id), data=ds)
tab_model(m1,m2)


hist(ds$window_sim_md)
hist(ds$window_div)
m1 = lmer(window_sim_md ~ position + session_i + window_corridor + lag_corridor + (1 | user_id), data=ds)
m2 = lmer(window_sim_md ~ position + session_i + window_corridor + lag_corridor + exp_group + age + edu + pol_knowledge + overconfidence + (1 | user_id), data=ds)
#m3 = lmer(lag_sim_md ~ position + session_i + (1 | user_id), data=ds)
tab_model(m1,m2)


hist(ds$lag_sim_md)
hist(ds$lag_div)
m1 = lmer(lag_sim_md ~ position + session_i + (1 | user_id), data=ds)
m2 = lmer(lag_sim_md ~ position + session_i + exp_group + age + edu + pol_knowledge + overconfidence + (1 | user_id), data=ds)
#m3 = lmer(lag_sim_md ~ position + session_i + (1 | user_id), data=ds)
tab_model(m1,m2)

hist(ds$lag_sim_md)

plot_model(m4, type='re')
hist(ranef(m4)$user_id[[1]])
mean(ds$window_sim_md)
hist(ds$window_sim)
p = plot_model(m4, type='pred', show.data = T)
plot_grid(p)
