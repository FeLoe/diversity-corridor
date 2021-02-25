library(ggplot2)
library(Rmisc)
library(psych)
library(tidyverse)
library(ggstatsplot)
library(runner)

e = read_csv('data/tmp/exposures.csv')
f = read_csv('data/tmp/article_features.csv')
u = read_csv('data/tmp/users_new.csv')

u = read_csv('data/tmp/users_new.csv') %>%
  select(user_id, exp_group=group, Diversity_1:Diversity_4) %>%
  mutate(exp_group = as.factor(paste('group', exp_group)))

d = f %>%
  select(news_id, topic) %>%
  inner_join(e) %>%
  select(user_id, session, exposure_id, timestamp, topic, position, 
         selected, devices_group) %>%
  mutate(timestamp = as.POSIXct(timestamp)) %>%
  arrange(user_id, timestamp)

d = left_join(d, u) %>%
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
#--------------------------------------TOPICS PRESENTED AND CHOSEN-------------------------------------------------------
#Topics presented
table(d$topic)
#Topics chosen
sel = d %>% filter(selected == '1')
table(sel$topic)

#Filter out small topics
d = d %>% filter(!topic %in% c("Anders", "Immigratie", "Milieu", "Wetenschap"))

#--------------------------------------EXPOSURES, SESSIONS, LENGTH OF SESSIONS-------------------------------------------
#How many exposures,how many sessions, length of sessions
exposures = d %>% 
  filter(selected == 1) %>% 
  group_by(user_id, exp_group, exposure_id) %>%  
  summarize(n = n()) %>% 
  group_by(user_id, exp_group) %>% 
  summarize(exp = n())
sessions = d %>% 
  filter(selected == 1) %>% 
  group_by(user_id,exp_group, session) %>%
  summarize(n = n()) %>% 
  group_by(user_id, exp_group) %>% 
  summarize(sess = n())
exp_sess = left_join(exposures, sessions)
exp_sess$len = exp_sess$exp/exp_sess$sess
describe(exp_sess)

#Length of sessions over time? 
overtime = d %>% 
  filter(selected == 1) %>%
  group_by(user_id,exp_group, session) %>%
  summarize(n = n()) %>%
  group_by(session) %>%
  summarize(sess_length = mean(n))
describe(overtime)
#ggplot(overtime, mapping = aes(x = session, y = sess_length)) + geom_point() + geom_smooth()

#--------------------------------------GROUP DIFFERENCES EXPOSURES, SESSIONS----------------------------------------------
describeBy(exp_sess,exp_sess$exp_group)
exp_sess = exp_sess %>% ungroup()

p1 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = sess,
  type = "np",
  title = "Differences in number of sessions between groups", 
  xlab = "Experimental Groups", 
  ylab = "Sessions"
)

p2 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = exp,
  type = "np",
  title = "Differences in number of selections between groups", 
  xlab = "Experimental Groups", 
  ylab = "Selections"
)

p3 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = len,
  type = "np",
  title = "Differences in length of sessions between groups", 
  xlab = "Experimental Groups", 
  ylab = "Length"
)

#multiplot(p2, p1, cols = 2)

#-------------------------------------------------DIVERSITY PER GROUP------------------------------------------------------
#Excluding the small topics & Other
entropy = function(x) {p=x/sum(x); -sum(ifelse(p==0, 0, p*log(p, 2)))}
dpu1 = d %>% filter(selected==1) %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(diversity=entropy(n))
dpu2 = d %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(expdiversity=entropy(n)) 
dpu = dpu1 %>% inner_join(dpu2) %>% inner_join(u)
describeBy(dpu,dpu$exp_group)
dpu = dpu %>% ungroup()

p4 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = diversity,
  type = "np",
  title = "Differences in consumption diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Consumption Diversity"
)

p5 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = expdiversity,
  type = "np",
  title = "Differences in exposure diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Exposure Diversity"
)

dpu = dpu %>% mutate(perc_div = (Diversity_1 + Diversity_2 + Diversity_3 +  Diversity_4)/4)

p6 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = perc_div,
  type = "np",
  title = "Differences in perceived diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Perceived Diversity"
)
#--------------------------------------PERCEIVED VS ACTUAL DIVERSITY------------------------------------------------------
cor.test(dpu$expdiversity, dpu$diversity, 
         method = "pearson")
cor.test(dpu$expdiversity, dpu$perc_div, 
         method = "pearson")
cor.test(dpu$diversity, dpu$perc_div, 
         method = "pearson")


#--------------------------------------BASELINE: SURVEY PREFERENCES, LONG WINDOW------------------------------------------
b = readRDS('data/intermediate/analysis_backup.rds')
b = b %>% filter(user_id != 637)
#Description topic preferences
topic_prefs = b %>% ungroup() %>% distinct(user_id, topic, topic_pref, topic_pref_dist) 
describeBy(topic_prefs_test, topic_prefs_test$topic)
topic_prefs_s = topic_prefs %>% group_by(topic, topic_pref) %>% summarise(n = n())
ggplot(topic_prefs_s, aes(x = topic_pref, y = n)) + geom_bar(stat = "identity") + facet_wrap(~topic) 

#Description corr topic preference & topics selected for different topics
pref_sel = b %>% ungroup() %>% 
  filter(selected == 1) %>% 
  group_by(user_id, topic) %>% 
  summarise(n = n()) %>%
  left_join(., topic_prefs)

pref_sel %>%
  group_by(topic) %>%
  summarize(coeff = cor.test(n,topic_pref)$estimate, 
                   p = cor.test(n, topic_pref)$p.value, par = cor.test(n, topic_pref)$parameter)


#--------------------------------------POSITIONING EFFECTS---------------------------------------------------------------
b = left_join(b, u)

#In general 
sel = b %>% 
  group_by(position, selected) %>%
  summarize(n = n()) %>%
  group_by(position) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1)

#Per topic
sel = b %>% 
  group_by(topic, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, topic) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1)

labels_topics <- c("Binnenland" = "Domestic news", "Buitenland" = "Foreign news", "Economie" = "Economy", "Entertainment" = "Entertainment", 
                   "Justitie" = "Crime", "Sport" = "Sports")

p7 = ggplot(sel, aes(y = p, x = position)) + 
  geom_line(aes(linetype = topic)) + 
  labs(x = "Position", y = "Proportion selected", linetype = "Topic") +
  scale_x_continuous(breaks = c(0:8)) + 
  scale_linetype(labels = c("Domestic", "Foreign", "Economy", "Entertainment", "Crime", "Sports")) +
  theme_minimal()

#Per preference
sel_pref = b %>%
  group_by(topic_pref, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, topic_pref) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1)

p8 = ggplot(sel_pref, aes(y = p, x = position)) + 
  geom_line(aes(linetype = factor(topic_pref))) + 
  labs(x = "Position", y = "Proportion selected", linetype = "Topic interest") +
  scale_x_continuous(breaks = c(0:8)) + 
  scale_linetype(labels = c("Not interested", '2','3', '4', '5', '6', 'Very interested')) +
  theme_bw()

#Per device used
sel = b %>% 
  group_by(devices_group, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, devices_group) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1)

p9 = ggplot(sel, aes(y = p, x = position)) + 
  geom_line(aes(linetype = factor(devices_group))) + 
  labs(x = "Position", y = "Proportion selected", linetype = "Device") +
  scale_x_continuous(breaks = c(0:8)) + 
  theme_bw()

#multiplot(p7, p8, p9, cols = 2)

