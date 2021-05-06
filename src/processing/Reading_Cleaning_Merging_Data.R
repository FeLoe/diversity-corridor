library(tidyverse)
library(lubridate)
library(psych)
library(here)

#Reading in all the different data files (originally retrieved from SQL database)
users = read_csv(here::here("data/raw/users.csv"))
selected_news = read_csv(here::here("data/raw/selected_news.csv"))
diversity = read_csv(here::here("data/raw/diversity.csv"))
num_rec = read_csv(here::here("data/raw/num_rec.csv"))
category = read_csv(here::here("data/raw/category.csv"))
show_again = read_csv(here::here("data/raw/show_again.csv"))
logins = read_csv(here::here("data/raw/logins.csv"))
topics = read_csv(here::here("data/raw/topics_final.csv"))
#Note: Next two datasets are not on Github (size and/or copyright limitations. Available upon request)
elasticsearch_info = read_csv(here::here("data/raw-private/elasticsearch_info.csv"))
news = read_csv(here::here("data/raw-private/news.csv"))


#Getting the qualtrics questionnaires, removing duplicate IDs in each questionnaire, merging them on the IDs
headers_first = read_csv(here::here("data/raw/Questionnaires/3bij3_first.csv"), col_names = FALSE, n_max = 1)
headers_second = read_csv(here::here("data/raw/Questionnaires/3bij3_second.csv"), col_names = FALSE, n_max = 1)
headers_third = read_csv(here::here("data/raw/Questionnaires/3bij3_third.csv"), col_names = FALSE, n_max = 1)
qualtrics_first = read_csv(here::here("data/raw/Questionnaires/3bij3_first.csv"), skip = 3, col_names = FALSE)
qualtrics_second = read_csv(here::here("data/raw/Questionnaires/3bij3_second.csv"), skip = 3, col_names = FALSE)
qualtrics_third = read_csv(here::here("data/raw/Questionnaires/3bij3_third.csv"), skip = 3, col_names = FALSE)
colnames(qualtrics_first)= headers_first
colnames(qualtrics_second)= headers_second
colnames(qualtrics_third)= headers_third
temp = list(qualtrics_first, qualtrics_second, qualtrics_third)
#Filter to (1) only include finished ones (2) remove duplicates based on id and keep the first one (first time someone filled in the questionnaire in case it was filled in twice)
temp = map(temp, ~ (.x %>% 
                      select(-c(3:5, 8:17)) %>% 
                      filter(Finished == 1) %>% 
                      filter(!is.na(EndDate)) %>% 
                      arrange(id, EndDate) %>% 
                      distinct(id, .keep_all = TRUE)))
qualtrics_first = temp[[1]]
qualtrics_second = temp[[2]]
qualtrics_third = temp[[3]]
rm(headers_first, headers_second, headers_third, temp)
qualtrics_third = qualtrics_third %>% select(-c(fake, group))

#Getting the topics of the news articles (elasticsearch export)
topics = topics[!duplicated(topics$ID), ]
topics = topics %>% select(-X1)

psych::describe(qualtrics_first)

#Description
#1753 people finished the first questionnaire, on average it took them 11 minutes to finish the questionnaire

#_______________________________________________________________________Recoding variables, making scales_________________________________________________________
#Gender
qualtrics_first$gender = qualtrics_first$Q1
#Recode age
qualtrics_first$age = qualtrics_first$Q2 + 15
#Education
qualtrics_first$edu = qualtrics_first$Q3

#Recode political knowledge  -> 0 if wrong or not known, 1 if correct answer (later: maybe do something with differentiating wrong/do not know)
qualtrics_first$Q16 = ifelse(qualtrics_first$Q16 == 6, 1, 0)
qualtrics_first$Q26 = ifelse(qualtrics_first$Q26 == 5, 1, 0)
qualtrics_first$Q30 = ifelse(qualtrics_first$Q30 == 3, 1, 0)

qualtrics_first$Q23 = ifelse(qualtrics_first$Q23 == 7, 1, 0)
qualtrics_first$Q25 = ifelse(qualtrics_first$Q25 == 2, 1, 0)
qualtrics_first$Q29 = ifelse(qualtrics_first$Q29 == 7, 1, 0)

qualtrics_first$Q17 = ifelse(qualtrics_first$Q17 == 2, 1, 0)
qualtrics_first$Q27 = ifelse(qualtrics_first$Q27 == 9, 1, 0)
#qualtrics_first$Q28 = ifelse(qualtrics_first$Q28 == 7, 1, 0) -> went out of politics in the middle of the study

qualtrics_first$Q42 = ifelse(qualtrics_first$Q42 == 2, 1, 0)
qualtrics_first$Q18 = ifelse(qualtrics_first$Q18 == 7, 1, 0)
qualtrics_first$Q19 = ifelse(qualtrics_first$Q19 == 3, 1, 0)

qualtrics_first = qualtrics_first %>% mutate(pol_knowledge = rowSums(.[, c("Q16", "Q26", "Q30", "Q23", "Q25", "Q29", "Q17", "Q27", "Q42", "Q18", "Q19")]))
table(qualtrics_first$pol_knowledge)

qualtrics_first$questions_right = qualtrics_first$Q39 - 2
qualtrics_first = qualtrics_first %>% mutate(questions_right = if_else(questions_right < 0, 0, questions_right))

#Overconfidence
#If number is positive, person thinks they have more questions correct than they actually have correct (overconfident), if the number is negative
#they think they have less quesitons correct than they actually have correct (underconfident)
qualtrics_first$overconfidence = qualtrics_first$questions_right - qualtrics_first$pol_knowledge

#Political efficacy/self-reported knowledge
alpha(qualtrics_first[c("Q5_1", "Q5_2", "Q5_3")], check.keys=TRUE)
cols = c("Q5_2")
qualtrics_first[ ,cols] = 8 - qualtrics_first[ ,cols]
qualtrics_first = qualtrics_first %>% mutate(political_efficacy = rowMeans(.[, c("Q5_1", "Q5_2", "Q5_3")]))

#Political interest
qualtrics_first$pol_interest = qualtrics_first$Q6

#News usage
alpha(qualtrics_first[c("Q4_1", "Q4_2", "Q4_3", "Q4_4","Q4_5")], check.keys=TRUE)
qualtrics_first = qualtrics_first %>% mutate(overall_media_usage = rowMeans(.[, c("Q4_1", "Q4_2", "Q4_3", "Q4_4","Q4_5")]))
qualtrics_first %>% select("Q4_1", "Q4_2", "Q4_3", "Q4_4","Q4_5") %>% describe()
qualtrics_first %>% select("Q4_1", "Q4_2", "Q4_3", "Q4_4","Q4_5") %>% cor(use = 'complete.obs')
qualtrics_first = qualtrics_first %>% 
  rename_at(vars(starts_with('Q4_')), ~ c('m_tv1', 'm_tv2', 'm_newspaper', 'm_computer', 'm_mobile'))

#News interest per topic 
alpha(qualtrics_first %>% select(., starts_with("Q31_")), check.keys=TRUE)
qualtrics_first = qualtrics_first %>% mutate(news_topic_interest = rowMeans(select(., starts_with("Q31_"))))
qualtrics_first %>% select(., starts_with("Q31_")) %>% describe()
qualtrics_first %>% select(., starts_with("Q31_")) %>% cor(use = 'complete.obs')
qualtrics_first = qualtrics_first %>% 
  rename_at(vars(starts_with('Q31_')), ~ c('t_buit', 't_bin', 't_econ', 't_crime','t_sport', 't_ent', 't_mil', 't_imm'))

#Diversity of news interest (baseline for model)
#We have eight news categories, each from 1-7 (not interested to very much interested)
#First option: In how many categories did someone give a 4 or higher (meaning they are at least moderately interested)

qualtrics_first = qualtrics_first %>% mutate_at(vars(starts_with("t_")), .funs = list(d = ~as.numeric(. > 4)))
qualtrics_first = qualtrics_first %>% mutate(div_trait = rowSums(select(., ends_with("_d"))))
psych::describe(qualtrics_first$div_trait)

#Second option: 3 or higher

qualtrics_first = qualtrics_first %>% mutate_at(vars(starts_with("t_"), -ends_with("_d")), .funs = list(d1 = ~as.numeric(. > 3)))
qualtrics_first = qualtrics_first %>% mutate(div_trait2 = rowSums(select(., ends_with("_d1"))))
psych::describe(qualtrics_first$div_trait2)

#Strength of news preferences 
alpha(qualtrics_first[c("Q36_1", "Q36_2", "Q36_3", "Q36_4")], check.keys=TRUE)
cols = c("Q36_3", "Q36_4")
qualtrics_first[ ,cols] = 8 - qualtrics_first[ ,cols]
qualtrics_first = qualtrics_first %>% mutate(news_pref_strength = rowMeans(select(., starts_with("Q36_"))))
qualtrics_first %>% select(., starts_with("Q36_")) %>% describe()
qualtrics_first %>% select(., starts_with("Q36_")) %>% cor(use = 'complete.obs')

#Desirability of control (recode three reversed items)
cols = c("Q7_2", "Q7_4","Q7_6")
qualtrics_first[ ,cols] = 8 - qualtrics_first[ ,cols]
qualtrics_first = qualtrics_first %>% mutate(desirability_of_control = rowMeans(select(., starts_with("Q7_"))))

#Going further
qualtrics_first$going_on = qualtrics_first$Q38
#Renaming start and enddates
qualtrics_first = qualtrics_first %>% rename_at(c('StartDate', 'EndDate', 'Duration (in seconds)', 'Finished'), ~ c('start_t1', 'end_t1', 'dur_t1','fin_t1'))
qualtrics_first = qualtrics_first %>% select(-matches('^t_|^m_'),matches('^t_|^m_'))
qualtrics_first = qualtrics_first %>% select(.,-starts_with("Q"))
psych::describe(qualtrics_first)

#Description
#In the whole sample: 
#Desirability of control (M = 3.43, SD = 0.77), scale 0-11
#Political Knowledge (M = 5.54, SD = 3.34), scale 0 - 11 
#Overconfidence (M = -0.66, SD = 2.92), scale -8 - 13
#Political Efficacy (M = 4, SD = 1.22), scale 1-7
#Overall Media Usage (M = 5.03, SD = 1.63), scale 1-8
#News Topic Interest (M = 4.42, SD = 1.07), scale 1-7
#News Preferences Strength (M = 5.25, SD = 1.01), scale 1-7
#Diversity Trait 1 (M = 4.15, SD = 0.24) scale 0-8
#Diversity Trait 2 (M = 5.84, SD = 0.21) scale 0-8

#_________________________________________________Renaming variables second dataset___________________
qualtrics_second = qualtrics_second %>% rename_at(c('StartDate', 'EndDate', 'Duration (in seconds)', 'Finished'), ~ c('start_t2', 'end_t2', 'dur_t2','fin_t2'))
qualtrics_second = qualtrics_second %>%
  rename_at(vars(starts_with("Q1_")), funs(sub("Q1_", "satis_", .)))
qualtrics_second = qualtrics_second %>% rename_at(c("Q2", "Q13"), ~ c("beh_int", "comment")) %>% select(-c("fake", 'group'))
#_______________________________________________________________Merge datasets____________________________________________________________________________________
#Merge data, only select relevant ones, make one dataset for the people that finished, one for the ones that did not
qualtrics = merge(x = qualtrics_first, y = qualtrics_second, by = "id", all.x = TRUE)
qualtrics_finished = qualtrics %>% filter(!is.na(end_t2))
qualtrics_finished$group = as.factor(qualtrics_finished$group)

qualtrics_not_finished = qualtrics %>% filter(is.na(end_t2))
psych::describe(qualtrics_not_finished)
#write.csv(qualtrics_not_finished, file = "data/intermediate/qualtrics_dropout.csv", row.names = FALSE)
remove(qualtrics, qualtrics_first, qualtrics_second, qualtrics_not_finished)

#Description
#Those who did not finish (1455 out of the original 1753): 
#Desirability of control (M = 3.46, SD = 0.77), scale 0-11
#Political Knowledge (M = 5.30, SD = 3.34), scale 0 - 11 
#Overconfidence (M = -0.57, SD = 2.91), scale -10 - 11
#Political Efficacy (M = 3.98, SD = 1.23), scale 1-7
#Overall Media Usage (M = 5.02, SD = 1.65), scale 1-8
#News Topic Interest (M = 4.40, SD = 1.08), scale 1-7
#News Preferences Strength (M = 5.19, SD = 1.03), scale 1-7

#________________________________Excluding respondents
#Excluding respondents who have a SD of 0 on one of the dependent variables (control, satisfaction)
qualtrics_finished$sd_system_control = qualtrics_finished %>% select(., starts_with("system_control")) %>%  apply(.,1,sd)
qualtrics_finished$sd_strategy_control = qualtrics_finished %>% select(., starts_with("strategy_control")) %>%  apply(.,1,sd)
qualtrics_finished$sd_satisfaction = qualtrics_finished %>% select(., starts_with("satis_")) %>%  apply(.,1,sd)

#Merging with the user data from the MySQL database and with the stories read database
users = users %>% rename(user_id = id) %>% select(-username)
users = users %>% filter(panel_id != "noIDyet" & !is.na(sum_logins))
qualtrics_finished = merge(x = qualtrics_finished, y = users, by.x = "id", by.y = 'panel_id', all.x = TRUE)
qualtrics_finished = qualtrics_finished %>% select(-matches('^t_|^m_|^system|^strategy|^satis_|Diversity_'),matches('^t_|^m_|^system|^strategy|^satis_|Diversity_'))

selected_news = selected_news %>% rename(story_id = id)
qualtrics_stories = merge(x = qualtrics_finished, y = selected_news, by.x = "user_id", by.y = "user_id")

#Finding out how much time was spent on reading a story
qualtrics_stories$time_spent = substring(qualtrics_stories$time_spent, 12)
qualtrics_stories =  qualtrics_stories %>% mutate(time_spent = period_to_seconds(hms(qualtrics_stories$time_spent)))

#Difference between Start Date of the second questionnaire (end date) and the starttime of reading a story
qualtrics_stories$qualtrics_story_diff = as.numeric(qualtrics_stories$start_t2 - (as_datetime(qualtrics_stories$starttime)+1*60*60))

qualtrics_stories_w1 = qualtrics_stories %>% filter(qualtrics_story_diff > 0 | is.na(starttime))
qualtrics_stories_w2 = qualtrics_stories %>% filter(qualtrics_story_diff <= 0 | is.na(starttime))
rm(qualtrics_stories_w2, qualtrics_stories)

#Finding respondents who spent less than 5 seconds on average on reading news articles (person with the ID 1484779109)
qualtrics_stories_w1 %>% group_by(id) %>% summarise(avg_time = mean(time_spent, na.rm = TRUE)) %>% arrange(avg_time)

qualtrics_finished = qualtrics_finished %>% filter(sd_system_control > 0 & sd_strategy_control > 0 & !(id %in% c('1484779109')) & sd_satisfaction > 0 & id != "noIDyet")

psych::describe(qualtrics_finished)

#Description
#Those who did finish and pass the straightliner test (248 out of the original 1753): 
#Desirability of control (M = 3.30, SD = 0.71), scale 0-11
#Political Knowledge (M = 6.82, SD = 3.05), scale 0 - 11 
#Overconfidence (M = -1.12, SD = 3.00), scale -10 - 11
#Political Efficacy (M = 4.17, SD = 5.10), scale 1-7
#Overall Media Usage (M = 5.10, SD = 1.49), scale 1-8
#News Topic Interest (M = 4.51, SD = 1.00), scale 1-7
#News Preferences Strength (M = 5.53, SD = 0.84), scale 1-7
#Diversity Trait 1 (M = 4.34, SD = 2.18) scale 0-8
#Diversity Trait 2 (M = 5.91, SD = 1.92) scale 0-8

#________________________________________________________Stories read_________________________________________________________________

id_list = qualtrics_stories_w1 %>% select(group, user_id, id) %>% filter(id %in% qualtrics_finished$id) %>% distinct(group, user_id, .keep_all = TRUE) 
qualtrics_stories_w1  = qualtrics_stories_w1 %>% filter(id %in% id_list$id)

#Getting the average ratings (also a measure of satisfaction) for each respondent and merge the datasets
qualtrics_stories_w1 = qualtrics_stories_w1 %>% group_by(id) %>% 
  mutate(avg_rating = mean(rating, na.rm = TRUE), avg_rating2 = mean(rating2, na.rm = TRUE), number_stories_read = n()) %>% 
  ungroup() %>% select(id, avg_rating2, number_stories_read)

qualtrics_finished = merge(x = qualtrics_finished, y = qualtrics_stories_w1, by = "id", all.x = TRUE)
qualtrics_finished = qualtrics_finished %>% group_by(user_id) %>% filter(row_number() == 1)
qualtrics_finished = qualtrics_finished %>% rename(panel_id = id)
remove(qualtrics_stories_w1)

psych::describe(qualtrics_finished$number_stories_read)
psych::describe(qualtrics_finished$avg_rating)
psych::describe(qualtrics_finished$avg_rating2)

#Description
#Average amount of stories read in wave one: 92.81 (SD = 66.58), ranging from 31 to 571, 
#75% read up to 100, one person is a clear outlier with 571, next most frequent at under 400
#Average ratings: 2.72 (0.75) for first rating, 2.64 (0.7) for second rating

#__________________________Working with the news dataset___________________________________________
#Merging news and topic dataset to attach a topic to each news article
news = merge(x = news, y = topics, by.x = "elasticsearch", by.y = 'ID', all.x = TRUE)
selected_news = merge(x = selected_news, y = topics, by.x = "news_id", by.y = 'ID', all.x = TRUE)
remove(topics)

news = news %>% rename(sql_id_news = id, news_id = elasticsearch)
news = merge(x = news, y = qualtrics_finished, by.x = "user_id", by.y = 'user_id', all.x = TRUE)

#Making two news datasets: One for the people that finished the first part and one for the ones that did not finish
news_finished = news %>% filter(user_id %in% id_list$user_id)
news_dropout = news %>% filter(!(user_id %in% id_list$user_id))
#write.csv(news_dropout, file = "data/intermediate/news_dropout.csv", row.names = FALSE)
#write.csv(qualtrics_finished, file = "data/intermediate/qualtrics_finished.csv", row.names = FALSE)
rm(news, news_dropout)

#Getting the time difference between two exposures (one user of one timestamp) since sometimes they are split by a second
exposures = news_finished %>% select(0:8) %>% arrange(user_id, timestamp) %>% 
  mutate(timediff = difftime(lead(timestamp), timestamp))

#Mutate: Keep the original timestamp + if the previous timestamp was only 0 or 1 second apart from the same user and not empty, give a new
#timestamp that equals the previous timestamp. This is needed to merge two exposures that belong together.
#This needs to be repeated multiple times to also catch the "long" exposures
exposures = exposures %>% mutate(timestamp_old = timestamp, timestamp_new = ifelse((lag(timediff) == 1 | lag(timediff) == 0) & lag(user_id) == user_id & !is.na(lag(timestamp)), lag(timestamp), timestamp))
x = 1
repeat {
  exposures = mutate(exposures, timestamp_old = timestamp_old, timestamp_new = ifelse((lag(timediff) == 1 | lag(timediff) == 0) & lag(user_id) == user_id & !is.na(lag(timestamp)), lag(timestamp_new), timestamp)) 
  x = x+1
  if (x == 300){
    break
  }
}

#Remove variables that are not needed for matching with selected news; making sure the data are arranged correctly for duplicate search
exposures = exposures %>% group_by(user_id, timestamp_new) %>% mutate(exposure_id = cur_group_id()) %>% ungroup() %>% arrange(user_id, timestamp_new, position)
exposures_select = exposures %>% select(-timestamp, -timediff, -recommended, -url, -Topic, -sql_id_news)

#Spread data to later check whether a selected news item was in the selection presented to the user. 
exposures_select = exposures_select %>% 
  group_by_at(vars(-news_id)) %>%  # group by everything other than the value column. 
  mutate(row_id=1:n()) %>% ungroup() %>%  # build group index
  spread(key=position, value=news_id) %>%    # spread
  select(-row_id)  # drop the index
exposures_select = exposures_select %>% rename_with(~paste0("pos_",.), `0`:`268`)
exposures_select = exposures_select %>% arrange(user_id, exposure_id)

#Filtering out those that are duplicates and where one exposure had more than 9 news displayed
exposures_select = exposures_select %>% group_by(exposure_id) %>% filter(!(!is.na(pos_9) & n() > 1))

#Getting the day of the date for matching with selected news (otherwise too computationally expensive)
exposures_select$timestamp_new = as_datetime(exposures_select$timestamp_new)
exposures_select$day = format(as.Date(exposures_select$timestamp_new), "%d")
selected_news$day = format(as.Date(selected_news$starttime), "%d")

#Merging exposures with information on the selected news on user_id and day
selected_news_info = merge(x = selected_news, y = exposures_select, by = c('user_id', "day"), all.x = TRUE)

#Looking at the difference between the timestamp of the selected news and the exposure - if it is lower than 0, the selected news 
#cannot be from that exposure. Further also filtering that they should be within 500 seconds of each other (otherwise too unlikely the news
#were selected in that exposure.
selected_news_info$diff = as.numeric(as_datetime(selected_news_info$starttime) - as_datetime(selected_news_info$timestamp_new))
selected_news_info = selected_news_info %>% filter(diff > 0)
selected_news_info = selected_news_info %>% filter(diff < 500)

#Filtering out only those exposures that have a match with the selected news item and getting the one with the closest difference and the 
#first time a news story was selected
selected_news_info = selected_news_info %>% filter_at(vars(starts_with("pos_")), any_vars(. == news_id))
selected_news_info = selected_news_info %>% group_by(exposure_id) %>% filter(diff == min(diff) | first(is.na(diff)))
selected_news_info = selected_news_info %>% group_by(user_id, news_id, exposure_id) %>% filter(row_number() == 1)
selected_news_info = selected_news_info %>% group_by(user_id, exposure_id) %>% arrange(story_id) %>% filter(row_number() == 1)

#Gathering the dataset again to filter out only those news that were selected and get their position as a number 
selected_news_info = 
  selected_news_info %>% 
  gather(key = "position", value = "news_id_position", "pos_0":"pos_268")
selected_news_info = selected_news_info %>% 
  filter(news_id == news_id_position) %>% 
  group_by(exposure_id) %>% 
  filter(row_number() == 1)

selected_news_info$position = as.numeric(sub("pos_", "", selected_news_info$position))


#Remaining 84 where the correct position cannot be inferred anymore (because the position number is higher than 8 + 
#the number of news displayed at the same timestamp is not a multiple of 9). They are excluded from the analysis.
selected_news_info = selected_news_info %>% filter(position < 9)


selected_news_info = selected_news_info %>% 
  rename(rating_selected = rating, rating2_selected = rating2, topic_selected = Topic, position_selected = position, news_id_selected = news_id, story_id_selected = story_id) %>%
  select(-day, -starttime, -endtime, -time_spent, -timestamp_old, -diff, -news_id_position, -timestamp_new, -user_id)
names(selected_news_info)

exposures_selected = merge(x = exposures, y = selected_news_info, by = c('exposure_id'), all.y = TRUE)
exposures_selected = exposures_selected %>% filter(position < 9)

#Turns out: Some exposures cannot correctly be identified (i.e. there is more than one exposure that is at that time and includes that news_id)
#Those exposures will be excluded
#This leads to 27,314 news selections in total

exposures_selected = exposures_selected %>% group_by(exposure_id) %>% filter(n() < 10)
exposures_selected_grouped = exposures_selected %>% group_by(exposure_id) %>% dplyr::summarize(position_selected = first(position_selected))

#Get additional information on the exposures, such as: How many different topics were displayed? What topic(s) were in the "majority"?
#Was the topic in the majority chosen?

exposures_selected = exposures_selected %>%
  # add a column n with count by categories
  add_count(user_id, timestamp_new, Topic) %>%
  # select max or first occurrence by user
  group_by(user_id, timestamp_new) %>%
  # keep only first TRUE
  mutate(majority = list(unique(Topic[n == max(n)])), number_most_freq = max(n)) %>%
  # do not keep temp var
  select(-n) %>% 
  mutate(number_topics_all = n_distinct(Topic), 
         ties = lengths(majority)) %>% 
  arrange(user_id, timestamp_new) %>% ungroup()
exposures_selected$majority = sapply(exposures_selected$majority , paste, collapse=" ")

exposures_selected$match = if_else(exposures_selected$ties == 1 & exposures_selected$topic_selected == exposures_selected$majority, 1, 
                                  ifelse(str_detect(exposures_selected$majority, exposures_selected$topic_selected) & exposures_selected$ties > 1, 1, 0))

#merge back with qualtrics information 
exposures_users = merge(x = exposures_selected, y = qualtrics_finished, by = "user_id", all.x = TRUE)
rm(selected_news_info, exposures, exposures_select, exposures_selected, exposures_selected_grouped)

#The actual diversity of the news selection of one person (on average, no time component yet)
actual_diversity = exposures_users %>% group_by(user_id) %>% summarise(act_div = mean(number_topics_all))
perc_diversity = news_finished %>% select(c(div_trait, Diversity_1, Diversity_2, Diversity_3, Diversity_4, user_id, group)) %>% 
  group_by(user_id) %>% 
  filter(row_number()==1)
diversity_comparison = merge(x = actual_diversity, y = perc_diversity, by = 'user_id', all.y = TRUE)
diversity_comparison = diversity_comparison %>% filter(!is.na(div_trait))
diversity_comparison$group = factor(diversity_comparison$group)

#For now: diversity is (1) how many different topics were on the page and (2) how often was the most used topic on the page (negative diversity)

topics_selected = exposures_users %>% group_by(Topic) %>% summarise(number_sel = n())
topics_all = news %>% group_by(Topic) %>% summarise(number_all = n())
topics_diff = merge(topics_all, topics_selected, by = 'Topic')
topics_diff$perc = topics_diff$number_all / topics_diff$number_sel

#Results: By far the most selected stories where the first one on the page, followed by the second one; after that, the ones in the first column are preferred
#Todo: find out whether that differs between mobile and desktop (have to merge it with the login data to see which device was used)
#Have to merge it with the user data to have the groups to say something about recommended/non recommended

#Logins
logins = logins %>% group_by(user_id) %>% mutate(number_logins = n()) %>% ungroup()
describe(logins)
#800 people logged in at least once; up to 61 times, on average (including the ones that did not finish) M = 6.84, SD = 7.09; 
logins_finished = logins %>% filter(user_id %in% id_list$user_id) %>% group_by(user_id) %>% mutate(number_logins = n()) %>% filter(number_logins > 6)
#Of those who finished: M = 14.3, SD = 6.4, median 13, between 7 and 61 times logged in; 75% needed 16 logins to finish overall 
#Checking only the first part 
#For that: Getting infos on when they finished the first part, subtracting the dates 

#Devices
logins = logins %>% 
  mutate(devices_group = case_when(is.na(user_agent) ~ NA_character_,
                                str_detect(user_agent, "^PC|^Mozilla") ~ "PC",
                                str_detect(user_agent, "^Generic Smartphone|^iPhone|^Samsung SM-A|^Samsung SM-G|^Samsung SM-J|^Nokia|^NUU|^LG|^SNE|^HTC|^Xiao") ~ "mobile", 
                                TRUE ~ "tablet"))

logins = logins %>% 
  mutate(os_group = case_when(is.na(user_agent) ~ NA_character_,
                                   str_detect(user_agent, "OS") ~ "Apple",
                                   str_detect(user_agent, "Windows") ~ "Windows", 
                                   str_detect(user_agent, "Android") ~ "Android",
                                   TRUE ~ "Linux"))

logins = logins %>% 
  mutate(browser_group = case_when(is.na(user_agent) ~ NA_character_,
                              str_detect(user_agent, "Firefox") ~ "Firefox",
                              str_detect(user_agent, "Chrome") ~ "Chrome", 
                              str_detect(user_agent, "Safari") ~ "Safari",
                              str_detect(user_agent, "Samsung") ~ "Samsung",
                              str_detect(user_agent, "Samsung") ~ "Opera",
                              str_detect(user_agent, "Edge") ~ "Edge",
                              str_detect(user_agent, "IE") ~ "IE",
                              TRUE ~ "other"))


logins_devices_amount = logins %>% group_by(user_id, devices_group) %>% summarise(number = n()) %>% 
  group_by(user_id) %>% summarise(number_devices = n())

logins = merge(x = logins, y = logins_devices_amount, by = "user_id", all.x = TRUE)
logins = logins %>% arrange(id)

logins_twice = logins %>% group_by(user_id) %>% filter(number_devices == 2) %>% 
  fill(devices_group, os_group, browser_group, .direction = "downup") %>% ungroup()

logins_more = logins %>% group_by(user_id) %>% filter(number_devices > 2) %>% 
  fill(devices_group, os_group, browser_group, .direction = "up") %>% ungroup()

logins_other = logins %>% group_by(user_id) %>% filter(n() < 2) %>% ungroup()

logins = bind_rows(logins_other, logins_twice, logins_more) %>% arrange(id)

remove(logins_twice, logins_more, logins_other, logins_devices_amount)

exposures_for_logins = exposures_users %>% select(user_id, exposure_id, timestamp_new)
exposures_for_logins = merge(x = exposures_for_logins, y = logins, by = "user_id", all.x = TRUE)
exposures_for_logins = exposures_for_logins %>% select(-id, -points_logins)
exposures_for_logins$diff = as.numeric(as_datetime(exposures_for_logins$timestamp_new) - as_datetime(exposures_for_logins$timestamp))
exposures_for_logins = exposures_for_logins %>% filter(diff >= 0)
exposures_for_logins = exposures_for_logins %>% 
  group_by(exposure_id) %>% 
  filter(diff == min(diff) | first(is.na(diff))) %>%
  filter(row_number() == 1)
exposures_for_logins = exposures_for_logins %>% select(-diff)

exposures_users = merge(x = exposures_users, y = exposures_for_logins, by = c('user_id', 'exposure_id', 'timestamp_new'), all.x = TRUE)
remove(exposures_for_logins)

exposures_users$timestamp_new = as_datetime(exposures_users$timestamp_new)
show_again$timestamp = as_datetime(show_again$timestamp)

exposures_users$day = format(as.Date(exposures_users$timestamp_new), "%d")
show_again$day = format(as.Date(show_again$timestamp), "%d")
show_again = show_again %>% rename(timestamp_decision = timestamp)

exposures_users_merge = exposures_users %>% select(user_id, day, exposure_id, news_id, timestamp_new)

exposures_users_merge = merge(x = exposures_users_merge, y = show_again, by = c('user_id', 'day'))
rm(show_again)
exposures_users_merge = exposures_users_merge %>% select(-day)

exposures_users_merge$diff = as.numeric(as_datetime(exposures_users_merge$timestamp_decision) - as_datetime(exposures_users_merge$timestamp_new))

exposures_users_merge = exposures_users_merge %>% 
  filter(diff >= 0) %>%
  group_by(exposure_id) %>% 
  filter(diff == min(diff) | first(is.na(diff)))
exposures_users_merge = exposures_users_merge %>% select(-diff)

exposures_users = merge(x = exposures_users_merge, y = exposures_users, by = c("user_id", "exposure_id", "news_id", "timestamp_new"))
rm(exposures_users_merge)

make_group_id = exposures_users %>% group_by(user_id, exposure_id) %>% dplyr::summarize(n = n()) %>% group_by(user_id) %>% 
  mutate(exposure_user_id = sequence(n())) %>% select(-n)

exposures_users = merge(x = exposures_users, y = make_group_id, by = c("user_id", "exposure_id"), all.x = TRUE)
exposures_users = exposures_users %>% group_by(user_id, exposure_id, position) %>% filter(row_number() == 1)
exposures_users = exposures_users %>%
  rename(timestamp = timestamp_new, topic = Topic, timestamp_login = timestamp.y) %>%
  select(-sql_id_news, -id, -timediff,-timestamp.x, -timestamp_old, -fin_t1, -fin_t2, -comment, -sd_system_control, 
         -sd_strategy_control, -sd_satisfaction, -sum_invites, -activated, -reminder_sent, -user_agent, -day)


get_sessions = exposures_users %>% group_by(exposure_id) %>% select(user_id, timestamp_login, timestamp) %>% mutate(timestamp_diff = timestamp_login - timestamp)
get_sessions = get_sessions %>% group_by(user_id, exposure_id) %>% filter(row_number() == 1) %>% arrange(user_id, exposure_id, timestamp_login) %>% ungroup()
get_sessions$timestamp_diff = abs(as.numeric(get_sessions$timestamp_diff))
get_sessions = get_sessions %>% mutate(timestamp_diff2 = timestamp_diff - lag(timestamp_diff, default = 0))
get_sessions = get_sessions %>% mutate(new_exposure = ifelse((timestamp_diff2 > 900 | timestamp_diff2 < 0 | lag(user_id, default = 0) != user_id), 1, 0))
get_sessions = get_sessions %>% group_by(user_id) %>% mutate(session = cumsum(new_exposure))
get_sessions = get_sessions %>% select(-timestamp_diff, -timestamp_diff2, -new_exposure)

exposures_users = merge(x = exposures_users, y = get_sessions, by = c('exposure_id', 'user_id', 'timestamp_login', 'timestamp'), all.x = TRUE)

rm(make_group_id, get_sessions)

##Adding elasticsearch information to the dataset
exposures_users = merge(x = exposures_users, y = elasticsearch_info, by.x = 'news_id', by.y = 'id', all.x = TRUE)
rm(elasticsearch_info)

#___________________________________________________________________Making scales for DVs__________________________________________________

#Cronbach's alpha for the variables (1) system control, (2) strategy control (3) perceived diversity and (4) website satisfaction (adjectives)

#system control: alpha 0.85, omega total 0.88
#strategy control: alpha 0.81, omega total 0.88
#system control + strategy_control: alpha 0.89, omega total 0.93
#diversity: alpha 0.69; excluding items only makes it worse, omega total 0.79
#satisfaction: alpha 0.93, omega total 0.95

omega(exposures_users[c("system_control_1","system_control_2","system_control_3","system_control_4")])
omega(exposures_users[c("strategy_control_1","strategy_control_2","strategy_control_3","strategy_control_4", "strategy_control_5")])
omega(exposures_users[c("Diversity_1","Diversity_2","Diversity_3","Diversity_4")])
omega(exposures_users[c("satis_1","satis_2","satis_3","satis_4", "satis_5", "satis_6", "satis_7")])
omega(exposures_users[c("system_control_1","system_control_2","system_control_3","system_control_4", "strategy_control_1","strategy_control_2","strategy_control_3","strategy_control_4", "strategy_control_5")])

#Recoding the items that were reverse coded 

cols = c("system_control_1", "strategy_control_1", "strategy_control_2", "strategy_control_3", "Diversity_2", "Diversity_4")
exposures_users[ ,cols] = 8 - exposures_users[ ,cols]

#Making the scales for the DVs (not making one for diversity yet)

exposures_users = exposures_users %>% mutate(system_control = rowMeans(select(., starts_with("system_control"))))

exposures_users = exposures_users %>% mutate(strategy_control = rowMeans(select(., starts_with("strategy_control"))))
exposures_users = exposures_users %>% mutate(satisfaction = rowMeans(select(., starts_with("satis_"))))

exposures_users = exposures_users %>% mutate(control_overall = rowMeans(select(., contains("_control_"))))


###Split up in 3 dataframes: User information, exposure information, article information 

exposures_users = exposures_users %>% mutate(selected = if_else(news_id == news_id_selected, 1, 0))
users = exposures_users %>% select(user_id, group, gender, age, edu, pol_knowledge, overconfidence, political_efficacy, pol_interest, overall_media_usage, news_topic_interest,
                                  div_trait, div_trait2, news_pref_strength, desirability_of_control, beh_int, first_login, last_visit, number_logins,
                                  number_stories_read, m_tv1, m_tv2, m_newspaper, m_computer, m_mobile, t_buit, t_bin, t_econ, t_crime, t_sport, t_ent, 
                                  t_mil, t_imm, Diversity_1, Diversity_2, Diversity_3, Diversity_4, system_control, strategy_control, satisfaction, control_overall, avg_rating2)
article_info = exposures_users %>% select(news_id, url, teaser_rss, title_rss, publication_date, text, topic)
exposures = exposures_users %>% select(exposure_id, exposure_user_id, session, timestamp, user_id,majority, number_most_freq, 
                                     number_topics_all, ties, match, devices_group, os_group, browser_group, show_again, selected, news_id, recommended, position, news_id_selected, 
                                rating_selected, rating2_selected, topic_selected, position_selected)

users = users[!duplicated(users$user_id),]
article_info = article_info[!duplicated(article_info$news_id),]

write.csv(users, file = (here::here("data/intermediate/users_new.csv")), row.names = FALSE)
write.csv(article_info, file = (here::here("data/intermediate/article_features.csv")), row.names = FALSE)
write.csv(exposures, file = (here::here("data/intermediate/exposures.csv")), row.names = FALSE)