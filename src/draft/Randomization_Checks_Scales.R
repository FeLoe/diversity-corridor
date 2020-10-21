library(tidyverse)
library(dplyr)
library(RMySQL)
library(ggpubr)
library(lubridate)
library(car)
library(FSA)
library(Hmisc)
library(psych)
library(car)
library(here)

#____________________________________________________________once with the SD people included_____________________________________________________________________
users = read_csv(here::here("data/tmp/users_new.csv"))

#Gender (Chi-square 1.759, df = 3, p-value = 0.6239) -> no differences between groups
tbl = table(users$group, users$gender)
chisq.test(tbl)

#Education (Chi-square 15.704, df = 18, p-value = 0.6132) -> no differences between groups -> does it need recoding? 
tbl = table(users$group, users$edu)
chisq.test(tbl)
#Education groups: 3 VMBO, 4 HAVO/VWO, 5 MBO, 6 HBO, 7 WO
users$edu_1 = recode_factor(users$edu, `3` = "lower", `4` = 'lower', `5` = 'middle', `6` = 'higher', `7` = 'higher')
tbl = table(users$group, users$edu_1)
chisq.test(tbl)

users$edu_2 = recode_factor(users$edu, `3` = "VMBO/HAVO/VWO", `4` = 'VMBO/HAVO/VWO', `5` = 'MBO', `6` = 'HBO', `7` = 'WO')
tbl = table(users$group, users$edu_2)
chisq.test(tbl)


#Age -> ANOVA first shows significant differences between group 4 and 2; also: normality assumptions are violated, Kruskal-Wallis test also shows significant differences between 2 and 4
res.aov <- aov(age ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(age ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(age ~ group, data = users)
PT = dunnTest(age ~ group,
              data=users,
              method="bh") 
PT

res = Anova(lm(age ~ group, data = users), type = "III")
res


#Desirability of control scale -> no significant differences between the groups
#alpha(qualtrics_finished[c("Q7_1", "Q7_2", "Q7_3", "Q7_4", "Q7_5", "Q7_6", "Q7_7", "Q7_8", "Q7_9", "Q7_10")], check.keys=TRUE)
res.aov <- aov(desirability_of_control ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(desirability_of_control ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

#Political efficacy/self-reported knowledge -> no differences between the groups also not with Kruskal Wallis (since normality assumption is violated)
#alpha(qualtrics_finished[c("Q5_1", "Q5_2", "Q5_3")], check.keys=TRUE)
res.aov <- aov(political_efficacy ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(political_efficacy ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(political_efficacy ~ group, data = users)
PT = dunnTest(political_efficacy ~ group,
              data=users,
              method="bh") 
PT



#Political knowledge -> violates normality assumption, both tests (Anova and Kruskal Wallis) show no difference in political knowledge between the groups
res.aov <- aov(pol_knowledge ~ group, data = users)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(pol_knowledge ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(pol_knowledge ~ group, data = users)
PT = dunnTest(pol_knowledge ~ group,
              data=users,
              method="bh") 
PT

#Political knowledge groups
users$pol_knowledge_1 = recode_factor(users$pol_knowledge, `0` = "low", `1` = 'low', `2` = 'low', `3` = 'low',
                                      `4` = 'middle', `5` = 'middle', `6` = 'middle', `7` = 'middle',
                                      `8` = 'high', `9` = 'high', `10` = 'high', `11` = 'high')
tbl = table(users$group, users$pol_knowledge_1)
chisq.test(tbl)

#Political interest -> violates normality assumption both tests (Anova and Kruskal Wallis) show no diffeerence in political interest between the groups
res.aov <- aov(pol_interest ~ group, data = users)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(pol_interest ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(pol_interest ~ group, data = users)
PT = dunnTest(pol_interest ~ group,
              data=users,
              method="bh") 
PT


#News usage -> no difference in overall news usage, no difference for each single item 
alpha(qualtrics_finished[c("Q4_1", "Q4_2", "Q4_3", "Q4_4","Q4_5")], check.keys=TRUE)
res.aov <- aov(overall_media_usage ~ group, data = users)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(overall_media_usage ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(overall_media_usage ~ group, data = users)
PT = dunnTest(overall_media_usage ~ group,
              data=users,
              method="bh") 
PT

res.aov <- aov(Q4_1 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q4_1 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q4_1 ~ group, data = qualtrics_finished)
PT = dunnTest(Q4_1 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


res.aov <- aov(Q4_2 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q4_2 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q4_2 ~ group, data = qualtrics_finished)
PT = dunnTest(Q4_2 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


res.aov <- aov(Q4_3 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q4_3 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q4_3 ~ group, data = qualtrics_finished)
PT = dunnTest(Q4_3 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


res.aov <- aov(Q4_4 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q4_4 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q4_4 ~ group, data = qualtrics_finished)
PT = dunnTest(Q4_4 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q4_5 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q4_5 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q4_5 ~ group, data = qualtrics_finished)
PT = dunnTest(Q4_5 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


#News interest per topic -> no differences for overall topic interest, difference regarding interest in Binnenlands nieuws (post hoc shows no significant differences between single groups, however)
#For none of the other topics any differences between groups
alpha(qualtrics_finished[c("Q31_1", "Q31_2", "Q31_3", "Q31_4","Q31_5", "Q31_6", "Q31_7", "Q31_8")], check.keys=TRUE)
res.aov <- aov(news_topic_interest ~ group, data = users)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(news_topic_interest ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(news_topic_interest ~ group, data = users)
PT = dunnTest(news_topic_interest ~ group,
              data=users,
              method="bh") 
PT

res.aov <- aov(Q31_1 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_1 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_1 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_1 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_2 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_2 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_2 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_2 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


res.aov <- aov(Q31_3 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_3 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_3 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_3 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_4 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_4 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_4 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_4 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_5 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_5 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_5 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_5 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_6 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_6 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_6 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_6 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_7 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_7 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_7 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_7 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q31_8 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q31_8 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q31_8 ~ group, data = qualtrics_finished)
PT = dunnTest(Q31_8 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

#Overconfidence -> no group differences regarding overconfidence
res.aov <- aov(overconfidence ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(overconfidence ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(overconfidence ~ group, data = qualtrics_finished)
PT = dunnTest(overconfidence ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


#Strength of news preferences -> no differences overall or for any single item (but: does not form a good scale, items should rather be treated separately)
alpha(qualtrics_finished[c("Q36_1", "Q36_2", "Q36_3", "Q36_4")], check.keys=TRUE)
res.aov <- aov(news_pref_strength ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(news_pref_strength ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(news_pref_strength ~ group, data = qualtrics_finished)
PT = dunnTest(news_pref_strength ~ group,
              data=qualtrics_finished,
              method="bh") 
PT


res.aov <- aov(Q36_1 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q36_1 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q36_1 ~ group, data = qualtrics_finished)
PT = dunnTest(Q36_1 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q36_2 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q36_2 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q36_2 ~ group, data = qualtrics_finished)
PT = dunnTest(Q36_2 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q36_3 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q36_3 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q36_3 ~ group, data = qualtrics_finished)
PT = dunnTest(Q36_3 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT

res.aov <- aov(Q36_4 ~ group, data = qualtrics_finished)
summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Q36_4 ~ group, data = qualtrics_finished)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Q36_4 ~ group, data = qualtrics_finished)
PT = dunnTest(Q36_4 ~ group,
              data=qualtrics_finished,
              method="bh") 
PT