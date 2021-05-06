library(tidyverse)
library(psych)
library(car)
library(FSA)
library(here)

#____________________________________________________________once with the SD people included_____________________________________________________________________
users = read_csv(here::here("data/intermediate/users_new.csv"))

#Gender (Chi-square 1.5409, df = 3, p-value = 0.6729) -> no differences between groups
tbl = table(users$group, users$gender)
chisq.test(tbl)

#Education (Chi-square 11.145, df = 12, p-value = 0.5165) -> no differences between groups -> does it need recoding? 
tbl = table(users$group, users$edu)
chisq.test(tbl)
#Education groups: 3 VMBO, 4 HAVO/VWO, 5 MBO, 6 HBO, 7 WO
users$edu_1 = recode_factor(users$edu, `3` = "lower", `4` = 'lower', `5` = 'middle', `6` = 'higher', `7` = 'higher')
tbl = table(users$group, users$edu_1)
chisq.test(tbl)

users$edu_2 = recode_factor(users$edu, `3` = "VMBO/HAVO/VWO", `4` = 'VMBO/HAVO/VWO', `5` = 'MBO', `6` = 'HBO', `7` = 'WO')
tbl = table(users$group, users$edu_2)
chisq.test(tbl)

users$group = as.factor(users$group)
#Age: No differences between groups
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

res.aov <- aov(desirability_of_control ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(desirability_of_control ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )

#Political efficacy/self-reported knowledge -> no differences between the groups also not with Kruskal Wallis (since normality assumption is violated)

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

#Political interest -> violates normality assumption both tests (Anova and Kruskal Wallis) show no differences in political interest between the groups
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


#News usage -> no difference in overall news usage 

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




#News interest per topic -> no differences for overall topic interest
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