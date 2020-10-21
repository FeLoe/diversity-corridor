library(tidyverse)
library(dplyr)
library(psych)
library(RMySQL)
library("ggpubr")
library(lubridate)
library(car)
library(FSA)
library(Hmisc)
library(moments)
library(sjstats)
library(effsize)
library(BayesFactor)
library(car)
library(rcompanion)
library(here)

#________________________________________________________Control and Satisfaction: ANOVA/T-test_____________________________________________________________________________________
users = read_csv(here::here("data/tmp/users_new.csv"))

users$group = as.factor(users$group)
#Want to use it further

ggboxplot(users, x = "group", y = "beh_int", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07",  "#24913A"),
          order = c("1", "2", "3", "4"),
          ylab = "Want to visit again", xlab = "Group")

ggline(users, x = "group", y = "beh_int", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "Want to visit again", xlab = "Group")

res.aov <- aov(beh_int ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(beh_int ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(beh_int ~ group, data = users)

#-------> Using Kruskal-Wallis test since normality assumptions are violated; no significant differences between the groups


#System Control

ggboxplot(users, x = "group", y = "system_control", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07",  "#FC4E07"),
          order = c("1", "2", "3", "4"),
          ylab = "System control", xlab = "Group")

ggline(users, x = "group", y = "system_control", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "System control", xlab = "Treatment")

res.aov <- aov(system_control ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(system_control ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
eta_sq(res.aov)

kruskal.test(system_control ~ group, data = users)
pairwise.wilcox.test(users$system_control, users$group,
                     p.adjust.method = "BH")
epsilonSquared(x = users$system_control, g = users$group)
freemanTheta(x = users$system_control, g = users$group)


#------------------------->Using Kruskal-Wallis test (normality assumptions violated), significant, but low power (~65%)

#Strategy control 

ggboxplot(users, x = "group", y = "strategy_control", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07",  "#FC4E07"),
          order = c("1", "2", "3", "4"),
          ylab = "Perceived Control", xlab = "Group", na.rm=TRUE)

ggline(users, x = "group", y = "strategy_control", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "Perceived Control", xlab = "Group") + scale_x_discrete(labels=c("1" = "Random", "2" = "Topic Rec",
                                                                               "3" = "Similarity Rec", "4" = "Customization"))

res.aov <- aov(strategy_control ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(strategy_control ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
eta_sq(res.aov)

kruskal.test(strategy_control ~ group, data = users)
pairwise.wilcox.test(users$strategy_control, users$group,
                     p.adjust.method = "BH")
epsilonSquared(x = users$strategy_control, g = users$group)
freemanTheta(x = users$strategy_control, g = users$group)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), significant, power at ~76% (etasquared 0.039, effect size f 0.2015)
#------------------------->Pairwise comparisons: group 4 is significantly different from all the other groups

#Control overall

users$group = factor(users$group, levels=c("1","2","3","4"), labels=c("Random", "Similarity Rec", "Topic Rec", "Customization"))
ggboxplot(users, x = "group", y = "control_overall", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07",  "#FC4E07"),
          order = c("1", "2", "3", "4"),
          ylab = "Want to visit again", xlab = "Group")

ggline(users, x = "group", y = "control_overall", 
       add = c("mean_se", "jitter"), 
       order = c("Random", "Similarity Rec", "Topic Rec", "Customization"),
       ylab = "Perceived Control (combined scale)", xlab = "Experimental Group"
       ) 

res.aov <- aov(control_overall ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(control_overall ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
eta_sq(res.aov)

shapiro.test(x = aov_residuals )
kruskal.test(control_overall ~ group, data = users)
pairwise.wilcox.test(users$control_overall, users$group,
                     p.adjust.method = "BH")

epsilonSquared(x = users$control_overall, g = users$group)
freemanTheta(x = users$control_overall, g = users$group)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), significant, power at ~76% (etasquared 0.038, effect size f 0.2015)
#------------------------->Pairwise comparisons: group 4 is significantly different from all the other groups



#Satisfaction 
ggboxplot(users, x = "group", y = "satisfaction", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07",  "#FC4E07"),
          order = c("1", "2", "3", "4"),
          ylab = "Want to visit again", xlab = "Group")

ggline(users, x = "group", y = "satisfaction", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "Weight", xlab = "Treatment")

res.aov <- aov(satisfaction ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(satisfaction ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(satisfaction ~ group, data = users)

#-------> Using Kruskal-Wallis test since normality assumptions are violated; no significant differences between the groups

#_____________________________________________________________T-test_______________________________________________________________________________________________
#T-test: First testing assumptions (normality), homogeneity of variances

with(users, shapiro.test(satisfaction[group == "Similarity Rec"]))# p = 0.04 -> violates normality assumption for T-test
with(users, shapiro.test(satisfaction[group == "Topic Rec"])) # p = 0.20
with(users, shapiro.test(satisfaction[group == "Customization"])) # p = 0.06 -> violates normality assumption for T-test


qualtrics_t_test = users %>% filter(group == "Topic Rec" | group == "Similarity Rec")
#res.ftest = var.test(satisfaction ~ group, data = qualtrics_t_test)
#res.ftest
#res = t.test(satisfaction ~ group, var.equal = TRUE, data = qualtrics_t_test)
#res

res = wilcox.test(satisfaction ~ group, data = qualtrics_t_test, exact = FALSE)
res
cohen.d(qualtrics_t_test$satisfaction, factor(qualtrics_t_test$group))


#-------------------------------------->Comparing customization group
qualtrics_t_test = users %>% filter(group == "Topic Rec" | group == "Customization")
res = wilcox.test(satisfaction ~ group, data = qualtrics_t_test, exact = FALSE)
res
cohen.d(qualtrics_t_test$satisfaction, factor(qualtrics_t_test$group))

qualtrics_t_test = users %>% filter(group == "Similarity Rec" | group == "Customization")
res = wilcox.test(satisfaction ~ group, data = qualtrics_t_test, exact = FALSE)
res
cohen.d(qualtrics_t_test$satisfaction, factor(qualtrics_t_test$group))

users$group_ttest[users$group == 'Similarity Rec'] = "Rec"
users$group_ttest[users$group == 'Topic Rec'] = "Rec"
users$group_ttest[users$group == 'Customization'] = "Customization"

qualtrics_t_test = users %>% filter(group_ttest == "Rec" | group_ttest == "Customization")
res = wilcox.test(satisfaction ~ group_ttest, data = qualtrics_t_test, exact = FALSE)
res
cohen.d(qualtrics_t_test$satisfaction, factor(qualtrics_t_test$group_ttest))

#------------------------->Using Wilcox test (normality assumptions violated), not significant

#Other satisfaction measure -> clearly no differences at all between the two groups regarding their article ratings
with(users, shapiro.test(avg_rating2[group == "Similarity Rec"]))# p = 0.54
with(users, shapiro.test(avg_rating2[group == "Topic Rec"])) # p = 0.35]
with(users, shapiro.test(avg_rating2[group == "Customization"])) # p = 0.059

qualtrics_t_test = users %>% filter(group == "Topic Rec" | group == "Similarity Rec")
res.ftest = var.test(avg_rating ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

res.ftest = var.test(avg_rating2 ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating2 ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

#Comparing to customization group 

qualtrics_t_test = users %>% filter(group == "Topic Rec" | group == "Customization")
res.ftest = var.test(avg_rating ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

res.ftest = var.test(avg_rating2 ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating2 ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

qualtrics_t_test = users %>% filter(group == "Similarity Rec" | group == "Customization")
res.ftest = var.test(avg_rating ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

res.ftest = var.test(avg_rating2 ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating2 ~ group, var.equal = FALSE, data = qualtrics_t_test)
res

qualtrics_t_test = users %>% filter(group_ttest == "Rec" | group_ttest == "Customization")
res.ftest = var.test(avg_rating~group_ttest, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating ~ group_ttest, var.equal = FALSE, data = qualtrics_t_test)
res

res.ftest = var.test(avg_rating2 ~ group, data = qualtrics_t_test)
res.ftest

res = t.test(avg_rating2 ~ group_ttest, var.equal = FALSE, data = qualtrics_t_test)
res



#Other satisfaction measure (coming back to the website) -> for both no differences

with(users, shapiro.test(beh_int[group == "Similarity Rec"]))# p = 0.000 -> violates normality assumption for T-test
with(users, shapiro.test(beh_int[group == "Topic Rec"])) # p = 0.007 -> violates normality assumption for T-test
with(users, shapiro.test(beh_int[group == "Customization"])) # p = 0.000 -> violates normality assumption for T-test


qualtrics_t_test = users %>% filter(group == "Topic Rec" | group == "Similarity Rec")

res = wilcox.test(beh_int ~ group, data = qualtrics_t_test, exact = FALSE)
res

qualtrics_t_test = users %>% filter(group == "Customization" | group == "Similarity Rec")

res = wilcox.test(beh_int ~ group, data = qualtrics_t_test, exact = FALSE)
res

qualtrics_t_test = users %>% filter(group == "Customization" | group == "Topic Rec")

res = wilcox.test(beh_int ~ group, data = qualtrics_t_test, exact = FALSE)
res

qualtrics_t_test = users %>% filter(group_ttest == "Customization" | group_ttest == "Rec")

res = wilcox.test(beh_int ~ group_ttest, data = qualtrics_t_test, exact = FALSE)
res

#_________________________________________________________Tests Perceived Diversity_________________________________________________________________________

#Diversity Item 1

ggline(users, x = "group", y = "Diversity_1", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "System control", xlab = "Treatment")

res.aov <- aov(Diversity_1 ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Diversity_1 ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Diversity_1 ~ group, data = users)
pairwise.wilcox.test(users$Diversity_1, users$group,
                     p.adjust.method = "BH")
eta_sq(res.aov)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), marginally significant, power at ~..% (etasquared 0.032, effect size f 0.2015)
#------------------------->But: Pairwise comparisons: no significant differences, only group 2 and 4 marginally significant

#Diversity Item 2


ggline(users, x = "group", y = "Diversity_2", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "System control", xlab = "Treatment")

res.aov <- aov(Diversity_2 ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Diversity_2 ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Diversity_2 ~ group, data = users)
pairwise.wilcox.test(users$Diversity_2, users$group,
                     p.adjust.method = "BH")
eta_sq(res.aov)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), no significance

#Diversity Item 3


ggline(users, x = "group", y = "Diversity_3", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "System control", xlab = "Treatment")

res.aov <- aov(Diversity_3 ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Diversity_3 ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Diversity_3 ~ group, data = users)
pairwise.wilcox.test(users$Diversity_3, users$group,
                     p.adjust.method = "BH")
eta_sq(res.aov)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), no significance

#Diversity Item 4


ggline(users, x = "group", y = "Diversity_4", 
       add = c("mean_se", "jitter"), 
       order = c("1", "2", "3", "4"),
       ylab = "System control", xlab = "Treatment")

res.aov <- aov(Diversity_4 ~ group, data = users)

summary(res.aov)
TukeyHSD(res.aov)
leveneTest(Diversity_4 ~ group, data = users)
plot(res.aov, 2)
aov_residuals <- residuals(object = res.aov )
shapiro.test(x = aov_residuals )
kruskal.test(Diversity_4 ~ group, data = users)
pairwise.wilcox.test(users$Diversity_4, users$group,
                     p.adjust.method = "BH")
eta_sq(res.aov)

#------------------------->Using Kruskal-Wallis test (normality assumptions violated), no significance

#___________________________________________________________________________Tests Third Wave_________________________________________________________________
load(file = "qualtrics_third_wave.Rda")

#Two groups that got new feature (not interested in control group at the moment)
qualtrics_third_wave_nc = qualtrics_third_wave %>% filter(group == 2 | group == 3)
qualtrics_third_wave_nc$fake = as.factor(qualtrics_third_wave_nc$fake)
  
ggline(qualtrics_third_wave_nc, x = "fake", y = "system_control_third", 
       add = c("mean_se", "jitter"), 
       ylab = "System control", xlab = "fake")

ggline(qualtrics_third_wave_nc, x = "fake", y = "strategy_control_third", 
       add = c("mean_se", "jitter"), 
       ylab = "Strategy Control", xlab = "fake")

ggline(qualtrics_third_wave_nc, x = "fake", y = "satisfaction_third", 
       add = c("mean_se", "jitter"), 
       ylab = "Satisfaction", xlab = "fake")

ggline(qualtrics_third_wave_nc, x = "fake", y = "system_control_diff", 
       add = c("mean_se", "jitter"), 
       ylab = "Want to visit again", xlab = "Group")

ggline(qualtrics_third_wave_nc, x = "fake", y = "strategy_control_diff", 
       add = c("mean_se", "jitter"), 
       ylab = "Want to visit again", xlab = "Group")

background = rgb(249/255, 235/255, 224/255, 1)
dark_blue = rgb(13/255, 33/255, 73/255, 1)
vu_blue = rgb(0/255, 137/255, 207/255, 1)

ggline(qualtrics_third_wave_nc, x = "fake", y = "satisfaction_diff", 
       add = c("mean_se", "jitter"), 
       ylab = "Satisfaction Difference", xlab = "Fake") +
       theme(legend.position="none") +
         theme(axis.title = element_text(colour = dark_blue, size = 15), axis.text = element_text(color = dark_blue,size = 15), axis.ticks = element_line(color = dark_blue))

test = gather(qualtrics_third_wave_nc, `satisfaction`, `satisfaction_third`, key = "wave", value = "satisfaction")
library(plyr)
test$wave = revalue(test$wave, c("satisfaction" = "first", "satisfaction_third" = "second"))

interaction.plot(x.factor = test$wave, trace.factor = test$fake, 
                 response = test$satisfaction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Measurement", ylab="Satisfaction", trace.label = 'Fake',
                 pch=c(1,19))       

with(qualtrics_third_wave_nc, shapiro.test(satisfaction_diff[fake == "0"]))# p = 0.14
with(qualtrics_third_wave_nc, shapiro.test(satisfaction_diff[fake == "1"])) # p = 0.054 -> on the edge of violating normality assumption

res = wilcox.test(satisfaction_diff ~ fake, data = qualtrics_third_wave_nc, exact = FALSE)
res

res.ftest = var.test(satisfaction_diff ~ fake, data = qualtrics_third_wave_nc)
res.ftest

res = t.test(satisfaction_diff ~ fake, var.equal = FALSE, data = qualtrics_third_wave_nc)
res

cohen.d(qualtrics_third_wave_nc$satisfaction_diff, factor(qualtrics_third_wave_nc$fake))

#Significant difference regarding satisfaction difference (w3-w2) between fake/real control () (t-test), power of 67%

bayes.t.test(qualtrics_third_wave_nc$satisfaction_diff[qualtrics_third_wave_nc$fake == 0], qualtrics_third_wave_nc$satisfaction_diff[qualtrics_third_wave_nc$fake == 1])

#Also Bayesian: "Difference of the means is greater than 0 by a probability of 0.972. 
#Interpretation: For those in the fake control condition the satisfaction went down a bit compared to the last time, while for those in the real control condition
#it stayed about the same (only went up a little bit)

bayes.t.test(qualtrics_third_wave_nc$system_control_diff[qualtrics_third_wave_nc$fake == 0], qualtrics_third_wave_nc$system_control_diff[qualtrics_third_wave_nc$fake == 1])

bayes.t.test(qualtrics_third_wave_nc$strategy_control_diff[qualtrics_third_wave_nc$fake == 0], qualtrics_third_wave_nc$strategy_control_diff[qualtrics_third_wave_nc$fake == 1])


#However, regarding feelings of control, the probabilities rather point towards no differences -> for both measures it went up in both groups

g3_satisfaction_2 = qualtrics_third_wave_nc$satisfaction[qualtrics_third_wave_nc$group.x == 3]
g3_satisfaction_3 = qualtrics_third_wave_nc$satisfaction_third[qualtrics_third_wave_nc$group.x == 3]
bayes.t.test(g3_satisfaction_2, g3_satisfaction_3, paired = TRUE)

g2_satisfaction_2 = qualtrics_third_wave_nc$satisfaction[qualtrics_third_wave_nc$group.x == 2]
g2_satisfaction_3 = qualtrics_third_wave_nc$satisfaction_third[qualtrics_third_wave_nc$group.x == 2]
bayes.t.test(g2_satisfaction_2, g2_satisfaction_3, paired = TRUE)

bayes.t.test(qualtrics_third_wave_nc$satisfaction, qualtrics_third_wave_nc$satisfaction_third, paired = TRUE)

#No differences between the waves when looking at (1) the groups separately (but not taking into account fake/not fake) and (2) overall (both groups)

#Next step: Repeated measures ANOVA (have to change the dataframe for that since the different measures should be in long format and not wide)

strat_control = qualtrics_third_wave_nc %>% gather(key = measurement, value = strat_control, c(strategy_control_third, strategy_control))
table(strat_control$measurement)

anovaModelRM = aov(strat_control ~ group*fake*measurement + Error(id), strat_control)
summary(anovaModelRM)

sys_control = qualtrics_third_wave_nc %>% gather(key = measurement, value = sys_control, c(system_control_third, system_control))
table(sys_control$measurement)

anovaModelRM = aov(sys_control~ group*fake*measurement + Error(id), sys_control)
summary(anovaModelRM)


satis = qualtrics_third_wave_nc %>% gather(key = measurement, value = satis, c(satisfaction_third, satisfaction))
table(satis$measurement)

anovaModelRM = aov(satis~ fake*group*measurement + Error(id), satis)
summary(anovaModelRM)

satis$id = as.factor(satis$id)
satis$fake = as.factor(satis$fake)
satis$group = as.factor(satis$group)
satis$measurement = as.factor(satis$measurement)

bf = anovaBF(satis~ fake*group + id, data = satis, whichModels="withmain", whichRandom="id")
plot(bf)
#_____________________________________________________________________________Exploratory_________________________________________________________________________
#Differences between the two ratings
ggplot(selected_news, mapping = aes(x = rating, y = rating2)) + geom_jitter() + geom_smooth()
bar_plot = ggplot(selected_news, aes(x = factor(rating), fill = factor(rating2)))
bar_plot + geom_bar()

ggplot(selected_news, aes(x = factor(rating2)))  +
  geom_bar(alpha = 0.8) + 
  facet_wrap(~ factor(rating), ncol = 4)

ggplot(selected_news, mapping = aes(x = rating - rating2)) + geom_bar()
psych::describe(selected_news)


#___________________________________________________________Bayesian?____________________________________________________________________________________________
library(rjags)
library(ggmcmc)
library(polspline)
library(propagate)
library(multcomp)
library(DT)
data=list(y=users$system_control,
          ind=as.numeric(users$group),
          N=length(users$system_control),
          p=length(levels(users$group)),
          overall_mean=mean(users$system_control))

pooled_var="
  model {
      #######  Likelihood
      for (i in 1:N) {                    # Loop through observations
        mu[i]<-Beta[ind[i]]               # The expected values are just the group means
        y[i] ~ dnorm(mu[i],tau)           # Values treated as from a single normal
       
      }
     ############## Uninformative priors
    for (j in 1:p) {
     Beta[j]~dnorm(0,0.0001)
   
     Effect[j]<-Beta[j]-overall_mean  ### Calculate difference from overall mean
     ################### Calculate pair wise differences 
      for (n in 1:(j-1)){
        Difbeta[n,j]<-Beta[n]-Beta[j]
      }
    }
    
    tau ~ dgamma(scale, rate) ## Prior for normal distribution precision.
    scale ~ dunif(0, 1)       ### Hyper parameters for tau.
    rate ~ dunif(0, 1)
    
  }
"
model=jags.model(textConnection(pooled_var),data=data)

update(model,n.iter=1000)
output=coda.samples(model=model,variable.names=c("Difbeta","Effect"),n.iter=100000,thin=10)

ms <-ggs(output) 
mt<-filter(ms,grepl("Difbeta",Parameter))
ggs_caterpillar(mt) +geom_vline(xintercept = 0,col="red")

mt<-filter(ms,grepl("Effect",Parameter))
ggs_caterpillar(mt) +geom_vline(xintercept = 0,col="red")


#Testing Two way Anova with education 
library(car)
my_anova <- lm(control_overall ~ group * edu_1, data = users, contrasts = list(group = contr.sum, edu_1 = contr.sum))
res = Anova(my_anova, type = "III")
eta_sq(res)
res

#Doing pairwise comparisons
library(emmeans)
em_out_category <- emmeans(my_anova,  ~ edu_1 | group)
print(em_out_category)
em_out_category %>%
  pairs() %>%
  test()

em_out_category <- emmeans(my_anova,  ~ group | edu_1)
print(em_out_category)

em_out_category %>%
  pairs() %>%
  test(joint = TRUE)

em_out_category %>%
  pairs() %>%
  test()


ggboxplot(users, x = "group", y = "control_overall", color = "edu_1")

ggline(users, x = "group", y = "control_overall", color = "edu_1",
       add = c("mean_se", "dotplot"))

interaction.plot(x.factor = users$group, trace.factor = users$edu_1, 
                 response = users$control_overall, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Group", ylab="Control Overall",
                 pch=c(1,19))

interaction.plot(x.factor = users$group, trace.factor = users$edu_1, 
                 response = users$satisfaction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Group", ylab="Satisfaction",
                 pch=c(1,19))

interaction.plot(x.factor = users$group, trace.factor = users$edu_1, 
                 response = users$beh_int, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Group", ylab="Behavioral Intention",
                 pch=c(1,19))


my_anova <- lm(satisfaction ~ group * edu_1, data = users, contrasts = list(group = contr.sum, edu_1 = contr.sum))
res = Anova(my_anova, type = "III")
eta_sq(res)
res

em_out_category <- emmeans(my_anova,  ~ edu_1 | group)
print(em_out_category)
em_out_category %>%
  pairs() %>%
  test(joint = TRUE)

em_out_category %>%
  pairs() %>%
  test()

em_out_category <- emmeans(my_anova,  ~ group | edu_1)
print(em_out_category)
em_out_category %>%
  pairs() %>%
  test(joint = TRUE)

em_out_category %>%
  pairs() %>%
  test()

my_anova <- lm(beh_int ~ group * edu_1, data = users, contrasts = list(group = contr.sum, edu_1 = contr.sum))
res = Anova(my_anova, type = "III")
eta_sq(res)
res

em_out_category <- emmeans(my_anova,  ~ edu_1 | group)
print(em_out_category)
em_out_category %>%
  pairs() %>%
  test(joint = TRUE)

em_out_category <- emmeans(my_anova,  ~ group | edu_1)
print(em_out_category)
em_out_category %>%
  pairs() %>%
  test(joint = TRUE)

#Testing Two way Anova with political knowledge

interaction.plot(x.factor = users$group, trace.factor = users$pol_knowledge_1, 
                 response = users$control_overall, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Group", ylab="Control Overall",
                 pch=c(1,19))

interaction.plot(x.factor = users$group, trace.factor = users$pol_knowledge_1, 
                 response = users$satisfaction, fun = mean, 
                 type = "b", legend = TRUE, 
                 xlab = "Group", ylab="Control Overall",
                 pch=c(1,19))

my_anova <- aov(control_overall ~ group * pol_knowledge_1, data = users, contrasts = list(group = contr.sum, pol_knowledge_1 = contr.sum))
res = Anova(my_anova, type = "III")
eta_sq(res)
res

my_anova <- aov(satisfaction ~ group * pol_knowledge_1, data = users, contrasts = list(group = contr.sum, pol_knowledge_1 = contr.sum))
res = Anova(my_anova, type = "III")
eta_sq(res)
res


