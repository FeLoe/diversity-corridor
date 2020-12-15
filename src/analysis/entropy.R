d = readRDS('data/intermediate/analysis_backup.rds')
u = read_csv('data/tmp/users_new.csv') 
  
entropy = function(x) {p=x/sum(x); -sum(ifelse(p==0, 0, p*log(p, 2)))}
dpu1 = d %>% filter(selected==1) %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(diversity=entropy(n))
dpu2 = d %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(expdiversity=entropy(n)) 
dpu = dpu1 %>% inner_join(dpu2) %>% inner_join(u)
install.packages("hrbrthemes")
dpu %>% ggplot(aes(x=div_trait, y=diversity)) + geom_jitter(alpha=.5,  height=0, width=.1) + 
  geom_smooth(method=lm , size=.5, color="red", fill="red", alpha=.2, se=TRUE) +
  facet_grid(rows=vars(group)) + hrbrthemes::theme_ipsum() + 
  ggtitle("Consumption diversity by div_trait per experimental group")

lm(data=dpu, diversity ~ as.factor(group) + gender + age + edu + pol_knowledge + 
     overconfidence + political_efficacy + pol_interest + div_trait + div_trait*as.factor(group)
) %>% summary()
lm(data=dpu, expdiversity ~ as.factor(group) + gender + age + edu + pol_knowledge + 
     overconfidence + political_efficacy + pol_interest + div_trait + div_trait*as.factor(group)
) %>% summary()

cormat = inner_join(dpu, u) %>%  select(diversity, group:div_trait2, Diversity_1:Diversity_4) %>% cor()
cormat %>% as_tibble(rownames = "var") %>% select(var, diversity)
cormat %>% as_tibble(rownames = "var") %>% pivot_longer(-var, names_to="var2") %>% ggplot(aes(x=var,y=var2,fill=value))


entropy(c(23, 14, 8, 6, 19, 22))

