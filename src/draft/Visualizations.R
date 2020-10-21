#Colors 
background = rgb(249/255, 235/255, 224/255, 1)
dark_blue = rgb(13/255, 33/255, 73/255, 1)
vu_blue = rgb(0/255, 137/255, 207/255, 1)

ggplot(data = qualtrics_first) + geom_bar(aes(x = overconfidence))
ggplot(data = qualtrics_first) + geom_bar(aes(x = political_efficacy))
ggplot(data = qualtrics_first) + geom_bar(aes(x = pol_interest))
ggplot(data = qualtrics_first) + geom_bar(aes(div_trait))
ggplot(data = qualtrics_first) + geom_bar(aes(div_trait2))
ggplot(data = qualtrics_finished) + geom_histogram(aes(number_stories_read))

sessions_grouped = sessions %>% group_by(session_id) %>% summarize(position_selected = first(position_selected))

#Position: Most news that were clicked were on position 0 (21.4%, a fifth of all news selections)
ggplot(data = sessions_grouped) + geom_bar(aes(position_selected), fill = dark_blue, na.rm = TRUE) + 
  xlab("Position of news story") + ylab("Amount") + 
  theme(panel.background = element_rect(fill = background, colour = background), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title = element_text(colour = dark_blue, size = 15), axis.text = element_text(color = dark_blue,size = 15), axis.ticks = element_line(color = dark_blue))

ggplot(data = actual_diversity) + geom_histogram(aes(act_div), na.rm = TRUE, bins = 70)


group.labs <-c("Random", "Recommender Similarity", "Recommender Topic", "Customization")
names(group.labs) <-  c("1", "2", "3", "4")

ggplot(diversity_comparison, aes(act_div, div_trait, color=div_trait)) +
  geom_jitter(height = 0) + 
  ggtitle("Actual Diversity vs. Diversity Trait") +
  ylab('Diversity Trait') + xlab("Actual Diversity")

ggplot(diversity_comparison, aes(act_div, Diversity_1, color=Diversity_1)) +
  geom_jitter(height = 0) + 
  ggtitle("Actual Diversity vs. 'De artikelen bevatten gemiddeld veel variatie'") + 
  ylab('Perceived Diversity') + xlab("Actual Diversity")

ggplot(diversity_comparison, aes(act_div, Diversity_2, color=Diversity_2)) +
  geom_jitter(height = 0) + 
  ggtitle("Actual Diversity vs. Perceived Diversity (2)")

ggplot(diversity_comparison, aes(Diversity_2, group, color=group)) +
  geom_jitter(height = 0) + 
  ggtitle("'De artikelen op één pagina waren vergelijkbaar met elkaar' vs. Group") +
  ylab('Group') + xlab("Perceived Diversity")

ggplot(diversity_comparison, aes(Diversity_3, group, color=group)) +
  geom_jitter(height = 0) + 
  ggtitle("'De artikelen omvatten vele genres' vs. Group") +
  ylab('Group') + xlab("Perceived Diversity")

ggplot(diversity_comparison, aes(Diversity_4, group, color=group)) +
  geom_jitter(height = 0) + 
  ggtitle("'De meeste artikelen behandelden \n soortgelijke onderwerpen' vs. Group") +
  ylab('Group') + xlab("Perceived Diversity")

plot1 = ggplot(diversity_comparison, aes(act_div, group, color=group)) +
  geom_jitter(height = 0) + 
  xlab("Actual Diversity") +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(size = 12)) + 
  theme(legend.position="none") +
  #theme(panel.background = element_rect(fill = background, colour = background)) +
  theme(axis.title = element_text(colour = dark_blue, size = 15), axis.text = element_text(color = dark_blue,size = 15), axis.ticks = element_line(color = dark_blue)) +
  scale_y_discrete(
    labels=c("Random", "Similarity Rec", "Topic Rec", "Customization")
  ) +
  theme(axis.text.y = element_text(angle = 60, size = 14))


plot2 = ggplot(diversity_comparison, aes(Diversity_4, group, color=group)) +
  geom_jitter(height = 0) + 
  xlab("'De artikelen omvatten vele genres'") +
  theme(axis.title.y=element_blank()) +
  theme(plot.title = element_text(size = 12)) +
  theme(legend.position="none") +
  #theme(panel.background = element_rect(fill = background, colour = background)) +
  theme(axis.title = element_text(colour = "blue", size = 15), axis.text = element_text(color = "blue",size = 15), axis.ticks = element_line(color = "blue")) +
  scale_y_discrete(
    labels=c("Random", "Similarity Rec", "Topic Rec", "Customization")
  ) +
  theme(axis.text.y = element_text(angle = 60, size = 14))

grid.arrange(plot1, plot2, ncol=2)

ggplot(diversity_comparison, aes(Diversity_4, act_div)) +
  geom_jitter(height = 0) + 
  ggtitle("'De artikelen omvatten vele genres' vs 'Actual Diversity") + 
  ylab('Actual Diversity') + xlab("Perceived Diversity") + facet_wrap(~ group, ncol = 2, labeller = labeller(group = group.labs))

ggplot(data = logins) + geom_bar(aes(x = forcats::fct_infreq(browser_group)))
ggplot(data = logins) + geom_bar(aes(x = forcats::fct_infreq(os_group)))
ggplot(data = logins) + geom_bar(aes(x = forcats::fct_infreq(devices_group)))

mobile_pc = sessions_users %>% filter(devices_group == "PC" | devices_group == 'mobile')
ggplot(data = mobile_pc) + 
  geom_bar(aes(fill = devices_group, position_selected), na.rm = TRUE, position = "dodge")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

sessions_users_sum = sessions_users %>% group_by(user_id) %>% 
  summarize(mean_sel = mean(position_selected), mode = Mode(position_selected), )
ggplot(data = sessions_users_sum) + geom_histogram(aes(mean_sel), bins = 50)
ggplot(data = sessions_users_sum) + geom_bar(aes(mode))