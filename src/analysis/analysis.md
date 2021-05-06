Analysis for Paper ‘Is this a click towards diversity?’
================
Felicia Loecherbach, Kasper Welbers, Judith Moeller, Damian Trilling,
Wouter van Atteveldt

``` r
library(ggplot2)
library(psych)
library(ggstatsplot)
library(lme4)
library(sjPlot)
library(tidyverse)
```

## 4.1 Usage of the system

### Topics

#### Topics presented

``` r
d = readRDS('../../data/intermediate/analysis_backup.rds')
knitr::kable(table(d$topic))
```

| Var1          |  Freq |
|:--------------|------:|
| Anders        | 29172 |
| Binnenland    | 41877 |
| Buitenland    | 15946 |
| Economie      | 10250 |
| Entertainment | 33192 |
| Immigratie    |   490 |
| Justitie      | 42932 |
| Milieu        |  2087 |
| Sport         | 55922 |
| Wetenschap    |  2870 |

#### Topics chosen

``` r
sel = d %>% filter(selected == '1')
knitr::kable(table(sel$topic))
```

| Var1          | Freq |
|:--------------|-----:|
| Anders        | 3542 |
| Binnenland    | 5368 |
| Buitenland    | 1887 |
| Economie      | 1319 |
| Entertainment | 3621 |
| Immigratie    |   61 |
| Justitie      | 5587 |
| Milieu        |  311 |
| Sport         | 3909 |
| Wetenschap    |  477 |

#### Filter out small topics

``` r
#Rename small topics
d = d %>%   
  mutate(topic = recode(topic, 
                        Wetenschap = 'Anders',
                        Immigratie = 'Anders',
                        Milieu = 'Anders'))

# remove 'Anders' topic
anders_id = d$exposure_id[d$topic == 'Anders' & d$selected == 1]
d = d %>%
  filter(!exposure_id %in% anders_id) %>%
  filter(topic != 'Anders')
```

### Exposures, Sessions

#### How many exposures, how many sessions, length of sessions?

``` r
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
t1 = describe(exp_sess, skew = F)
knitr::kable(t1)
```

|              | vars |   n |       mean |         sd |       min |        max |      range |         se |
|:-------------|-----:|----:|-----------:|-----------:|----------:|-----------:|-----------:|-----------:|
| user\_id     |    1 | 247 | 501.052632 | 339.781137 | 15.000000 | 1173.00000 | 1158.00000 | 21.6197560 |
| exp\_group\* |    2 | 247 |   2.380567 |   1.020574 |  1.000000 |    4.00000 |    3.00000 |  0.0649376 |
| exp          |    3 | 247 |  87.817814 |  65.044850 | 23.000000 |  456.00000 |  433.00000 |  4.1387047 |
| sess         |    4 | 247 |  16.951417 |   9.382005 |  6.000000 |   74.00000 |   68.00000 |  0.5969627 |
| len          |    5 | 247 |   5.579805 |   3.744642 |  1.555556 |   37.66667 |   36.11111 |  0.2382659 |

#### Length of sessions over time?

``` r
overtime = d %>% 
  filter(selected == 1) %>%
  group_by(user_id,exp_group, session) %>%
  summarize(n = n()) %>%
  group_by(session) %>%
  summarize(sess_length = mean(n))
t2 = describe(overtime, skew = F)
knitr::kable(t2)
```

|              | vars |   n |      mean |       sd | min | max | range |        se |
|:-------------|-----:|----:|----------:|---------:|----:|----:|------:|----------:|
| session      |    1 |  78 | 39.500000 | 22.66054 |   1 |  78 |    77 | 2.5658007 |
| sess\_length |    2 |  78 |  3.902576 |  2.38252 |   1 |  14 |    13 | 0.2697673 |

``` r
p1 = ggplot(overtime, mapping = aes(x = session, y = sess_length)) + geom_point() + geom_smooth()
plot(p1)
```

![](analysis_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

## 4.2 Positioning Effects

*RQ1*: To what extent can positioning effects be found in news selection
and how consistent are they across (1) preferences, (2) topics, and (3)
devices.

### In general

``` r
sel = d %>% 
  group_by(position, selected) %>%
  summarize(n = n()) %>%
  group_by(position) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1) %>%
  add_column(condition = "Overall", .before = "position")
```

### Per topic

``` r
sel_top = d %>% 
  group_by(topic, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, topic) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1) %>%
  dplyr::rename(condition = topic)

labels_topics <- c("Binnenland" = "Domestic news", "Buitenland" = "Foreign news", "Economie" = "Economy", "Entertainment" = "Entertainment", 
                   "Justitie" = "Crime", "Sport" = "Sports")
```

### Per preference

``` r
sel_pref = d %>%
  group_by(topic_pref, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, topic_pref) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1) %>%
  dplyr::rename(condition = topic_pref)

sel_pref = sel_pref %>% ungroup() %>% 
  mutate(condition = case_when(condition == 1 ~ "P: low", 
                               condition == 2 | condition == 3 | 
                                 condition == 4 ~ "P: medium", 
                               condition == 5 | condition == 6 | 
                                 condition == 7 ~ "P: high" )) %>%
  group_by(position, condition) %>%
  summarise(p = mean(p))
```

### Per device used

``` r
sel_dev = d %>% 
  group_by(devices_group, position, selected) %>%
  summarize(n = n()) %>%
  group_by(position, devices_group) %>%
  mutate(p = n[selected == 1]/n[selected == 0]) %>%
  filter(selected == 1) %>%
  dplyr::rename(condition = devices_group)
```

### Everything combined

``` r
sel_all = rbind(sel, sel_top, sel_dev, sel_pref)
sel_all = sel_all %>% select(-c(selected, n)) %>% mutate(position = as.factor(position))

sel_all = sel_all %>% mutate(condition = case_when(condition == 'tablet' ~ 'D: Tablet', 
                                                   condition == 'mobile' ~ 'D: Mobile', 
                                                   condition == 'PC' ~ 'D: PC', 
                                                   condition == 'Justitie' ~ 'T: Crime',
                                                   condition == 'Economie' ~ 'T: Economics',
                                                   condition == 'Entertainment' ~ 'T: Entertain',
                                                   condition == 'Buitenland' ~ 'T: Foreign',
                                                   condition == 'Binnenland' ~ 'T: Domestic',
                                                   condition == 'Sport' ~ 'T: Sport', 
                                                   TRUE ~ condition))
sel_all$condition = factor(sel_all$condition, levels = 
                             c("Overall", "D: Mobile", "D: Tablet", "D: PC",
                               "T: Sport", 'T: Crime', 'T: Domestic', 
                               'T: Entertain', 'T: Foreign', 'T: Economics', 
                               "P: low", "P: medium", "P: high"))


p2 = ggplot(data = sel_all, aes(x = position, y = reorder(condition, desc(condition)))) + 
  geom_tile(aes(fill = p)) + geom_text(aes(label = round(p, 2), color = p > 0.2, fontface = 2)) + 
  scale_fill_viridis_b(direction = -1, "Proportion selected", guide = guide_legend(label.vjust = 0.2)) + 
  labs(x = "Position", y = "") + 
  theme_classic(base_size = 13) +
  theme(legend.position="bottom") +
  scale_color_manual(guide = FALSE, values = c("black", "white")) + 
  geom_hline(aes(yintercept = 9.5), size = 1, color = "#450B59") + 
  geom_hline(aes(yintercept = 3.5), size = 1, color = "#450B59") + 
  geom_hline(aes(yintercept = 12.5), size = 1, color = "#450B59")
plot(p2)
```

![](analysis_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
ggplot(data = sel_all, aes(x = position, y = reorder(condition, desc(condition)))) + 
  geom_tile(aes(fill = p)) + geom_text(aes(label = round(p, 2), color = p > 0.2, fontface = 2)) + 
  scale_fill_gradient(low = "white", high = "black", "Proportion selected", guide = guide_legend(label.vjust = 0.2)) +
  labs(x = "Position", y = "") + 
  theme_classic(base_size = 13) +
  theme(legend.position="bottom") +
  scale_color_manual(guide = FALSE, values = c("black", "white")) + 
  geom_hline(aes(yintercept = 9.5), size = 1, color = "black") + 
  geom_hline(aes(yintercept = 3.5), size = 1, color = "black") + 
  geom_hline(aes(yintercept = 12.5), size = 1, color = "black")
```

![](analysis_files/figure-gfm/unnamed-chunk-13-1.png)<!-- -->

## 4.3 Control

*RQ2*: To what extent does news personalisation (explicit and implicit)
impact the amount and length of browsing sessions?

### Differences in sessions, selections, lengths of sessions between groups

``` r
knitr::kable(describeBy(exp_sess[3:5],exp_sess$exp_group, skew = F, mat = T, ranges = F))
```

|       | item | group1 | vars |   n |       mean |        sd |         se |
|:------|:-----|:-------|-----:|----:|-----------:|----------:|-----------:|
| exp1  | 1    | random |    1 |  57 | 124.035088 | 84.865432 | 11.2407011 |
| exp2  | 2    | rec\_A |    1 |  81 |  84.654321 | 46.678464 |  5.1864960 |
| exp3  | 3    | rec\_B |    1 |  67 |  75.000000 | 66.916502 |  8.1751518 |
| exp4  | 4    | custom |    1 |  42 |  65.214286 | 38.118900 |  5.8818739 |
| sess1 | 5    | random |    2 |  57 |  19.631579 |  9.178063 |  1.2156642 |
| sess2 | 6    | rec\_A |    2 |  81 |  18.407407 | 10.645161 |  1.1827956 |
| sess3 | 7    | rec\_B |    2 |  67 |  15.880597 |  8.668163 |  1.0589846 |
| sess4 | 8    | custom |    2 |  42 |  12.214286 |  5.654852 |  0.8725626 |
| len1  | 9    | random |    3 |  57 |   7.158523 |  6.252330 |  0.8281414 |
| len2  | 10   | rec\_A |    3 |  81 |   5.078690 |  2.147253 |  0.2385836 |
| len3  | 11   | rec\_B |    3 |  67 |   4.799815 |  2.497223 |  0.3050843 |
| len4  | 12   | custom |    3 |  42 |   5.647963 |  2.597204 |  0.4007573 |

``` r
exp_sess = exp_sess %>% ungroup()

p3 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = sess,
  type = "np",
  title = "Differences in number of sessions between groups", 
  xlab = "Experimental Groups", 
  ylab = "Sessions",
  pairwise.display = "all"
)

plot(p3)
```

![](analysis_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
p4 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = exp,
  type = "np",
  title = "Differences in number of selections between groups", 
  xlab = "Experimental Groups", 
  ylab = "Selections", 
  pairwise.display = "all"
)

plot(p4)
```

![](analysis_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
p5 = ggbetweenstats(
  data = exp_sess,
  x = exp_group,
  y = len,
  type = "np",
  title = "Differences in length of sessions between groups", 
  xlab = "Experimental Groups", 
  ylab = "Length"
)

plot(p5)
```

![](analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

### Differences in diversity between groups

``` r
entropy = function(x) {p=x/sum(x); -sum(ifelse(p==0, 0, p*log(p, 2)))}
dpu1 = d %>% filter(selected==1) %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(diversity=entropy(n))
dpu2 = d %>% group_by(user_id, topic) %>% summarize(n=n()) %>% 
  summarize(expdiversity=entropy(n)) 
u = d %>% ungroup() %>% select(1, 12:24) %>% distinct()
dpu = dpu1 %>% inner_join(dpu2) %>% inner_join(u)

dpu = dpu %>% ungroup()

p6 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = diversity,
  type = "np",
  title = "Differences in consumption diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Consumption Diversity"
)
plot(p6)
```

![](analysis_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

``` r
p7 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = expdiversity,
  type = "np",
  title = "Differences in exposure diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Exposure Diversity"
)
plot(p7)
```

![](analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
dpu = dpu %>% mutate(perc_div = (Diversity_1 + Diversity_2 + Diversity_3 +  Diversity_4)/4)

p8 = ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = perc_div,
  type = "np",
  title = "Differences in perceived diversity between groups", 
  xlab = "Experimental Groups", 
  ylab = "Perceived Diversity"
)
plot(p8)
```

![](analysis_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

### Baseline vs consumption diversity between groups

*RQ3*: To what extent does the amount of control affect the relationship
between the diversity of topics preferences (the baseline) and the
diversity of read topics?

#### Description topic preferences

``` r
topic_prefs = d %>% ungroup() %>% distinct(user_id, topic, topic_pref, topic_pref_dist) 
topic_prefs_s = topic_prefs %>% group_by(topic, topic_pref) %>% summarise(n = n())
ggplot(topic_prefs_s, aes(x = topic_pref, y = n)) + geom_bar(stat = "identity") + facet_wrap(~topic) 
```

![](analysis_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

#### Additional randomization check: Differences in diversity trait between groups

``` r
ggbetweenstats(
  data = dpu,
  x = exp_group,
  y = div_trait,
  type = "np",
  title = "Differences in diversity trait between groups", 
  xlab = "Experimental Groups", 
  ylab = "Diversity Trait"
)
```

![](analysis_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

#### Baseline + experimental group predicting consumption diversity

``` r
m1 = lm(data=dpu, diversity ~ as.factor(exp_group) + gender + age + edu + pol_knowledge + 
     pol_interest + div_trait + div_trait*as.factor(exp_group))
tab_model(m1)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="3" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
diversity
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Estimates
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
CI
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
p
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.27
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.04 – 2.51
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>&lt;0.001
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[rec\_A\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.07 – 0.26
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.245
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[rec\_B\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.14
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.31 – 0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.105
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[custom\]
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.48 – -0.11
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.002</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
gender
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.09 – 0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.233
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
age
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.00 – 0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.809
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
edu
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.03 – 0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.899
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
pol\_knowledge
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.01 – 0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.525
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
pol\_interest
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.03 – 0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.406
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
div\_trait
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.01 – 0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.290
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[rec\_A\] \*<br>div\_trait
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.02
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.05 – 0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.195
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[rec\_B\] \*<br>div\_trait
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
-0.03 – 0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.914
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
exp\_group \[custom\] \*<br>div\_trait
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.04
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.01 – 0.08
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
<strong>0.023</strong>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="3">
247
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
R<sup>2</sup> / R<sup>2</sup> adjusted
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="3">
0.183 / 0.141
</td>
</tr>
</table>

``` r
p9 = dpu %>% ggplot(aes(x=div_trait, y=diversity, color = exp_group)) + geom_jitter(alpha=.5,  height=0, width=.1) + 
  geom_smooth(method=lm , size=.7, alpha=.2, se=TRUE) +
  ylab("Consumption diversity") + 
  xlab("Baseline diversity") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom") +
  scale_colour_viridis_d(name = "Groups", 
                        labels = c("Random", "Rec_A", "Rec_B", "Customized"), end = 0.9)
plot(p9)
```

![](analysis_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
dpu %>% ggplot(aes(x=div_trait, y=diversity, linetype = exp_group, shape = exp_group), color = "black") + geom_jitter(alpha=.5,  height=0, width=.1, color = "black") + 
  geom_smooth(method=lm , size=.5, alpha=.05, se=TRUE, color = "black") +
  ylab("Consumption diversity") + 
  xlab("Baseline diversity") + 
  theme_classic(base_size = 15) +
  theme(legend.position="bottom") +
  scale_shape_manual(values = c(3, 16, 2, 15),
                     name = "Groups", 
                     labels = c("Random", "Rec_A", "Rec_B", "Customized")) +
  scale_linetype_manual(values = c('longdash', 'dotted', 'twodash', 'solid'), name = "Groups", 
                        labels = c("Random", "Rec_A", "Rec_B", "Customized"))
```

![](analysis_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->

## 4.4 Integrated model

*RQ4*: Under which conditions does the selection of a topic in the
recent past reduce its likelihood of being selected again, showing
saturation effects?

*RQ5*: To what extent can content-wise and situational factors explain
topic choice?

``` r
ds = d %>% 
  group_by_at(vars(-position, -selected)) %>%
  summarize(position=mean(position), min_position=min(position), selected=sum(selected)) %>%
  ungroup() %>%
  filter(long_window_seen >= 5 & short_window_seen >= 1)

optim = control=glmerControl(optimizer="bobyqa",
                             optCtrl=list(maxfun=2e5))

m = list()
for (topic in unique(ds$topic)) {
  message(topic)
  m[[topic]] = glmer(selected ~ position + long_selected + short_selected + topic_pref + topic_in_exp + (1 | user_id), 
                     data=ds[ds$topic == topic,], family=binomial, control=optim)
}

pooled = glmer(selected ~ position + short_selected + long_selected + topic_pref + topic_in_exp + (1 | user_id) + (1 | topic), 
               data=ds, family=binomial, control=optim)

tab_model(c(m, pooled), dv.labels =c(names(m), 'Pooled'), show.aic=T, show.r2 = F, show.ci = F, p.style = 'stars', show.re.var=T)
```

<table style="border-collapse:collapse; border:none;">
<tr>
<th style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm;  text-align:left; ">
 
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Binnenland
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Entertainment
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Buitenland
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Sport
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Justitie
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Economie
</th>
<th colspan="1" style="border-top: double; text-align:center; font-style:normal; font-weight:bold; padding:0.2cm; ">
Pooled
</th>
</tr>
<tr>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  text-align:left; ">
Predictors
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  ">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col7">
Odds Ratios
</td>
<td style=" text-align:center; border-bottom:1px solid; font-style:italic; font-weight:normal;  col8">
Odds Ratios
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
(Intercept)
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.18 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.10 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.11 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.03 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.14 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
0.08 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
0.09 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
position
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.86 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.87 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.87 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.84 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.86 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
0.88 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
0.86 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
long\_selected
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.26 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
3.09 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
7.15 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.62 <sup></sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
2.16 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
2.67 <sup>\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
6.33 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
short\_selected
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.64 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.58 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.48 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.75 <sup>\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
0.71 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
0.41 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
0.83 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
topic\_pref
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.02 <sup></sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.07 <sup>\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.01 <sup></sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.34 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.10 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
1.10 <sup>\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
1.11 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; ">
topic\_in\_exp
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.77 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.82 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.94 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.65 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  ">
1.76 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col7">
2.17 <sup>\*\*\*</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:center;  col8">
1.68 <sup>\*\*\*</sup>
</td>
</tr>
<tr>
<td colspan="8" style="font-weight:bold; text-align:left; padding-top:.8em;">
Random Effects
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
σ<sup>2</sup>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
3.29
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
τ<sub>00</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.05 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.11 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.03 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.50 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.10 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.00 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.00 <sub>user\_id</sub>
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.05 <sub>topic</sub>
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
ICC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.01
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.13
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.03
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.00
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
0.02
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
N
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
245 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
237 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
220 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
244 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
244 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
185 <sub>user\_id</sub>
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
247 <sub>user\_id</sub>
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
 
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
6 <sub>topic</sub>
</td>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm; border-top:1px solid;">
Observations
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
10068
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
9036
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
4381
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
11596
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
10225
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
2670
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left; border-top:1px solid;" colspan="1">
47976
</td>
</tr>
<tr>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; text-align:left; padding-top:0.1cm; padding-bottom:0.1cm;">
AIC
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
11400.138
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
9073.681
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
4020.006
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
10360.177
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
11790.871
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
2496.989
</td>
<td style=" padding:0.2cm; text-align:left; vertical-align:top; padding-top:0.1cm; padding-bottom:0.1cm; text-align:left;" colspan="1">
49620.922
</td>
</tr>
<tr>
<td colspan="8" style="font-style:italic; border-top:double black; text-align:right;">

-   p&lt;0.05   \*\* p&lt;0.01   \*\*\* p&lt;0.001
    </td>
    </tr>

</table>

## 4.5 Perceived vs actual diversity

``` r
cor.test(dpu$expdiversity, dpu$diversity, 
         method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  dpu$expdiversity and dpu$diversity
    ## t = 8.4703, df = 245, p-value = 2.288e-15
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.3732837 0.5670613
    ## sample estimates:
    ##       cor 
    ## 0.4759282

``` r
cor.test(dpu$expdiversity, dpu$perc_div, 
         method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  dpu$expdiversity and dpu$perc_div
    ## t = 0.32158, df = 245, p-value = 0.748
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1045467  0.1449886
    ## sample estimates:
    ##        cor 
    ## 0.02054086

``` r
cor.test(dpu$diversity, dpu$perc_div, 
         method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  dpu$diversity and dpu$perc_div
    ## t = -0.2197, df = 245, p-value = 0.8263
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.1386114  0.1109792
    ## sample estimates:
    ##         cor 
    ## -0.01403471
