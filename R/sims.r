library(data.table)

topic_prob <- function(selective_exp) {
  selective_exp / sum(selective_exp)
}

position_prob <- function(n=9, bias=0.2) {
  geometric_prob = bias*(1-bias)^(1:n-1)   ## prob of choosing article on first sequential read
  no_decision_prob = 1-sum(geometric_prob) ## prob of not having chosen any
  geometric_prob + (no_decision_prob / n)  ## assume order then no longer matters
}

test_position_prob <- function(bias=0.2, exposures_csv = 'data/tmp/exposures.csv') {
  e = read_csv(exposures_csv, col_types=cols())
  pos = table(e$position[e$selected==1] + 1)
  plot(pos / sum(pos), xlab='position',ylab='percentage', type='l')
  lines(position_prob(9, bias=bias), lwd=2, lty=2)
  legend('topright', lty=c(1,2), legend = c('observed', 'estmated (bias = 0.2)'))
}

read_prob <- function(topic_prob, position_prob) {
  p = topic_prob * position_prob
  np = (1-topic_prob) * (1-position_prob)
  p / (p + np)
}

entropy <- function(x) -sum(x * log2(x))

sim <- function(selective_exp = c(0.1,0.1,0.1,0.1,0.6), positions=9, pos_bias=0.5, trials=10000, return_data=F) {
  k = length(selective_exp)
  tp = topic_prob(selective_exp)
  pp = position_prob(positions, pos_bias)
  d = data.frame(topic=numeric(trials), pos=numeric(trials))
  l = vector('list', trials)
  for (i in 1:trials) {
    topic_order = sample(1:k, size=positions, replace = T)    
    rp = read_prob(tp[topic_order], pp)
    d$pos[i] = sample(1:positions, size = 1, prob = rp)
    d$topic[i] = topic_order[d$pos[i]]
    if (return_data) {
      l[[i]] = data.frame(topic=topic_order, position=1:positions, topic_pref=tp[topic_order], selected=0)
      l[[i]]$selected[d$pos[i]] = 1
    }
  }
  sim_tp = as.numeric(table(d$topic) / nrow(d))
  sim_pos = as.numeric(table(d$pos) / nrow(d))
  list(d=rbindlist(l),
       topics = data.frame(topic=1:k, prior=tp, post=sim_tp, diff=tp-sim_tp),
       positions = data.frame(pos=1:positions, prior=pp, post=sim_pos, diff=pp-sim_pos),
       entropy = data.frame(before = entropy(tp), after= entropy(sim_tp)))
}

## position bias (pos_bias) indicates how much items
## If bias is 0, order doesn't matter, and if its 1, the first items is always chosen

## This shows the (in hindsight obvious) point, that if topics are randomly assigned positions
## then a stronger position bias will reduce the effect of selective exposure
## If there are clear outliers in selective exposure, 

sim(pos_bias=0.1)$topics
sim(pos_bias=0.5)$topics

sim(pos_bias=0.1)$entropy  ## with low position bias entropy decreases
sim(pos_bias=0.5)$entropy  ## with high position bias entropy increases

## it results in an interaction effect (though positive, since low position value means prominent position in this case)
s1 = sim(pos_bias=0.1, return_data = T)
s2 = sim(pos_bias=0.2, return_data = T)
s3 = sim(pos_bias=0.5, return_data = T)

library(lme4)
library(sjPlot)
m1 = glmer(selected ~ position + topic_pref + position*topic_pref + (1 | topic), family=binomial, data=s1$d)
m2 = glmer(selected ~ position + topic_pref + position*topic_pref + (1 | topic), family=binomial, data=s2$d)
m3 = glmer(selected ~ position + topic_pref + position*topic_pref + (1 | topic), family=binomial, data=s3$d)

tab_model(m1,m2,m3, dv.labels = c(paste('position bias =', c(0.1,0.2,0.5))))

## the reason is supposedly the geometric distribution of the position bias
## which I think makes sense, and also matches the data somewhat
test_position_prob()

## reading probability decreases exponentially, 
plot_model(m3, type='pred', terms=c('position','topic_pref [0.1,0.3,0.5,0.7,0.9]'))


## What if people would immediately update their selective exposure?
super_real_case <- function(prior = c(0.1,0.1,0.1,0.1,0.6), pos_bias=0.2) {
  s = sim(prior, pos_bias=pos_bias)
  cat('Entropy:')
  cat('\n\t',s$entropy$before)
  cat('\n\t',s$entropy$after)
  for (i in 2:10) {
    s = sim(s$topics$post, pos_bias=pos_bias)
    cat('\n\t',s$entropy$after)
  }
  cat('\n\nPosterior: ',s$topics$post)
}

## if position bias is high, diversity increases (yay!!)
super_real_case(pos_bias=0.5)

## if position bias is low, diversity decreases (boo!!)
super_real_case(pos_bias=0.2)




