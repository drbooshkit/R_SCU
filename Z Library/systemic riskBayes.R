## Correlated default. eg CDO market, example of not using proper probabilities

# what is the probability of firm or market x failing?
p1 <- 0.01
p2 <- 0.03

# binary outcomes. foreclose/fail or not
# indicative variable, d_1 or d_2. 0 not failure, 1 has failure (event happened)
# what's the mean value of d_i?
# Expected value (mean value) of d_i is p_i

# Bernoulli distribution
# 
sig_d_1 <- sqrt(p1*(1-p1)) 
sig_d_2 <- sqrt(p2*(1-p2))

# what's the correlation between these two firms? what's the chance both of them will fail? eg insurance contract for event of both failing
# mean of probability that d_1 and d_2 are both 1 = p12 = p1 * p2.  this is joint probability

# p(d_1|d_2) probability that firm 1 fails if firm 2 has failed
# p(d_2|d_1) probability that firm 2 fails if firm 1 has failed
# this would drive the value of an insurance contract

