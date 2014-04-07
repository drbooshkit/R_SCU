# Mean reversion
#r_t+1 - r_t = k(theta - r_t) + e_t
#theta variable drives +/-  tendency
#e_t is random error term

# what is the k value? the rate at which the parameter is being driven back to mean level? how fast are the oscillations?
# what is the level of noise? std dev of e_t (sigma)

# rearrange for regression
r_t+1 = k*a + (1-k)r_t + e_t
r_t+1 <- A + B*r_t + e_t

# how far back should you use to capture A and B? sensitivity study may be required
k <- 1-B	#this is the rate at which it reverts
a <- A/(1-B)	#this is the long-run level
A <- k*a
B <- 1-k

# Trend line
# take entire regression and add c*t component
# r_t+1 <- A + B*r_t + c*t + e_r

