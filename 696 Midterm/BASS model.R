# calibrate BASS model
# sales in period s(t)
# cumulative sales up to t, S(t)
# s(t) = beta_0 + beta_1*S(t) + beta_2*S(t)^2
#
# beta_0 = p*m
# beta_1 = q - p
# beta_2 = -q/m

# regression:





m <- 100000
p <- .01
q <- .2
t <- seq(1,20,1)
f_t <- (exp((p+q)*t)*p*(p+q)^2)/(p*exp((p+q)*t)+q)^2

t_peak <- -log(p/q)/(p+q)