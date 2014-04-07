# prior distrtribution given by
# p(theta) ~ N[mu_0, sigma_0]

mu_0 <- .005
sigma_0 <- .001

# new observation, sigma_1 = .04, mu_1 = .01 (which we will call x)
x <- .01
sigma_1 <- .004

#calculate tao
tau_0 <- 1/sigma_0^2
tau <- 1/sigma_1^2

posterior_mean <- tau_0*mu_0/(tau_0 + tau) + tau*x/(tau_0 + tau)
posterior_var <- 1/(tau_0 + tau)
