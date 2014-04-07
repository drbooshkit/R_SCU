library(sampleSelection)
data(Mroz87)

#test for significance
res <- lm(wage ~ age + I(age^2) + educ + city, data=Mroz87)
summary(res)

# but maybe LFP already accounts for these above variables?
res <- lm(wage ~ age + I(age^2) + lfp + city, data=Mroz87)
summary(res)

# RUN with both
res <- lm(wage ~ age + I(age^2) + lfp + educ + city, data=Mroz87)
summary(res)

# how do i know if LFP and EDUC are endogenous to each other?
# run Heckman equation

# what is question we are trying to prove?
# if education causes LFP, and LFP causes wage, then does EDUC still drive Wage?
# Which "channels" are operating between endogenous variable
# use truncated variables in a 2-step form
res <- selection(lfp ~ age + I(age^2) + faminc + kids5 + educ, 
	wage ~ exper + I(exper^2) + educ + city, data=Mroz87, method="2step")

summary(res)

# even after accouning for EDUC > LFP > WAGE, we see that EDUC > WAGE is still significant
# so education affects in two ways, to both wage and to both LFP
# 

Tobit 2 model (sample selection model)
2-step Heckman / heckit estimation
753 observations (325 censored and 428 observed)
14 free parameters (df = 740)
Probit selection equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.399e-01  1.514e+00  -0.092    0.926    
age         -1.174e-02  6.876e-02  -0.171    0.864    
I(age^2)    -2.567e-04  7.808e-04  -0.329    0.742    
faminc       3.233e-06  4.297e-06   0.752    0.452    
kids5       -8.531e-01  1.144e-01  -7.457 2.48e-13 ***
educ         1.166e-01  2.365e-02   4.931 1.01e-06 ***
Outcome equation:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2.7413454  1.3679742  -2.004   0.0454 *  
exper        0.0334859  0.0614715   0.545   0.5861    
I(exper^2)  -0.0003096  0.0018477  -0.168   0.8670    
educ         0.4887549  0.0795133   6.147 1.29e-09 ***
city         0.4467137  0.3162288   1.413   0.1582    
Multiple R-Squared:0.1248,	Adjusted R-Squared:0.1145
Error terms:
              Estimate Std. Error t value Pr(>|t|)
invMillsRatio  0.13220    0.73970   0.179    0.858
sigma          3.09469         NA      NA       NA
rho            0.04272         NA      NA       NA