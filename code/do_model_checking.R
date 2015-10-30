# aims
## test the different parametrisations for the effect modification by urban
## we show that the coeffs and se are equivalent but that the psuedo-R
## squared will be better when including all our data in stratified analysis

# model 0 effect in eastern
d_eastern
fit <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_eastern )
summa <- summary(fit)
summa
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -4.82425    0.14451 -33.382  < 2e-16 ***
## buffer      -0.24702    0.07921  -3.119  0.00182 **

# model 1 effect in urban
fit1 <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_urban )
summa <- summary(fit1)
summa
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -5.52363    0.33354 -16.561   <2e-16 ***
## buffer       0.03853    0.06352   0.607    0.544

# step 1, combine the urban and rural data
d_eastern$urban <- 0
d_urban$urban <- 1
dat2 <- rbind(d_eastern, d_urban)
str(dat2)
dat2

# model 2 is a multiplicative term
fit2 <- glm(cases ~ buffer * urban + offset(log(1+pops)), family = 'poisson', data = dat2)
summa <- summary(fit2)
summa
## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -4.82425    0.14451 -33.382  < 2e-16 ***
## buffer       -0.24702    0.07921  -3.119  0.00182 **
## urban        -0.69938    0.36350  -1.924  0.05435 .
## buffer:urban  0.28555    0.10153   2.812  0.00492 **

# the coeff on buffer is for urban = 0 is main effect
# the coeff on buffer:urban is for urban = 1 is the marginal effect
b1 <- summa$coeff[2,1]
b3 <- summa$coeff[4,1]
b1 + b3
# 0.0385268
# but what about that p-value?  and the se?
str(fit2)
fit2_vcov <- vcov(fit2)
fit2_vcov
# now calculate the conditional standard error for the marginal effect of buffer for the value of the modifying variable (Z, urban =1)
varb1<-fit2_vcov[2,2]
varb3<-fit2_vcov[4,4]
covarb1b3<-fit2_vcov[2,4]
Z<-1
conditional_se <- sqrt(varb1+varb3*(Z^2)+2*Z*covarb1b3)
conditional_se



# model 3 is the re-parametrisation
dat2$buffer_urban <- dat2$buffer * dat2$urban
dat2$buffer_eastern <- dat2$buffer * (1-dat2$urban)

fit3 <- glm(cases ~ buffer_urban + buffer_eastern + urban + offset(log(1+pops)), family = 'poisson', data = dat2)
summa <- summary(fit3)
summa

## Coefficients:
##                Estimate Std. Error z value Pr(>|z|)
## (Intercept)    -4.82425    0.14451 -33.382  < 2e-16 ***
## buffer_urban    0.03853    0.06352   0.607  0.54416
## buffer_eastern -0.24702    0.07921  -3.119  0.00182 **
## urban          -0.69938    0.36350  -1.924  0.05435 .
