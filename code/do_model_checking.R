
# aims
## test the different way to handle the missing population row, also
## different parametrisations for the effect modification by urban
## we show that the coeffs and se are equivalent

# model 0 effect in eastern
#d_eastern
fit <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_eastern )
summa <- summary(fit)
summa
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -4.82425    0.14451 -33.382  < 2e-16 ***
## buffer      -0.24702    0.07921  -3.119  0.00182 **

# model 1 effect in urban with dropped zero pop zone
d_urban2 <- d_urban[-1,]
fit1 <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_urban2 )
summary(fit1)
# now  without dropping the empty pop
fit1.1 <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_urban )
summa <- summary(fit1.1)
summa
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept) -5.52363    0.33354 -16.561   <2e-16 ***
## buffer       0.03853    0.06352   0.607    0.544


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
#str(fit2)
fit2_vcov <- vcov(fit2)
#fit2_vcov
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



#########################
# model 4 is multilevel
library(lme4)
str(dat2)
dat2$urban <- factor(dat2$urban)
# null model, grouping by urban but not fixed effects.
Norm1 <-glmer(cases ~ 1 + (1|urban),
              data=dat2,
              family = 'poisson',
              offset = log(1+pops)
              )
summary(Norm1)

# Adding fixed-effects predictors
# Predict cases from distance buffer

Norm2 <-glmer(cases ~ buffer + (1|urban),
             data=dat2,
             family = 'poisson',
             offset = log(1+pops)
             )

summary(Norm2)

# Random slopes

# Add a random effect of buffer as well. Now in addition to estimating the distribution of intercepts across urban/rual, we also estimate the distribution of the slope of distance.

Norm3 <- glmer(cases ~ buffer + (buffer|urban),
              data=dat2,
              family = 'poisson',
              offset = log(1+pops)
              )
# Warning message:
#In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#               Model failed to converge with max|grad| = 0.00123993 (tol = 0.001, component 1)
summary(Norm3)
coefficients(Norm3)

pred1  <-  predict(Norm3,type='link')
#se.fit <- sqrt(diag(vcov(mylm)))[2]
fixef(Norm3)
pred1 <- cbind(dat2, pred1)

par(mar=c(4,4,1.75,1), mfrow = c(2,2))

plot(c(0.5,d_urban2$buffer),c(NA,(d_urban2$cases/d_urban2$pops)*1000),type='b',ylim=ylims, xlim = c(0,7.5),ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d_urban2$buffer,(predict(Norm3,type='response')[17:30]/d_urban2$pops)*1000,lwd=2)
#
# with(d_urban2,
#      with(pred1[17:30, "pred1"],
#           matlines(buffer,
#                    cbind(
#                      exp(fit-1.96*se.fit)/pops,
#                      exp(fit+1.96*se.fit)/pops
#                    )*1000,
#                    lty=2,
#                    col=1))
# )
# dev.off()


fit <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_urban2 )

plot(c(0.5,d_urban2$buffer),c(NA,(d_urban2$cases/d_urban2$pops)*1000),type='b',ylim=ylims, xlim = c(0,7.5),ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d_urban2$buffer,(predict(fit,type='response')/d_urban2$pops)*1000,lwd=2)

pred1  <-  predict(fit,type='link',se.fit=T)

with(d_urban2,
     with(pred1,
          matlines(buffer,
                   cbind(
                     exp(fit-1.96*se.fit)/pops,
                     exp(fit+1.96*se.fit)/pops
                   )*1000,
                   lty=2,
                   col=1))
)



####################################################3

plot(d_eastern$buffer,(d_eastern$cases/d_eastern$pops)*1000,type='b',ylim=ylims,ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d_eastern$buffer,(predict(Norm3,type='response')[1:15]/d_eastern$pops)*1000,lwd=2)


fit <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_eastern )
plot(d_eastern$buffer,(d_eastern$cases/d_eastern$pops)*1000,type='b',ylim=ylims,ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d_eastern$buffer,(predict(fit,type='response')/d_eastern$pops)*1000,lwd=2)

pred1  <-  predict(fit,type='link',se.fit=T)

#CIs = exp(pred1$fit-1.96*pred1$se.fit)

with(d_eastern,
     with(pred1,
          matlines(buffer,
                   cbind(
                     exp(fit-1.96*se.fit)/pops,
                     exp(fit+1.96*se.fit)/pops
                   )*1000,
                   lty=2,
                   col=1))
)



#dev.off()