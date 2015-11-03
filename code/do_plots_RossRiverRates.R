
# ~/projects/Vally2012-RossRiverRates/
# EDA is in EDA_RossRiverRates.r
# this makes final plots

#################################################################################
d_eastern


fit <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_eastern )
summa <- summary(fit)
summa
## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)
## (Intercept)  -4.8244     0.1445 -33.384  < 2e-16 ***
## buffer       -0.2465     0.0792  -3.112  0.00186 **
## ---

exp(-0.2465)-1
#= -0.2184686
exp(-0.2465-1.96*0.0792)-1
#=  -0.3308399
exp(-0.2465+1.96*0.0792)-1
# =  -0.08722695


write.table('###### EASTERN','reports/output.txt',row.names=F,col.names=F,quote=F)
sink('reports/output.txt',append=T)
print(summary(fit))
sink()

par(mar=c(4,4,1.75,1))

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

legend('topright',c('Data','Model fit','95% CI'),lty=c(1,1,2),pch=c(1,NA,NA),lwd=c(1,2,1))


savePlot('reports/Eastern.jpg',type=c('jpeg'))
dev.off()


#################################################################################
d_urban

fit <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_urban )
summa <- summary(fit)
summa$coeff
##               Estimate Std. Error     z value     Pr(>|z|)
## (Intercept) -5.5236291 0.33353693 -16.5607719 1.338655e-61
## buffer       0.0385268 0.06351951   0.6065349 5.441596e-01

# decided to exclude zero pop
d_urban2 <- d_urban[-1,]
d_urban2

fit <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_urban2 )

write.table('###### URBAN EXCLUDING ZERO POPULATION BUFFER 0.5','reports/output.txt',row.names=F,col.names=F,quote=F,append=T)
sink('reports/output.txt',append=T)
print(summary(fit))
sink()

par(mar=c(4,4,1.75,1))

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

legend('topright',c('Data','Model fit','95% CI'),lty=c(1,1,2),pch=c(1,NA,NA),lwd=c(1,2,1))


savePlot('reports/Urban.jpg',type=c('jpeg'))
dev.off()
