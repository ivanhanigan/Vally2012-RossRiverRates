# ~/projects/Vally2012-RossRiverRates/
# EDA is in EDA_RossRiverRates.r
# this makes final plots

require(readxl)

dataframe=read_excel("data_provided/RRV Erp96_MGA_EastVUrban_210208 Mark's final w MDLs additions 30 March 2008.xls",sheet='Table 1', skip =1)
str(dataframe)
dataframe$`Buffer (km)` <- as.numeric(dataframe$`Buffer (km)`)
d_eastern=dataframe[1:15,c(1,5:7)]
d_urban=dataframe[1:15,c(1,8:10)]


ylims=c(0,10)

#################################################################################
d=d_eastern
names(d)=c('buffer','cases','pops','rate')
d

fit=glm(cases~ buffer + offset(log(pops)),family='poisson', data=d )
summa=summary(fit)
exp(-0.2465)-1 
#= -0.2184686 
exp(-0.2465-1.96*0.0792)-1 
#=  -0.3308399
exp(-0.2465+1.96*0.0792)-1
# =  -0.08722695


write.table('###### EASTERN','reports/output.txt',row.names=F,col.names=F,quote=F)
sink('reports/output.txt',append=T)
d
summary(fit)
sink()

par(mar=c(4,4,1.75,1)) 

plot(d$buffer,(d$cases/d$pops)*1000,type='b',ylim=ylims,ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d$buffer,(predict(fit,type='response')/d$pops)*1000,lwd=2) 

#lines(d$buffer,((
#  predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit)
#  )/d$pops)*1000,lty=2)
#
#  
#lines(d$buffer,((
#  predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit)
#  )/d$pops)*1000,lty=2)
#  
# ASKED MARK AND IT TURNS OUT THIS IS WRONG, CIs SHOULD BE

pred1 = predict(fit,type='link',se.fit=T)
  
#CIs = exp(pred1$fit-1.96*pred1$se.fit)
# NOT
# predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit)

# AND MARK ALSO GIVES THIS FANCY WAY OF PLOTTING THEM
with(d, 
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


savePlot('Eastern.jpg',type=c('jpg'))
dev.off()


#################################################################################
d=d_urban
names(d)=c('buffer','cases','pops','rate')
#exclude zero pop
d=d[-1,]
d

fit=glm(cases~ buffer + offset(log(pops)),family='poisson', data=d )

write.table('###### URBAN EXCLUDING ZERO POPULATION BUFFER 0.5','reports/output.txt',row.names=F,col.names=F,quote=F,append=T)
sink('reports/output.txt',append=T)
d
summary(fit)
sink()

par(mar=c(4,4,1.75,1)) 

plot(c(0.5,d$buffer),c(NA,(d$cases/d$pops)*1000),type='b',ylim=ylims, xlim = c(0,7.5),ylab='Incidence Rate per 1000',xlab='Buffer (Kilometres)')

lines(d$buffer,(predict(fit,type='response')/d$pops)*1000,lwd=2) 

pred1 = predict(fit,type='link',se.fit=T)

with(d, 
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


savePlot('Urban.jpg',type=c('jpg'))
dev.off()
