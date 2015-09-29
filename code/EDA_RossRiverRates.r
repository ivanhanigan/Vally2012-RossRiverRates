# Sampling from the binomial distribution
# the cases of ross river counted in concentric buffers around a swamp
# ihanigan 23/12/2009

#require(sqldf)
#require(reshape)
require(xlsReadWrite)

dataframe=read.xls("I:/projects/0.99999 software training and support/Vally20091223_RossRiverRates/RRV Erp96_MGA_EastVUrban_210208 Mark's final w MDLs additions 30 March 2008.xls",sheet='Table 1', from =2)
str(dataframe)
d_eastern=dataframe[1:15,c(1,5:7)]
d_urban=dataframe[1:15,c(1,8:10)]

d=d_eastern
names(d)=c('buffer','cases','pops','rate')
d
# first model this

fit=glm(cases~ buffer + offset(log(pops)),family='poisson', data=d )

write.table('###### EASTERN','output.txt',row.names=F,col.names=F,quote=F)

sink('output.txt',append=T)
d
summary(fit)
sink()
termplot(fit,se=T,partial.resid = TRUE)
par(mfrow=c(2,2))
plot(fit)

# how compare to Hass orig logistic model?
#fit_logistic=glm(cbind(d$cases,(d$pops-d$cases)) ~ buffer,family=binomial(link = "logit"), data=d )
#summary(fit_logistic)
# same

# now plot curve

par(mfrow=c(2,1))
plot(predict(fit,type='response'),type='b',ylim=c(0,50))
lines(predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
lines(predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
points(d$cases,pch=16,col='blue')
title('eastern')
legend('topright',c('orig','predicted'),pch=c(1,16),col=c('black','blue'))


d=d_urban
names(d)=c('buffer','cases','pops','rate')

# first model this
#require(splines)
#require(mgcv)


fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d)
# or
#fit=glm(cases~ ns(buffer,df=3) + offset(log(pops)),family='poisson', data=d)
#fit=gam(cases~ s(buffer) + offset(log(1+pops)),family='poisson', data=d)

write.table('###### URBAN','output.txt',row.names=F,col.names=F,quote=F,append=T)

sink('output.txt',append=T)
d
summary(fit)
sink()

plot(predict(fit,type='response'),type='b',ylim=c(0,50))
lines(predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
lines(predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
points(d$cases,pch=16,col='blue')
legend('topright',c('orig','predicted'),pch=c(1,16),col=c('black','blue'))

title('urban')

savePlot('model1.png')
dev.off()
# the rbinom function 
# n = number of random numbers to be generated
# size = sample size
# prob = probability of infection

# create an empty matrix to recieve the estimated counts for each buffer
out=matrix(nrow=0,ncol=4)

# create an empty matrix to recieve the estimated CIs
#ci=matrix(nrow=0,ncol=4)






# with a manual loop (DURR)

#################################################################################
par(mfrow=c(2,1))
d=d_eastern
#d=d_urban
names(d)=c('buffer','cases','pops','rate')

# create an empty matrix to recieve the estimated counts for each buffer
out=matrix(nrow=0,ncol=4)

# now loop through each buffer zone
for(i in 1:15){
		# step one generate 1000 random numbers for the buffer zone
		out=rbind(out,# append the new rows to the 'out' matrix
			cbind(1:1000,d[i,'buffer'], # an index for each buffer zone
			rbinom(n=1000,size=d[i,'pops'],prob=d[i,'cases']/d[i,'pops']), # the random number generator is given the required n, sample size (pop) and probability (rate)
			d[i,'pops'])) # when we finish we want to display the counts as a rate so need the denominator 
		
#		# step 2 calculate the two tailed lower and upper 95% expected counts of ross river virus given the specified probability and sample size	
#		ci=rbind(ci,cbind(d[i,'buffer'],
#			qbinom(p=0.025,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']), # qbinom is the quantile function for the binomial distribution
#			qbinom(p=0.975,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']),
#			d[i,'pop'])) # for the CIs as a rate
#		
}


# reshape?
head(out)
out=as.data.frame(out)
names(out)=c('index','buffer','cases','pops')
# get rid of NAs
out$cases=ifelse(is.na(out$cases),0,out$cases)

out_table=matrix(nrow=0,ncol=5)

#i=1
for(i in 1:1000){
#out[out$index==i,]
fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=out[out$index==i,])

#summary(fit)

out_table=rbind(out_table,
  cbind(out[out$index==i,],predict(fit,type='response'))
  )
#out_table  
}

out_table[1:16,]

output=matrix(nrow=0,ncol=4)

for(i in seq(0.5 ,7.5,0.5)){
output=rbind(output,
  cbind(i,
  quantile(out_table[out_table$buffer==i,5],0.5),
  quantile(out_table[out_table$buffer==i,5],0.05),
  quantile(out_table[out_table$buffer==i,5],0.95)
  )
)
}

output

plot(output[,2],type='b',ylim=c(0,50))
lines(output[,3],col='red')
lines(output[,4],col='red')
title('eastern')
#title('urban')

output=as.data.frame(output)
names(output)=c('buffer','50pct','5pct','95pct')
write.csv(output,'eastern.csv',row.names=F)
#write.csv(output,'urban.csv',row.names=F)


# manual loop

savePlot('simulated.png')
dev.off()

# same plot
 d=d_eastern
 names(d)=c('buffer','cases','pops','rate')
 fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d )
 par(mfrow=c(2,1))
 
 plot(seq(.5,7.5,.5),predict(fit,type='response'),type='b',ylim=c(0,50))
 lines(seq(.5,7.5,.5),predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
 lines(seq(.5,7.5,.5),predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
 points(seq(.5,7.5,.5),d$cases,pch=16,col='blue')
 title('eastern')

eastern_out= read.csv(dir(pattern='eastern'))
lines(eastern_out$buffer,eastern_out[,3],col='blue')
lines(eastern_out$buffer,eastern_out[,4],col='blue')
legend('topright',c('empirical poisson','simulated x 1000','data'),lty=c(1,1,NA),pch=c(NA,NA,16),col=c('red','blue','blue'))


 d=d_urban
 names(d)=c('buffer','cases','pops','rate')
 fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d )

 plot(seq(.5,7.5,.5),predict(fit,type='response'),type='b',ylim=c(0,50))
 lines(seq(.5,7.5,.5),predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
 lines(seq(.5,7.5,.5),predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit),col='red')
 points(seq(.5,7.5,.5),d$cases,pch=16,col='blue')
 title('urban')

urban_out= read.csv(dir(pattern='urban'))
lines(urban_out$buffer,urban_out[,3],col='blue')
lines(urban_out$buffer,urban_out[,4],col='blue')
legend('topright',c('empirical poisson','simulated x 1000','data'),lty=c(1,1,NA),pch=c(NA,NA,16),col=c('red','blue','blue'))


#########################################
# as rate 

par(mfrow=c(2,1),mar=c(4,4,1.75,1))
ylims=c(0,12)
d=d_eastern
names(d)=c('buffer','cases','pops','rate')
d
eastern_out= read.csv(dir(pattern='eastern'))
 plot(d$buffer,d$rate,type='b',ylim=ylims,ylab='rate per 1000',xlab='buffer')
lines(eastern_out$buffer,(eastern_out[,3]/d$pops)*1000,col='blue')
lines(eastern_out$buffer,(eastern_out[,4]/d$pops)*1000,col='blue')
lines(eastern_out$buffer,(eastern_out[,2]/d$pops)*1000,col='blue',lwd=2)

# and poisson?
 fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d )
 lines(seq(.5,7.5,.5),(predict(fit,type='response')/d$pops)*1000,col='red',lwd=2,lty=2) 
 lines(seq(.5,7.5,.5),((predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit))/d$pops)*1000,col='red',lty=2)
 lines(seq(.5,7.5,.5),((predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit))/d$pops)*1000,col='red',lty=2)
legend('topright',c('empirical poisson','simulated x 1000','data (rate/1000)'),lty=c(2,1,1),pch=c(NA,NA,1),col=c('red','blue','black'))
title('Eastern')

d=d_urban
names(d)=c('buffer','cases','pops','rate')
d
urban_out= read.csv(dir(pattern='urban'))
 plot(d$buffer,d$rate,type='b',ylim=ylims,ylab='rate per 1000',xlab='buffer')
lines(urban_out$buffer,(urban_out[,3]/d$pops)*1000,col='blue')
lines(urban_out$buffer,(urban_out[,4]/d$pops)*1000,col='blue')
lines(urban_out$buffer,(urban_out[,2]/d$pops)*1000,col='blue',lwd=2)

# and poisson?
 fit=glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d )
 lines(seq(.5,7.5,.5),(predict(fit,type='response')/d$pops)*1000,col='red',lwd=2,lty=2) 
 lines(seq(.5,7.5,.5),((predict(fit,type='response')+(1.96*predict(fit,type='response',se.fit=T)$se.fit))/d$pops)*1000,col='red',lty=2)
 lines(seq(.5,7.5,.5),((predict(fit,type='response')-(1.96*predict(fit,type='response',se.fit=T)$se.fit))/d$pops)*1000,col='red',lty=2)
legend('topright',c('empirical poisson','simulated x 1000','data (rate/1000)'),lty=c(2,1,1),pch=c(NA,NA,1),col=c('red','blue','black'))
title('Urban')

savePlot('Figure.jpg',type=c('jpg'))
dev.off()


########################################
#old work
#
## now to generate the plot
## first plot the 1000 generated counts (as a rate/1000)
#
#plot(out[,1],1000*(out[,2]/out[,3]),type='n',pch=16,cex=.4,ylab='RRV rate/1000',xlab='Buffer (km)',ylim=c(0,20))
#
## show the area between the 95% CIs (as a rate/1000) with a grey polygon (note the need to reverse the order of the upper confidence estimates).
#polygon(c(ci[,1],ci[15:0.5,1]),c(1000*(ci[,2]/ci[,4]),1000*(ci[15:.5,3]/ci[15:.5,4])),col='grey',border = NA)
#
##points(out[,1],1000*(out[,2]/out[,3]),pch=16,cex=.4)
#
## now show the 95% CI as dotted lines
#lines(ci[,1],1000*(ci[,2]/ci[,4]),lty=2)
#lines(ci[,1],1000*(ci[,3]/ci[,4]),lty=2)
## show the data points on top of the grey polygon
##points(out[,1],1000*(out[,2]/out[,3]),pch=16,cex=.4)
## plot the empirical rate
#lines(d[,1],1000*(d[,2]/d[,3]),lwd=2)
#
## add a legend
##legend('topright',legend=c('1000 simulated rates','Empirical rate','Simulated 95%CI'),pch=c(16,NA,NA),lty=c(0,1,2),lwd=c(0,2,1))
#legend('topright',legend=c('Empirical rate','Simulated 95%CI'),lty=c(1,2),lwd=c(2,1))
#
## save out as whatever image format the journal requires
##savePlot('Vally2009_RRV_Eastern.jpg',type=c('jpg'))
##savePlot('Vally2009_RRV_Urban.jpg',type=c('jpg'))
#savePlot('Vally2009_RRV_combined.jpg',type=c('jpg'))
#
#dev.off()
#
#
#out=data.frame(out)
#names(out)=c('buffer','count','pop')
#par(mfrow=c(3,5))
#for(i in 1:length(names(table(out$buffer)))){
#
#n=names(table(out$buffer))[i]
#print(n)
#b1=sqldf(paste('select * from out where buffer = ',n,sep=''))$count
#hist(b1)
#segments(c(ci[i,2],ci[i,3]),0,c(ci[i,2],ci[i,3]),50,col='red')
#title(n)
#}
#