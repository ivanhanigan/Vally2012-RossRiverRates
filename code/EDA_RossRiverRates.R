# Sampling from the binomial distribution
# the cases of ross river counted in concentric buffers around a swamp

str(d_eastern)
# first model this

fit  <- glm(cases~ buffer + offset(log(pops)),family='poisson', data=d_eastern)
summary(fit)
termplot(fit,se=T,partial.resid = TRUE)
par(mfrow=c(2,2))
plot(fit)

# how compare to Hass orig logistic model?
fit_logistic <- glm(cbind(d_eastern$cases,(d_eastern$pops-d_eastern$cases)) ~ buffer,family=binomial(link = "logit"), data=d_eastern)
summary(fit_logistic)
# very close

#### URBAN ####
d_urban


# first model this
fit <- glm(cases~ buffer + offset(log(1+pops)),family='poisson', data=d_urban)
summary(fit)
# try some alternatives
require(splines)
require(mgcv)
# Exclude row with zero pop
d_urban2 <- d_urban[-1,]
fit1 <- glm(cases~ ns(buffer,df=3) + offset(log(pops)),family='poisson', data=d_urban2)
summary(fit1)
termplot(fit1, se = T)
dev.off()
fit2 <- gam(cases~ s(buffer) + offset(log(pops)),family='poisson', data=d_urban2)
summary(fit2)
plot(fit2)
dev.off()
# Looks like nothing going on in Urban

#### Try sampling from the binomial
"Description

background

The data are counts of cases of a disease in concentric buffers around a putative exposure source.

The aim is to display the measured incidence rates with 95% confidence intervals across the buffers to discern if there is a trend related to distance from exposure source.
methods

To calculate the 95% confidence intervals we will generate 1000 random numbers for each buffer zone from the binomial distribution by specifying the probability and sample size of that buffer zone.

The use of the rbinom() function follows the description in Crawley 2002 pp 476-477 (which includes a mathematical description as well).
Input

The data include cases and resident populations counted within each buffer zone using an overlay and intersect GIS operation.
Output

We want to generate a graph with distance on the x axis and incidence rate on the y axis, showing the measured rates and the estimated 95%CI.

REFERENCE:
Crawley, M.J. (2002). Statistical Computing: An Introduction to Data Analysis using S-Plus. John Wiley & Sons Ltd, Chichester.
"
# FIRST WITH FAKE DATA
# Sampling from the binomial distribution
# construct a data frame with the distances, cases and populations.
# the motivating example for this is unpublished data so for this example I'll generate random numbers from a normal distribution to simulate those data, using their (absolute) mean and standard deviation
# construct a data frame with the distances, cases and populations.
buffer=c(0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5)
cases=abs(round(rnorm(15,13,10)))
cases=cases[order(cases,decreasing=T)]
pop=abs(round(rnorm(15,2686,1122)))
pop=pop[order(pop,decreasing=T)]
d=data.frame(buffer,cases,pop)

# the rbinom function
# n = number of random numbers to be generated
# size = sample size
# prob = probability of infection

# create an empty matrix to recieve the estimated counts for each buffer
out=matrix(nrow=0,ncol=3)

# create an empty matrix to recieve the estimated CIs
ci=matrix(nrow=0,ncol=4)

# now loop through each buffer zone
for(i in 1:15){
                # step one generate 1000 random numbers for the buffer zone
                out=rbind(out,# append the new rows to the 'out' matrix
                        cbind(d[i,'buffer'], # an index for each buffer zone
                        rbinom(n=1000,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']), # the random number generator is given the required n, sample size (pop) and probability (rate)
                        d[i,'pop'])) # when we finish we want to display the counts as a rate so need the denominator

                # step 2 calculate the two tailed lower and upper 95% expected counts given the specified probability and sample size
                ci=rbind(ci,cbind(d[i,'buffer'],
                        qbinom(p=0.025,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']), # qbinom is the quantile function for the binomial distribution
                        qbinom(p=0.975,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']),
                        d[i,'pop'])) # for the CIs as a rate

}

# now to generate the plot
# first plot the 1000 generated counts (as a rate/1000)
plot(out[,1],1000*(out[,2]/out[,3]),pch=16,cex=.4,ylab='rate/1000',xlab='Buffer (km)',ylim=c(0,15))
# show the area between the 95% CIs (as a rate/1000) with a grey polygon (note the need to reverse the order of the upper confidence estimates).
polygon(c(ci[,1],ci[15:0.5,1]),c(1000*(ci[,2]/ci[,4]),1000*(ci[15:.5,3]/ci[15:.5,4])),col='grey',border = NA)
# now show the 95% CI as dotted lines
lines(ci[,1],1000*(ci[,2]/ci[,4]),lty=2)
lines(ci[,1],1000*(ci[,3]/ci[,4]),lty=2)
# show the data points on top of the grey polygon
points(out[,1],1000*(out[,2]/out[,3]),pch=16,cex=.4)
# plot the empirical rate
lines(d[,1],1000*(d[,2]/d[,3]),lwd=2)

# add a legend
legend('topright',legend=c('1000 simulated rates','Empirical rate','Simulated 95%CI'),pch=c(16,NA,NA),lty=c(0,1,2),lwd=c(0,2,1))

# save out as whatever image format the journal requires
savePlot('reports/rbinom.jpg',type=c('jpeg'))

dev.off()


##########################################################################3
# NOW WITH THE DATA
# the rbinom function
# n = number of random numbers to be generated
# size = sample size
# prob = probability of infection

# create an empty matrix to recieve the estimated counts for each buffer
out=matrix(nrow=0,ncol=4)

# create an empty matrix to recieve the estimated CIs
#ci=matrix(nrow=0,ncol=4)


#################################################################################
# Do this with a manual loop, set d to the appropriate input and then repeat
par(mfrow=c(2,1))
# start second time here, change d
# and set up a title as appropriate
for(i in 1:2){
if(i == 1){
d=d_eastern
title_label <- "eastern"
} else {
d=d_urban
title_label <- "urban"
}

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

#               # NOT RUN step 2 calculate the two tailed lower and upper 95% expected counts of ross river virus given the specified probability and sample size
#               ci=rbind(ci,cbind(d[i,'buffer'],
#                       qbinom(p=0.025,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']), # qbinom is the quantile function for the binomial distribution
#                       qbinom(p=0.975,size=d[i,'pop'],prob=d[i,'cases']/d[i,'pop']),
#                       d[i,'pop'])) # for the CIs as a rate
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
title(title_label)

output=as.data.frame(output, row.names = F)
names(output)=c('buffer','50pct','5pct','95pct')
output
# NOT RUN
#write.csv(output,'eastern.csv',row.names=F)
#write.csv(output,'urban.csv',row.names=F)
}
# manual loop ends here, repeat with other group
#############################################################
savePlot('reports/simulated.png')
dev.off()

# decision made to go with Poisson
