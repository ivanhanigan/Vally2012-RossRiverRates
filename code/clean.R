# Project: Vally2012-RossRiverRates
# Author: Your Name
# Maintainer: Who to complain to <yourfault@somewhere.net>

# All the potentially messy data cleanup
dat$`Buffer (km)` <- as.numeric(dat$`Buffer (km)`)
d_eastern <- dat[1:15,c(1,5:7)]
names(d_eastern) <- c('buffer','cases','pops','rate')

d_urban <- dat[1:15,c(1,8:10)]
names(d_urban) <- c('buffer','cases','pops','rate')
