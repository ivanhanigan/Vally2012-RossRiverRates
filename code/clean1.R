# Project: Vally2012-RossRiverRates
# Author: ivanhanigan
# Maintainer: <ivan.hanigan@gmail.com>

# All the potentially messy data cleanup
dat[,1:4]
# We limit the analysis to up to 7.5 km away
dat <- dat[1:15,]
str(dat)
dat$`Buffer (km)` <- as.numeric(dat$`Buffer (km)`)
names(dat)
 ## [1] "Buffer (km)"                      "RRV cases"
 ## [3] "Total persons"                    "Entire Leschenault RRV rate/1000"
 ## [5] "RRV cases"                        "Total persons"
 ## [7] "Eastern Estuary RRV rate/1000"    "RRV cases"
 ## [9] "Total persons"                    "Urban Bunbury RRV rate/1000"

d_eastern <- dat[,c(1,5:7)]
names(d_eastern) <- c('buffer','cases','pops','rate')

d_urban <- dat[1:15,c(1,8:10)]
names(d_urban) <- c('buffer','cases','pops','rate')

# combine the urban and rural data
d_eastern$urban <- 0
d_urban$urban <- 1
dat2 <- rbind(d_eastern, d_urban)
str(dat2)
dat2

save.image()
