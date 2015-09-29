# Project: Vally2012-RossRiverRates
# Author: Your Name
# Maintainer: Who to complain to <yourfault@somewhere.net>

# This file loads all the libraries and data files needed
# Don't do any cleanup here

### Load any needed libraries
#load(LibraryName)


### Load in any data files
#read.csv("data/FileName" as.is=T)
dat <- read_excel("data_provided/RRV Erp96_MGA_EastVUrban_210208 Mark's final w MDLs additions 30 March 2008.xls",
                        sheet='Table 1', skip =1
                        )
str(dat)
