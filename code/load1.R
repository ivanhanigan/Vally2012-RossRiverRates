# Project: Vally2012-RossRiverRates
# Author: Ivan Hanigan
# Maintainer: ivan.hanigan@gmail.com

# This file loads all the libraries and data files needed
# Don't do any cleanup here

### Load in any data files
# NOT RUN, the excel sheet was read once only and used to create derived data for future work
dat <- read_excel("data_provided/RRV Erp96_MGA_EastVUrban_210208 Mark's final w MDLs additions 30 March 2008.xls", sheet='Table 1', skip =1)
str(dat)
