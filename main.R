# Project: Vally2012-RossRiverRates
# Author: Hanigan, Ivan Charles
# Maintainer: <ivan.hanigan@gmail.com>

# This is the main file for the project
# It should do very little except call the other files

### Set the working directory
setwd("~/projects/Vally2012-RossRiverRates")


### Set any global variables here
####################
ylims <- c(0,10)

####################


### Run the code
source("code/func.R")
source("code/load.R")
source("code/clean.R")

# Do some EDA, especially test the sampling from the binomial
# distribution idea
# NOT RUN source("code/EDA_RossRiverRates.R")

# Do the RRv rates figures for paper
source("code/do_plots_RossRiverRates.R")
