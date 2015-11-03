# Project: Vally2012-RossRiverRates
# Author: Hanigan, Ivan
# Maintainer: <ivan.hanigan@gmail.com>

# This is the main file for the project
# It should do very little except call the other files

### Set the working directory
projdir <- "~/projects/Vally2012-RossRiverRates"
setwd(projdir)

### Set any global variables here
####################
ylims <- c(0,10)

####################


### Run the code
source("code/func.R")
# data provided needed cleaning once only
#source("code/load1.R")
#source("code/clean1.R")
# load data derived 
source("code/load.R")
# no more cleaning done

# Do some EDA, especially test the sampling from the binomial
# distribution idea
# NOT RUN, do this manually
# source("code/EDA_RossRiverRates.R")

# Do the RRv rates figures for paper
source("code/do_plots_RossRiverRates.R")

# Do the model checking all at once, or alternately do this for the report
#source("code/do_model_checking.R")
