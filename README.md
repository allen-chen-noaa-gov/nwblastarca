NWFSC BLAST/ARCA model
=========
---

If you run into problems you can contact allen.chen@noaa.gov

# Installation #
---

To install:

    > install.packages("devtools")
	> library(devtools)
	
Set the directory you've downloaded the package into:

    > setwd("J:/Fishperson/Directory_containing_nwblastarca")

Run through the wrapper function (still in progress):

    > library(nwblastarca)

    > rootdir <- "U:\\NWFSC_data_code\\BLAST\\" 

    > firstyear <- 2017
    > forecastyrs <- 5
    > dynamic.stocks <- c('WA.OR.lingcod')

    > blast_arca_wrapper(firstyear = firstyear, forecastyrs = forecastyrs,
        rootdir = rootdir, dynamic.stocks)
    