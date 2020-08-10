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

Install:

    > install("nwblastarca", build_vignettes = FALSE)

You can look at the documentation here:

    > help(package = "nwblastarca")

# Example #
---
    
Run through the wrapper function:

    > library(nwblastarca)

    > firstyear <- 2017
    > forecastyrs <- 2
    > dynamic.stocks <- c('WA.OR.lingcod')
    > MCMC <- 100

    > tsout <- blast_arca_wrapper(MCMC = MCMC, firstyear = firstyear, 
        forecastyrs = forecastyrs, dynamic.stocks = dynamic.stocks)

If you want to run it in parallel you need to register your workers first, for
example:

    > library(doParallel)
    > registerDoParallel(cores = 8)
        
    > tsout <- blast_arca_wrapper(MCMC = MCMC, firstyear = firstyear, 
        forecastyrs = forecastyrs, dynamic.stocks = dynamic.stocks, 
        parallel = TRUE)

Currently outputs numbers at age and recreational catches for the dynamic 
species, and takes about 16 hours for 100 iterations with 8 workers (on an Intel
i7-8700).

We can use the included summary function to plot the results:

    > plotout <- Plot_res(tsout, MCMC)

    > plotout[[1]]
    > dev.new()
    > plotout[[2]]

![Numbers at age](https://nwcgit.nwfsc.noaa.gov/achen/nwblastarca/-/raw/master/inst/rdme/NAA.png)
![Recreational catches](https://nwcgit.nwfsc.noaa.gov/achen/nwblastarca/-/raw/master/inst/rdme/reccatches.png)
