#### load required packages into the library #### 
library(tidyverse)
library(ggplot2)
library(haven)
library(foreign)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(expss)
library(mlbench)
library(xtable)
library(Matrix)
library(cowplot)
library(corrplot)
library(FactoMineR)
library(car)
library(glmnet)
library(RANN)
library(stringr)
library(MatchIt)
library(doBy)
library(fixest)
library(partykit)


#### load in the data ####
source("data.R")

#### loop over estimation scripts and save out files ####
source("training_loop.R")

#### clean up results ####
source("results.R")               #clean up the results and generate output for the paper

#### apply data in fixed-effects estimations ####
source("practical_test.R")        #generate the harmonized dataset and run practical tests
