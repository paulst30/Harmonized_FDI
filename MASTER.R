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

#### define output to be saved during estimation loops ####
quin_perfs <- list()        #place to store decile performance tables
besttune <- list()          #place to store best tunes
coverage_all <- list()      #place to store coverage of variables
graphs <- list()            #place to store graphs
train_models <- list()      #place to store the final best tuned model   
prediction_summary_tdiff <- matrix(nrow = 6, ncol=6) %>%  #place to store performance measures
                            as.data.frame() 
colnames(prediction_summary_tdiff) <- c("TrainRMSE", "TrainR2", "TrainMAE" , "TestRMSE", "TestR2", "TestMAE")
prediction_tasks <- matrix(nrow=6, ncol=5) #place to store prediction tasks and descriptives

#### load in the data ####
source("data.R")

#### loop over setup and estimation scripts by prediction tasks ####
for (i in 1:6) {
source("Setup.R")   #defines predictor and target vintage, builds features, defines training and testing sets
source("mob.R")     #fits MOB on training set and evaluates performance; uses best model to predict the prediction set

#clear memory but keep track of ...
rm(list=setdiff(ls(), c("quin_perfs",               #...decile performance
                        "besttune",                 #...best tunes
                        "graphs",                   #...graphs
                        "prediction_summary_tdiff", #...performance metrics
                        "prediction_tasks",         #...summary of prediction tasks
                        "test_tdiff",               #...test set
                        "prediction_train_tdiff",   #...prediction on the training set
                        "prediction_test_tdiff",    #...prediction on the test set
                        "prediction_tdiff",         #...prediction on the prediction set
                        "predictor_matrix",         #...table that defines prediction tasks
                        "train_models",             #...ultimate models
                        "data")))                   #...data set
}

source("results.R")               #clean up the results and generate output for the paper
source("practical_test.R")        #generate the harmonized dataset and run practical tests
