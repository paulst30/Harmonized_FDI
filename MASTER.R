#Setup 
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("haven")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("expss")
#install.packages("randomForest")
#install.packages("gbm")
#install.packages("randomForestSRC")
#install.packages("missForest")
#install.packages("foreign")
#install.packages("mlbench")
#install.packages("xtable")
#install.packages("xgboost")
#install.packages("Matrix")
#install.packages("cowplot")
#install.packages("FactoMineR")
#install.packages("car")
#install.packages("glmnet")
#install.packages("RANN")
#install.packages("diversityForest")
#install.packages("stringr")
#install.packages("MatchIt")
#install.packages("doBy")
#install.packages("neuralnet")
#install.packages("fixest")



library(tidyverse)
library(ggplot2)
library(haven)
library(foreign)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(expss)
library(randomForest)
library(gbm)
library(randomForestSRC)
library(missForest)
library(leaps)
library(mlbench)
library(xtable)
library(xgboost)
library(Matrix)
library(cowplot)
library(corrplot)
library(FactoMineR)
library(car)
library(glmnet)
library(RANN)
library(diversityForest)
library(stringr)
library(MatchIt)
library(doBy)
library(neuralnet)
library(fixest)
library(party)


quin_perfs <- list()
besttune <- list()
coverage_all <- list()
graphs <- list()
prediction_summary_tdiff <- matrix(nrow = 6, ncol=6) %>% as.data.frame()
colnames(prediction_summary_tdiff) <- c("TrainRMSE", "TrainR2", "TrainMAE" , "TestRMSE", "TestR2", "TestMAE")
prediction_tasks <- matrix(nrow=6, ncol=5)

source("data.R")
for (i in 1:6) {
source("Setup.R")
source("mob.R")
rm(list=setdiff(ls(), c("quin_perfs", 
                        "besttune", 
                        "graphs", 
                        "prediction_summary_tdiff", 
                        "coverage_all", 
                        "prediction_tdiff", 
                        "prediction_tasks", 
                        "test_tdiff",
                        "prediction_train_tdiff",
                        "predictor_matrix",
                        "data"))) 
}

source("results.R")
source("practical_test.R")
source("fellow_diff_train.R")