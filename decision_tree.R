
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

#read in Stata data
data <- read_dta('quality_analysis_ml_data.dta')


#prediction data
p_data <- data %>% filter(!is.na(OUT_BMD4) & is.na(IN_BMD4)) %>%
                    select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
                          -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair, -re_des_pair,
                          -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
                          -OECD_OUT_BMD4, IMF_OUT_net_fellow ) %>%
                  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(p_data) <- c("Prediction")
p_data <- t(p_data)
#training data
t_data <- data %>% filter(!is.na(OUT_BMD4) & !is.na(IN_BMD4)) %>%
                    select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
                            -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair,-re_des_pair,
                            -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
                            -OECD_OUT_BMD4, IMF_OUT_net_fellow )  %>%
                summarise(across(everything(),~ mean(!is.na(.x))))
rownames(t_data) <- c("Training")
t_data <- t(t_data)

#combining both
coverage <- cbind(t_data,p_data)
coverage_30 <- coverage[coverage[,2]>.3,]

#missingness <- data %>% filter(!is.na(OUT_BMD4) & is.na(IN_BMD4)) %>% select(rownames(coverage_30), -starts_with("s_"), -starts_with("r_"), -Type)
#sum(is.na(missingness))/(nrow(missingness)*ncol(missingness))
# missingness is at roughly 30 percent


#####################select variables to work with##############################
set.seed(100)
working_data_wm <- data %>% select(s_iso3c, r_iso3c, year, diff_inBMD4_outBMD4, 
                                IN_BMD4,OUT_BMD4, rownames(coverage_30), re_OUT_BMD4,
                               -Type, -re_IMF_IN, -re_OECD_IN_BMD4) %>%
                                mutate_if(is.character, as.factor) %>%
                                filter(!is.na(OUT_BMD4)) %>%    # Outward stocks data needs to be non-missing
                                group_by(s_iso3c, r_iso3c) %>%
                                mutate( lag1_diff = lag(diff_inBMD4_outBMD4, n=1, default=NA,order_by = year),
                                        lag2_diff = lag(diff_inBMD4_outBMD4, n=2, default=NA,order_by = year),
                                        lag3_diff = lag(diff_inBMD4_outBMD4, n=3, default=NA,order_by = year),
                                        lag4_diff = lag(diff_inBMD4_outBMD4, n=4, default=NA,order_by = year),
                                        lead1_diff = lead(diff_inBMD4_outBMD4, n=1, default=NA, order_by = year),
                                        lead2_diff = lead(diff_inBMD4_outBMD4, n=2, default=NA, order_by = year),
                                        lead3_diff = lead(diff_inBMD4_outBMD4, n=3, default=NA, order_by = year),
                                        lead4_diff = lead(diff_inBMD4_outBMD4, n=4, default=NA, order_by = year))%>%
                                ungroup()
  
# generate working data without any missings
working_data_wom <- select(-s_iso3c, -r_iso3c, -year, -diff_inBMD4_outBMD4, -IN_BMD4)
working_data_wom <- impute(data = working_data_wom, 
                           mf.q = 0.05,             #mForest grouping variables into 10 groups
                      splitrule = "random",         #using random split points instead of calculating the optimal one
                         nsplit = 5,                #random split point selection
                         # fast = T                 #makes it faster, but less accurate
                             )  
working_data_wom <- cbind(s_iso3c = working_data_wm$s_iso3c,
                          r_iso3c = working_data_wm$r_iso3c,
                             year = working_data_wm$year,
              diff_inBMD4_outBMD4 = working_data_wm$diff_inBMD4_outBMD4,
                          IN_BMD4 = working_data_wm$IN_BMD4,
                           working_data_wom)

#partition into training data and prediction data
training_data_wm <- working_data_wm %>% 
                   filter(!is.na(diff_inBMD4_outBMD4)) #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)                # will be used to build training and validation data
prediction_data_wm <- working_data_wm  %>%
                   filter(!is.na(OUT_BMD4) & is.na(IN_BMD4)) #& OUT_BMD4!=0 & OUT_BMD4<10000 )         #select observations that only have outward FDI stocks

prediction_data_wom <- working_data_wom  %>%
                   filter(!is.na(OUT_BMD4) & is.na(IN_BMD4))         # prediction without missings
training_data_wom <- working_data_wom %>% 
                   filter(!is.na(diff_inBMD4_outBMD4))                # training and validation data without missings


######################assign training and test data############################

sample1 <- createDataPartition(training_data_wm$diff_inBMD4_outBMD4,    #dependent variable
                               times = 1,                            # number of partitions to create
                               list = F,                             # return a list?
                               groups = 10,                          # how many percentiles should the dep. variable be stratified over?
                               p = .8                               # percentage that goes to selected group
                               )

train_complete_wm <- training_data_wm[sample1,] %>% as.data.frame()     #complete training data with missings
test_complete_wm <- training_data_wm[-sample1,] %>% as.data.frame()     #complete validation set with missings

train_complete_wom <- training_data_wom[sample1,]             #complete training data without missings
test_complete_wom <- training_data_wom[-sample1,]             #complete validation set without missings



########################prepare data for training##############################

###############Training data################
#exclude variables that should not be used for prediction
train_data_wm <- train_complete_wm %>% select(-diff_inBMD4_outBMD4,-s_iso3c,-r_iso3c,-IN_BMD4) #includes year

#one-hot endcoding
xgb_train_num <- train_data_wm %>% select(where(is.numeric))
xgb_train_fac <- train_data_wm %>% select(where(is.factor))
xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
xgb_train <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
xgb_train_label <- train_complete_wm$diff_inBMD4_outBMD4 %>% as.matrix()
xgb_train <- xgb.DMatrix(data = xgb_train, label = xgb_train_label ) #combining matrix and label to xgb matrix

###############Test data################
test_data_wm <- test_complete_wm %>% select(-diff_inBMD4_outBMD4,-s_iso3c,-r_iso3c,-IN_BMD4) #includes year

#one-hot endcoding
xgb_test_num <- test_data_wm %>% select(where(is.numeric))
xgb_test_fac <- test_data_wm %>% select(where(is.factor))
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_test_fac))

#combine to xgb Matrix
xgb_test <- cbind(xgb_test_num, xgb_single_answers) %>% as.matrix()
xgb_test_label <- test_complete_wm$diff_inBMD4_outBMD4 %>% as.matrix()
xgb_test <- xgb.DMatrix(data = xgb_test, label = xgb_test_label)

###############Prediction data################
predict_data_wm <- prediction_data_wm %>% select(-diff_inBMD4_outBMD4,-s_iso3c,-r_iso3c,-IN_BMD4) 

#one-hot endcoding
xgb_predict_num <- predict_data_wm %>% select(where(is.numeric))
xgb_predict_fac <- predict_data_wm %>% select(where(is.factor))
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_predict_fac))

#combine to xgb Matrix
xgb_predict <- cbind(xgb_predict_num, xgb_single_answers) %>% as.matrix()
xgb_predict_label <- prediction_data_wm$diff_inBMD4_outBMD4 %>% as.matrix()
#xgb_predict <- xgb.DMatrix(data = xgb_predict, label = xgb_predict_label) does not work due to missings in label...

###############Data without missings################
train_data_wom <- train_complete_wom %>% select(-s_iso3c,-r_iso3c,-year,-IN_BMD4)
test_data_wom <- test_complete_wom %>% select(-s_iso3c,-r_iso3c,-year,-IN_BMD4)


########################Train ML Models#########################################

# define watchlist
#weight <- sqrt((train_complete_wm$diff_inBMD4_outBMD4/train_complete_wm$IN_BMD4*100)^2)
#weight[weight>200] <- 200
#weight <- abs(train_complete_wm$diff_inBMD4_outBMD4)/(abs(train_complete_wm$IN_BMD4)+abs(train_complete_wm$OUT_BMD4))

watchlist <- list(train=xgb_train,test=xgb_test)


#Training an XG Forest
forest2 <- xgb.train(
                      data = xgb_train,
                      objective = "reg:squarederror",  # Loss-function. Can also be set to reg:squarederror
                      #eval_metric = "mae",
                      tree_method = "approx",          # method for finding split values 
                      watchlist = watchlist,           # Watchlist defines with dataset to fit the model to while training.
                      nrounds = 1,                     # number of iterations
                      max.depth = 30,                   # Depth of the trees 
                      eta = 1,                         # Learning rate for forests
                      #gamma =,                        # Regularization parameter: minimum loss reduction to parition a leaf
                      subsample = 0.5,                    # Ratio of oberservations used for fitting the next tree
                      colsample_bynode= 0.3,              # Share of random columns (features) used to fit one node
                      num_parallel_tree = 200,              # Number of trees fitted per round -> can be used to simulate RF
                      #interaction_constraints         # List of features that are allowed to be interacted
                      #early_stopping_rounds = 5,        # Stops the training process if the error on the validation set does not improve for k rounds
                      verbose = 1                      # zero means no update printed in the console
)
#xgb.save(forest2, "xgforest.model")
importance_forest <- xgb.importance(model = forest2) #saves the most important variables (uses rel. influence measure)
forest_first10 <- importance_forest$Feature %>% head(10)

                          


# training the xgb boost model
boost2 <- xgb.train(
                   data = xgb_train,
                   objective = "reg:squarederror",  # Loss-function. Can also be set to pseudohubererror
                   #weight = weight,
                   #eval_metric = "mae",
                   min_child_weight = 0,
                   tree_method = "approx",          # method for finding split values
                   watchlist = watchlist,           # Watchlist defines with dataset to fit the model to while training.
                   nrounds = 500,                     # number of iterations
                   max.depth = 8,                   # Depth of the trees 
                   eta = 0.1,                       # Learning rate
                   gamma = 0,                        # Regularization parameter: minimum loss reduction to parition a leaf
                   subsample = 0.4,                    # Ratio of oberservations used for fitting the next tree
                   colsample_bytree= 1,              # Share of random columns (features) used to fit one tree
                   #num_parallel_tree = 10,              # Number of trees fitted per round -> can be used to simulate RF
                   #interaction_constraints         # List of features that are allowed to be interacted
                   #early_stopping_rounds = 50,        # Stops the training process if the error on the validation set does not improve for k rounds
                   verbose = 1                      # zero means no update printed in the console
)

#xgb.save(boost2, "xgboost.model")     # save model, when using caret package: xgb.save.raw(boost2) 
#boost2 <- xgb.load("xgboost.model")  # load model
importance_boost <- xgb.importance(model = boost2) #saves the most important variables (uses rel. influence measure)
boost_first10 <- importance_boost$Feature %>% head(10)

#linear model with forward selection
#       needs to be implemented.....


####################Predict and compare in-sample and out-of-sample performance##############
# could be upped to k-fold cross validation. For now just a simple validation set approach.
# Mean squared prediction error: The average distance between the prediction and the actual observation.

# In-sample comparison R squared
is_resid_forest <- train_complete_wm$diff_inBMD4_outBMD4 - predict(forest2, xgb_train)
rsq_forest <- round(1-(sum((is_resid_forest)^2)/sum((train_complete_wm$diff_inBMD4_outBMD4-mean(train_complete_wm$diff_inBMD4_outBMD4))^2)), digits = 2)
is_imprate_forest <- round(mean(abs(train_complete_wm$diff_inBMD4_outBMD4)-abs(is_resid_forest)>0), digits=2)

is_resid_boost <- train_complete_wm$diff_inBMD4_outBMD4 - predict(boost2, xgb_train)
rsq_boost <- round(1-(sum((is_resid_boost)^2)/sum((train_complete_wm$diff_inBMD4_outBMD4-mean(train_complete_wm$diff_inBMD4_outBMD4))^2)), digits = 2)
is_imprate_boost <- round(mean(abs(train_complete_wm$diff_inBMD4_outBMD4)-abs(is_resid_boost)>0), digits=2)

#is_resid_linear <- train_final$diff_inBMD4_outBMD4 - predict(linear, train_final)


#Out-of-sample Prediction
os_resid_forest <- test_complete_wm$diff_inBMD4_outBMD4 - predict(forest2, xgb_test)
os_imprate_forest <- round(mean(abs(test_complete_wm$diff_inBMD4_outBMD4)-abs(os_resid_forest)>0), digits=2)
os_resid_boost <- test_complete_wm$diff_inBMD4_outBMD4 - predict(boost2, xgb_test)
os_imprate_boost <- round(mean(abs(test_complete_wm$diff_inBMD4_outBMD4)-abs(os_resid_boost)>0), digits=2)

#Root Means Squared Error (RMSE): emphasis of outliers
rmse_forest <- round(sqrt(mean(os_resid_forest^2)), digits =0)
rmse_boost <- round(sqrt(mean(os_resid_boost^2)), digits = 0)

#Mean absolute error: The mean of all (absolute) prediction errors. Considers outliers less.
mae_forest <- round(mean(sqrt(os_resid_forest^2)), digits = 0)
mae_boost <- round(mean(sqrt(os_resid_boost^2)), digits = 0)

#alternatively r2
pseudo_rsq_forest <- round(1-(sum((os_resid_forest)^2)/sum((test_complete_wm$diff_inBMD4_outBMD4)^2)), digits=2)
pseudo_rsq_boost <- round(1-(sum((os_resid_boost)^2)/sum((test_complete_wm$diff_inBMD4_outBMD4)^2)), digits=2)

ErrorRed_forest <- round(1-(sum(abs(os_resid_forest)))/sum(abs(test_complete_wm$diff_inBMD4_outBMD4)), digits=2)
ErrorRed_boost <- round(1-(sum(abs(os_resid_boost)))/sum(abs(test_complete_wm$diff_inBMD4_outBMD4)), digits=2)

#Visualization
#cbind(prediction=prediction1,real_diff=test1$real_diff_inBMD4_outBMD4) %>% as.data.frame() %>%
#  ggplot() + geom_jitter(mapping = aes(x=prediction, y=real_diff))


# Comparing model performances
perf_forest <- c(
                #Training = "",
                # IMPrate = is_imprate_forest,
                 Rsq =  rsq_forest,
                 #Test = "",
                 MAE = mae_forest,
                 RMSE = rmse_forest,
                 #osIMPrate = os_imprate_forest,
                 pRsq = pseudo_rsq_forest,
                 ErrorRed = ErrorRed_forest)
perf_boost <- c(
               #Training = "",
                #IMPrate = os_imprate_forest,
                Rsq =  rsq_boost,
                #Test = "",
                MAE = mae_boost,
                RMSE = rmse_boost,
                #osIMPrate = os_imprate_boost,
                pRsq = pseudo_rsq_boost,
                ErrorRed = ErrorRed_boost)
comparison <- data.frame(Forest = perf_forest, GBM = perf_boost) 
print(comparison)

#export to excel
write.csv(comparison, "C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/estimation/model_comparison", row.names=T)
print(xtable(comparison, type = "latex"), file = "C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/tex Tables/model_comparison.tex")

#get first ten variables
first10 <- cbind(Forest = forest_first10,
                 GBM = boost_first10)

write.csv(first10, "C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/estimation/first_ten", row.names=T)
print(xtable(first10, type = "latex"), file = "C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/tex Tables/first10.tex")

####################Predict FDI inflows with the best Model####################


#predict both using both models and export dataset
final_prediction_forest <- predict(forest2, xgb_predict)
final_prediction_boost <- predict(boost2, xgb_predict)

ml_prediction <- data.frame(s_iso3c = prediction_data_wm$s_iso3c,
                            r_iso3c = prediction_data_wm$r_iso3c,
                            year = prediction_data_wm$year,
                            pred_boost = final_prediction_boost,
                            pred_forest = final_prediction_forest)

write.dta(ml_prediction, 'C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/Do files/ml_prediction.dta',
          convert.factors = "string")

#predict the whole dataset

prediction_train <- predict(boost2, xgb_train)
prediction_train <- data.frame(s_iso3c = train_complete_wm$s_iso3c,
                               r_iso3c = train_complete_wm$r_iso3c,
                               year = train_complete_wm$year, 
                               prediction = prediction_train,
                               set = "train")
prediction_test <- predict(boost2, xgb_test)
prediction_test <- data.frame(s_iso3c = test_complete_wm$s_iso3c,
                         r_iso3c = test_complete_wm$r_iso3c,
                         year = test_complete_wm$year, 
                         prediction = prediction_test,
                         set = "test")
prediction_predict <- predict(boost2, xgb_predict)
prediction_predict <- data.frame(s_iso3c = prediction_data_wm$s_iso3c,
                              r_iso3c = prediction_data_wm$r_iso3c,
                              year = prediction_data_wm$year, 
                              prediction = prediction_predict,
                              set = "predict")

prediction_all <- rbind(prediction_train, prediction_test, prediction_predict)
save(prediction_all,file = "prediction_all.rdata")
write.dta(prediction_all, 'C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/Do files/prediction_all.dta',
          convert.factors = "string")

