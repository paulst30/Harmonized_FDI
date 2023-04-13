#XGboost

#install.packages("drat", repos="https://cran.rstudio.com")
#drat:::addRepo("dmlc")
#install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")
#install.packages("xgboost")
#install.packages("Matrix")
#install.packages("party")
#install.packages("partykit")

library(xgboost)
library(partykit)


########################prepare data for training##############################



###############Training data################
#exclude variables that should not be used for prediction
train_data_wm <- training_data_wm %>% select(-s_iso3c,-r_iso3c,-year,-IN_BMD4 , -diff_inBMD4_outBMD4)

#one-hot endcoding
xgb_train_num <- train_data_wm %>% select(where(is.numeric))
xgb_train_fac <- train_data_wm %>% select(where(is.factor))
xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
xgb_train <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
xgb_train_sp <- as(xgb_train, "dgCMatrix")

#xgb_train_label <- training_data_wm$diff_inBMD4_outBMD4 %>% as.matrix()
#xgb_train <- xgb.DMatrix(data = xgb_train, label = xgb_train_label ) #combining matrix and label to xgb matrix


#### preparation of cv-fold ##### 

#defines the resampling method
fitControl <- trainControl(method = "repeatedcv",    # method for resampling 
                           number = 10,              # k in k-fold resampling 
                           repeats = 1,              # repetition of resampling (3 times 10-fold crossvalidation)
                           savePredictions = TRUE    # save prediction during fitting process
                           )

#defines the tuning Grid
customGrid <-  expand.grid(nrounds = c(200, 500, 1000), 
                           max_depth = c(4,8,10), 
                           eta = c(0.1, 0.2, 0.3),
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(0.4)
                           )

#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample


# k-fold crossvalidated boosted tree
boostcv <- train(y = training_data_wm$diff_inBMD4_outBMD4, 
                 x = xgb_train_sp, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 tuneGrid = customGrid,
                 #other parameters to pass to xgboost training function:
                 objective = "reg:squarederror",
                 tree_method = "approx",
                 verbose = FALSE
                 )
#xgb.save(boostcv$finalModel, "xgboost.model") 


# k-fold crossvalidated forest
customGrid <-  expand.grid(nrounds = 1, 
                           max_depth = 20, 
                           eta = 1,
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(0.5)
)

forestcv <- train(y = training_data_wm$diff_inBMD4_outBMD4, 
                  x = xgb_train_sp, 
                  method = "xgbTree", 
                  trControl = fitControl,
                  tuneGrid = customGrid,
                  #other parameters to pass to xgboost training function:
                  objective = "reg:squarederror",       #can be changed to "pseudohubererror"
                  tree_method = "approx",
                  colsample_bynode= 0.5,              # Share of random columns (features) used to fit one node
                  num_parallel_tree = 200,              # Number of trees fitted per round -> can be used to simulate RF
                  verbose = FALSE
)



prediction_all <-data.frame(s_iso3c = training_data_wm$s_iso3c,
                            r_iso3c = training_data_wm$r_iso3c,
                            year = training_data_wm$year,
                            diff = training_data_wm$diff_inBMD4_outBMD4)

# save predictions from cv-model
final_boost_pred <- data.frame(rowIndex= boostcv$pred$rowIndex[boostcv$pred$nrounds==1000 & boostcv$pred$max_depth==4 & boostcv$pred$eta==0.1],
                               pred= boostcv$pred$pred[boostcv$pred$nrounds==1000 & boostcv$pred$max_depth==4 & boostcv$pred$eta==0.1])

#final_forest_pred <- data.frame(rowIndex= forestcv$pred$rowIndex[forestcv$pred$nrounds==1000 & forestcv$pred$max_depth==4 & forestcv$pred$eta==0.1],
                                pred= forestcv$pred$pred[forestcv$pred$nrounds==1000 & forestcv$pred$max_depth==4 & forestcv$pred$eta==0.1])

prediction_all$prediction[final_boost_pred$rowIndex] <- final_boost_pred$pred      
prediction_all$prediction_f[forestcv$pred$rowIndex] <- forestcv$pred$pred




######MOB tree#######
mob_train_complete <- working_data_wm %>% select(IN_BMD4, OUT_BMD4, starts_with("r_IMF"), starts_with("s_IMF")) %>%
                            filter(!is.na(IN_BMD4) & !is.na(OUT_BMD4))
mob_train <- mob_train_complete[sample1,]
mob_test <- mob_train_complete[-sample1,]

mobtree <- lmtree(IN_BMD4 ~ OUT_BMD4 | ., data=mob_train, minbucket=50, mtry=10)
prediction_mob <- predict(mobtree, newdata=mob_test)

os_resid_mob <- mob_test$IN_BMD4-prediction_mob
rmse_mob <- round(sqrt(mean(os_resid_mob^2)), digits = 0)
mae_mob <- round(mean(sqrt(os_resid_mob^2)), digits = 0)

graph_mob <-cbind(IN_BMD4=mob_test$IN_BMD4,
                       prediction=prediction_mob) %>% as.data.frame()

ggplot(data = graph_mob) + geom_jitter(aes( x=prediction, y=IN_BMD4))
