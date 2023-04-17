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

working_data_wm <- data %>% select(s_iso3c, r_iso3c, year, diff_inBMD4_outBMD4, re_diff_inBMD4_outBMD4, 
                                   IN_BMD4,OUT_BMD4, rownames(coverage_30), re_OUT_BMD4,
                                   -Type, -re_IMF_IN, -re_OECD_IN_BMD4, diff_fellow) %>%
  mutate(fel_diff=IN_BMD4-OUT_BMD4+re_IN_BMD4-re_OUT_BMD4) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(s_iso3c, r_iso3c) %>%
  mutate( lag1_diff = lag(diff_inBMD4_outBMD4, n=1, default=NA,order_by = year),
          lag2_diff = lag(diff_inBMD4_outBMD4, n=2, default=NA,order_by = year),
          lag3_diff = lag(diff_inBMD4_outBMD4, n=3, default=NA,order_by = year),
          lag4_diff = lag(diff_inBMD4_outBMD4, n=4, default=NA,order_by = year),
          lead1_diff = lead(diff_inBMD4_outBMD4, n=1, default=NA, order_by = year),
          lead2_diff = lead(diff_inBMD4_outBMD4, n=2, default=NA, order_by = year),
          lead3_diff = lead(diff_inBMD4_outBMD4, n=3, default=NA, order_by = year),
          lead4_diff = lead(diff_inBMD4_outBMD4, n=4, default=NA, order_by = year),
          lag1_rediff = lag(re_diff_inBMD4_outBMD4, n=1, default=NA,order_by = year),
          lag2_rediff = lag(re_diff_inBMD4_outBMD4, n=2, default=NA,order_by = year),
          lag3_rediff = lag(re_diff_inBMD4_outBMD4, n=3, default=NA,order_by = year),
          lag4_rediff = lag(re_diff_inBMD4_outBMD4, n=4, default=NA,order_by = year),
          lead1_rediff = lead(re_diff_inBMD4_outBMD4, n=1, default=NA, order_by = year),
          lead2_rediff = lead(re_diff_inBMD4_outBMD4, n=2, default=NA, order_by = year),
          lead3_rediff = lead(re_diff_inBMD4_outBMD4, n=3, default=NA, order_by = year),
          lead4_rediff = lead(re_diff_inBMD4_outBMD4, n=4, default=NA, order_by = year)
  ) %>%
  ungroup()

working_data_summary <- working_data_wm %>% filter(is.na(diff_fellow)) %>%
  summarize(lag1diff_f=sum(!is.na(lag1_diff_f)),
            lag2diff_f=sum(!is.na(lag2_diff_f)),
            lag3diff_f=sum(!is.na(lag3_diff_f)),
            lag4diff_f=sum(!is.na(lag4_diff_f)),
            lag1diff=sum(!is.na(lag1_diff)),
            lag2diff=sum(!is.na(lag2_diff)),
            lag3diff=sum(!is.na(lag3_diff)),
            lag4diff=sum(!is.na(lag4_diff)))



###############Training data################
training_data_wm <- working_data_wm %>% filter(!is.na(diff_fellow))

#exclude variables that should not be used for prediction
train_data_wm <- training_data_wm %>% select(-s_iso3c,-r_iso3c, -diff_fellow)

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
customGrid <-  expand.grid(nrounds = c(200), 
                           max_depth = c(8), 
                           eta = c(0.2),
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(0.4)
)

#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample


# k-fold crossvalidated boosted tree
boostcv <- train(y = training_data_wm$diff_fellow, 
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

forestcv <- train(y = training_data_wm$diff_fellow, 
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
                            diff = training_data_wm$diff_fellow)

# save predictions from cv-model
#final_boost_pred <- data.frame(rowIndex= boostcv$pred$rowIndex[boostcv$pred$nrounds==1000 & boostcv$pred$max_depth==4 & boostcv$pred$eta==0.1],
#                               pred= boostcv$pred$pred[boostcv$pred$nrounds==1000 & boostcv$pred$max_depth==4 & boostcv$pred$eta==0.1])

#final_forest_pred <- data.frame(rowIndex= forestcv$pred$rowIndex[forestcv$pred$nrounds==1000 & forestcv$pred$max_depth==4 & forestcv$pred$eta==0.1],
#                                pred= forestcv$pred$pred[forestcv$pred$nrounds==1000 & forestcv$pred$max_depth==4 & forestcv$pred$eta==0.1])

prediction_all$boost[boostcv$pred$rowIndex] <- boostcv$pred$pred      
prediction_all$forest[forestcv$pred$rowIndex] <- forestcv$pred$pred

#performance measurement
prediction_summary <- prediction_all %>% pivot_longer(cols = c(boost, forest), names_to = "method", values_to = "prediction") %>%
  group_by(method) %>%
  summarize(rmse = round(sqrt(mean((diff-prediction)^2)), digits = 0),
            mae = round(mean(sqrt((diff-prediction)^2)), digits = 0),
            pseudo_rsq = round(1-(sum((diff-prediction)^2)/sum((diff)^2)), digits=3),
            N = n())

#quintile performance
quin_perf_fellow <- prediction_all %>% 
  mutate(quintile = ntile(prediction, 10)) %>%
  group_by(quintile) %>%
  summarise(
    min = min(prediction),
    max = max(prediction),
    Errorred = round(1-(sum(abs(diff-prediction), na.rm=T))/sum(abs(diff), na.rm=T), digits=2),
    pRsquared = round(1-(sum((diff-prediction)^2, na.rm=T)/sum((diff)^2, na.rm=T)), digits=2)
  )
print(quin_perf_fellow) 


ggplot(data=prediction_all) + geom_point(aes(x=prediction, y=diff))