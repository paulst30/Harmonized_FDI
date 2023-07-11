
#### preparation of cv-fold ##### 

#defines the resampling method
#folds <- groupKFold(dep_tdiff$des_pair, k = 10)

fitControl <- trainControl(method = "repeatedcv",    # method for resampling 
                           number = 10,              # k in k-fold resampling 
                           repeats = 1,              # repetition of resampling (3 times 10-fold crossvalidation)
                           savePredictions = TRUE,    # save prediction during fitting process
                           #index = folds              # designate complete pairs to hold-out/training sets
                           )

#defines the tuning Grid
boost_grid <-  expand.grid(nrounds = c(200, 500), 
                           max_depth = c(5, 10), 
                           eta = c(0.1, 0.2),
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(1)
                           )

# #weights
# weights <- log(abs(predict(weight_function, newdata = dep_tdiff)))
# weights <-1/(0.5+(weights-min(weights))*2.5/(max(weights)-min(weights)))

#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample


# k-fold crossvalidated boosted tree
boost_tdiff_cv <- train(y = dep_tdiff$dep_var, 
                 x = xgb_train_tdiff_sp, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 tuneGrid = boost_grid,
                 #weights = weights,
                 #other parameters to pass to xgboost training function:
                 objective = "reg:squarederror",  # reg:pseudohubererror
                 tree_method = "approx",
                 verbose = FALSE
                 )
#xgb.save(boost_tdiff_cv$finalModel, "boost_tdiff.model")
importance_boost_tdiff <- varImp(boost_tdiff_cv)
getTrainPerf(boost_tdiff_cv)
boost_tdiff_cv$bestTune
varimps[i] <- list(importance_boost_tdiff$importance)
besttune[i] <- list(boost_tdiff_cv$bestTune)

# # k-fold crossvalidated forest
# forest_grid <-  expand.grid(nrounds = 1, 
#                            max_depth = c(20,30), 
#                            eta = 1,
#                            gamma = 0,
#                            colsample_bytree = 1,
#                            min_child_weight = 0,
#                            subsample = c(0.5)
# )
# 
# forest_tdiff_cv <- train(y = dep_tdiff$dep_var, 
#                   x = xgb_train_tdiff_sp, 
#                   method = "xgbTree", 
#                   trControl = fitControl,
#                   tuneGrid = forest_grid,
#                   #other parameters to pass to xgboost training function:
#                   objective = "reg:squarederror",       #can be changed to "pseudohubererror"
#                   tree_method = "approx",
#                   colsample_bynode= 0.5,              # Share of random columns (features) used to fit one node
#                   num_parallel_tree = 300,              # Number of trees fitted per round -> can be used to simulate RF
#                   verbose = FALSE
# )
# #xgb.save(forest_tdiff_cv$finalModel, "forest_tdiff.model")
# #importance_forest_tdiff <- varImp(forest_tdiff_cv)
# 
# 
# #k-fold elastic net 
# glmnet_grid <- expand.grid(alpha = c(0.1,0.5,0.9),
#                            lambda = c(0.01, 0.1 ,0.2))
# 
# glmnet_tdiff_cv <- train(dep_var ~ .,
#                           data = cbind(dep_var=dep_tdiff$dep_var, train_data_tdiff),
#                           method = "glmnet", 
#                           tune_grid = glmnet_grid,
#                           trControl = fitControl,
#                           preProcess = c("knnImpute", "center", "scale", "YeoJohnson"),
#                           na.action = na.pass
#                           )
# # 



# #nnet 
# nnet_data <- train_data_tdiff %>% as.data.frame() %>% mutate(across(where(is.numeric), ~ asinh(.)))
# 
# knn_impute2 <- preProcess(as.data.frame(nnet_data),
#                          method = c("knnImpute"),
#                          k = 5,
#                          knnSummary = mean)
# 
# nnet_data <- predict(knn_impute2, newdata=nnet_data, na.action = na.pass)
# 
# nnet_dep <- scale(asinh(dep_tdiff$diff_inBMD4_outBMD4))
# 
# # nnet_grid <- expand.grid(size = 40,
# #                          decay = 0.5)
# fitControl <- trainControl(method = "repeatedcv",    # method for resampling 
#                            number = 10,              # k in k-fold resampling 
#                            repeats = 1,              # repetition of resampling (3 times 10-fold crossvalidation)
#                            savePredictions = TRUE,    # save prediction during fitting process
#                            search = "random"
# )
# 
# nnet_tdiff_cv <- train(diff_inBMD4_outBMD4 ~ OUT_BMD4 + max_OUT + mean_group + sd_group + 
#                          re_IN_BMD4 + OUT_debt_N + A_ti_T_T + m_re_in_fellow + OUT_debt_N_fin + 
#                          re_IMF_IN_net_fellow,
#                        data = cbind(diff_inBMD4_outBMD4=nnet_dep, nnet_data),
#                        method = "neuralnet", 
#                        #tune_grid = nnet_grid,
#                        tuneLength = 3,
#                        trControl = fitControl,
#                        #preProcess = c( "center", "scale", "YeoJohnson"),   #"knnImpute"
#                        na.action = na.pass
#                        #linout = T
# )


#save out-of-sample predictions
if (i==1) {
prediction_train_tdiff <-data.frame(s_iso3c = dep_tdiff$s_iso3c,
                            r_iso3c = dep_tdiff$r_iso3c,
                            year = dep_tdiff$year,
                            dep_var = dep_tdiff$dep_var,
                            predictor = dep_tdiff$predictor,
                            run = i
                            )
} else {
  merge <- data.frame(s_iso3c = dep_tdiff$s_iso3c,
                      r_iso3c = dep_tdiff$r_iso3c,
                      year = dep_tdiff$year,
                      dep_var = dep_tdiff$dep_var,
                      predictor = dep_tdiff$predictor,
                      run = i
  )
}

# save predictions from cv-model
boost_tdiff_pred <- boost_tdiff_cv$pred %>% filter(nrounds == boost_tdiff_cv$bestTune$nrounds &
                                                     max_depth == boost_tdiff_cv$bestTune$max_depth &
                                                     eta == boost_tdiff_cv$bestTune$eta) %>% 
                                            select(rowIndex, pred)

# forest_tdiff_pred <- forest_tdiff_cv$pred %>% filter(nrounds == forest_tdiff_cv$bestTune$nrounds &
#                                                      max_depth == forest_tdiff_cv$bestTune$max_depth &
#                                                      eta == forest_tdiff_cv$bestTune$eta) %>% 
#                                             select(rowIndex, pred)

# glmnet_tdiff_pred <- glmnet_tdiff_cv$pred %>% filter(alpha == glmnet_tdiff_cv$bestTune$alpha & 
#                                                      lambda == glmnet_tdiff_cv$bestTune$lambda) %>%
#                                               select(rowIndex, pred)




# combine with dependent variable
if (i==1){
  prediction_train_tdiff$boost[boost_tdiff_pred$rowIndex] <- boost_tdiff_pred$pred
} else {
  merge$boost[boost_tdiff_pred$rowIndex] <- boost_tdiff_pred$pred
  prediction_train_tdiff <- rbind(prediction_train_tdiff, merge)
}
     
# prediction_train_tdiff$forest[forest_tdiff_pred$rowIndex] <- forest_tdiff_pred$pred
# prediction_train_tdiff$glmnet[glmnet_tdiff_pred$rowIndex] <- glmnet_tdiff_pred$pred

#################measure and compare performance###############################

#performance on training set


# prediction_summary_tdiff <- rbind(getTrainPerf(boost_tdiff_cv),
#                                   getTrainPerf(forest_tdiff_cv),
#                                   getTrainPerf(glmnet_tdiff_cv))
# print(prediction_summary_tdiff)
#,
#                                  getTrainPerf(glmnet_tdiff_cv))
#write.csv(prediction_summary_tdiff, row.names=T)

#performance on the test set
prediction_test_tdiff <- data.frame(boost = predict(boost_tdiff_cv, newdata =  xgb_test_tdiff_sp)#,
                                    #forest = predict(forest_tdiff_cv , newdata = xgb_test_tdiff_sp),
                                    #glmnet = predict(glmnet_tdiff_cv, newdata = test_data_tdiff)
                                    ) %>%
                                    apply(., 2, postResample, obs = dep_test_tdiff$dep_var) %>%
                                    t()
#save performance
prediction_summary_tdiff[i,]<- c(getTrainPerf(boost_tdiff_cv), prediction_test_tdiff)


#quintile performance
quin_perf_tdiff <- prediction_train_tdiff %>% 
  mutate(quintile = ntile(predictor, 10)) %>%
  #filter( OUT_BMD4 !=0 ) %>%
  group_by(quintile) %>%
  summarise(
    min = min(predictor),
    max = max(predictor),
    ImpRate = round(mean(abs(dep_var-boost)<=abs(dep_var-predictor)) ,digits = 2),
    Errorred = round(1-(sum(abs(dep_var-boost), na.rm=T)/sum(abs(dep_var-predictor), na.rm=T)), digits=2),
    pRsquared = round(1-(sum((dep_var-boost)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2)
  )
print(quin_perf_tdiff)
quin_perfs[i] <- list(quin_perf_tdiff)
#write.csv(quin_perf_tdiff, row.names=T)


# 
# 
# #graph histogram of rel measurement error
# hist_diff1 <- ggplot(data = graph_data_tdiff) + geom_histogram(aes(x=rel_error2, y=after_stat(count/sum(count)*100))) #+
#   #facet_wrap(~ fin_center, ncol = 2) 
# hist_adjust1 <- ggplot(data =graph_data_tdiff) + geom_histogram(aes(x=adj_rel_error2, y=after_stat(count/sum(count)*100))) #+
#  # facet_wrap(~ fin_center, ncol = 2) 
# plot_grid(hist_diff1, hist_adjust1, nrow = 2)

#prediction 

if (i == 1) {
  prediction_tdiff <- data.frame(s_iso3c = dep_pred_tdiff$s_iso3c,
                                    r_iso3c = dep_pred_tdiff$r_iso3c,
                                    year = dep_pred_tdiff$year,
                                    boost = predict(boost_tdiff_cv, newdata =  xgb_pred_tdiff_sp),
                                    target = paste(target),
                                predictor=paste(predictor))
} else {
  merge <- data.frame(s_iso3c = dep_pred_tdiff$s_iso3c,
                      r_iso3c = dep_pred_tdiff$r_iso3c,
                      year = dep_pred_tdiff$year,
                      boost = predict(boost_tdiff_cv, newdata =  xgb_pred_tdiff_sp),
                      target = paste(target),
                      predictor = paste(predictor))
  prediction_tdiff <- rbind(prediction_tdiff, merge)
}

