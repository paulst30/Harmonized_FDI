
#### preparation of cv-fold ##### 

#defines the resampling method
fitControl <- trainControl(method = "repeatedcv",    # method for resampling 
                           number = 10,              # k in k-fold resampling 
                           repeats = 1,              # repetition of resampling (3 times 10-fold crossvalidation)
                           savePredictions = TRUE    # save prediction during fitting process
                           )

#defines the tuning Grid
boost_grid <-  expand.grid(nrounds = c(100, 200, 400), 
                           max_depth = c(5), 
                           eta = c(0.1, 0.2),
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(1)
                           )



#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample


# k-fold crossvalidated boosted tree
boost_tdiff_cv <- train(y = dep_tdiff$diff_inBMD4_outBMD4, 
                 x = xgb_train_tdiff_sp, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 tuneGrid = boost_grid,
                 #other parameters to pass to xgboost training function:
                 objective = "reg:squarederror",
                 tree_method = "approx",
                 verbose = FALSE
                 )
#xgb.save(boost_tdiff_cv$finalModel, "boost_tdiff.model")
importance_boost_tdiff <- varImp(boost_tdiff_cv)


# k-fold crossvalidated forest
forest_grid <-  expand.grid(nrounds = 1, 
                           max_depth = c(20,30), 
                           eta = 1,
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(0.5)
)

forest_tdiff_cv <- train(y = dep_tdiff$diff_inBMD4_outBMD4, 
                  x = xgb_train_tdiff_sp, 
                  method = "xgbTree", 
                  trControl = fitControl,
                  tuneGrid = forest_grid,
                  #other parameters to pass to xgboost training function:
                  objective = "reg:squarederror",       #can be changed to "pseudohubererror"
                  tree_method = "approx",
                  colsample_bynode= 0.5,              # Share of random columns (features) used to fit one node
                  num_parallel_tree = 300,              # Number of trees fitted per round -> can be used to simulate RF
                  verbose = FALSE
)
#xgb.save(forest_tdiff_cv$finalModel, "forest_tdiff.model")
#importance_forest_tdiff <- varImp(forest_tdiff_cv)


#k-fold elastic net 
glmnet_grid <- expand.grid(alpha = c(0.1,0.5,0.9),
                           lambda = c(0.01, 0.1 ,0.2))

glmnet_tdiff_cv <- train(diff_inBMD4_outBMD4 ~ .,
                          data = cbind(diff_inBMD4_outBMD4=dep_tdiff$diff_inBMD4_outBMD4, train_data_tdiff),
                          method = "glmnet", 
                          tune_grid = glmnet_grid,
                          trControl = fitControl,
                          preProcess = c("knnImpute"),
                          na.action = na.pass
                          )


#save out-of-sample predictions
prediction_train_tdiff <-data.frame(s_iso3c = dep_tdiff$s_iso3c,
                            r_iso3c = dep_tdiff$r_iso3c,
                            year = dep_tdiff$year,
                            diff = dep_tdiff$diff_inBMD4_outBMD4,
                            new_diff = dep_tdiff$new_diff,
                            #weights= dep_tdiff$weights,
                            IN_BMD4 = dep_tdiff$IN_BMD4,
                            OUT_BMD4 = dep_tdiff$OUT_BMD4)#,
                            #max=dep_tdiff$max_OUT_BMD4)


# save predictions from cv-model
boost_tdiff_pred <- boost_tdiff_cv$pred %>% filter(nrounds == boost_tdiff_cv$bestTune$nrounds &
                                                     max_depth == boost_tdiff_cv$bestTune$max_depth &
                                                     eta == boost_tdiff_cv$bestTune$eta) %>% 
                                            select(rowIndex, pred)

forest_tdiff_pred <- forest_tdiff_cv$pred %>% filter(nrounds == forest_tdiff_cv$bestTune$nrounds &
                                                     max_depth == forest_tdiff_cv$bestTune$max_depth &
                                                     eta == forest_tdiff_cv$bestTune$eta) %>% 
                                            select(rowIndex, pred)

glmnet_tdiff_pred <- glmnet_tdiff_cv$pred %>% filter(alpha == glmnet_tdiff_cv$bestTune$alpha & 
                                                     lambda == glmnet_tdiff_cv$bestTune$lambda) %>%
                                              select(rowIndex, pred)




# combine with dependent variable
prediction_train_tdiff$boost[boost_tdiff_pred$rowIndex] <- boost_tdiff_pred$pred      
prediction_train_tdiff$forest[forest_tdiff_pred$rowIndex] <- forest_tdiff_pred$pred
prediction_train_tdiff$glmnet[glmnet_tdiff_pred$rowIndex] <- glmnet_tdiff_pred$pred

#################measure and compare performance###############################

#performance on training set
prediction_summary_tdiff <- rbind(getTrainPerf(boost_tdiff_cv),
                                  getTrainPerf(forest_tdiff_cv),
                                  getTrainPerf(glmnet_tdiff_cv))
#write.csv(prediction_summary_tdiff, row.names=T)

#performance on the test set
# prediction_test_tdiff <- data.frame(boost = predict(boost_tdiff_cv, newdata =  xgb_test_tdiff_sp),
#                                     forest = predict(forest_tdiff_cv , newdata = xgb_test_tdiff_sp)) %>%
#                                     apply(., 2, postResample, obs = dep_test_tdiff$diff_inBMD4_outBMD4) %>%
#                                     t()


#quintile performance
quin_perf_tdiff <- prediction_train_tdiff %>% 
  mutate(quintile = ntile(diff, 10)) %>%
  group_by(quintile) %>%
  summarise(
    min = min(diff),
    max = max(diff),
    Errorred = round(1-(sum(abs(diff-boost), na.rm=T))/sum(abs(diff), na.rm=T), digits=2),
    pRsquared = round(1-(sum((diff-boost)^2, na.rm=T)/sum((diff)^2, na.rm=T)), digits=2)
  )
print(quin_perf_tdiff)
#write.csv(quin_perf_tdiff, row.names=T)

#graphical analysis of the performance

graph_data_tdiff <- prediction_train_tdiff %>% select(s_iso3c, r_iso3c, year, IN_BMD4, boost, OUT_BMD4) %>% #, max_OUT_BMD4) %>%
                             mutate(adj_OUT_BMD4=OUT_BMD4+boost,
                                    rel_error2=2*abs(IN_BMD4-OUT_BMD4)/(abs(IN_BMD4)+abs(OUT_BMD4)),
                                    adj_rel_error2=2*abs(IN_BMD4-adj_OUT_BMD4)/(abs(IN_BMD4)+abs(adj_OUT_BMD4)))

# graph Inward versus outward, distinction by fin_center, comparison to prediction
diff1 <- ggplot(data = graph_data_tdiff) + geom_jitter(aes(y=IN_BMD4, x=OUT_BMD4)) #+
                                     #facet_wrap(~ fin_center, ncol = 2) #+
#coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
#labs(title = "Differences between inward and outward stocks",
#      caption = "N=XX")
adjust1 <- ggplot(data = graph_data_tdiff) + geom_jitter(aes(y=IN_BMD4, x=adj_OUT_BMD4)) #+
                                       #facet_wrap(~ fin_center, ncol = 2) +
#coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
plot_grid(diff1, adjust1, ncol = 2)                            


#graph histogram of rel measurement error
hist_diff1 <- ggplot(data = graph_data_tdiff) + geom_histogram(aes(x=rel_error2, y=after_stat(count/sum(count)*100))) #+
  #facet_wrap(~ fin_center, ncol = 2) 
hist_adjust1 <- ggplot(data =graph_data_tdiff) + geom_histogram(aes(x=adj_rel_error2, y=after_stat(count/sum(count)*100))) #+
 # facet_wrap(~ fin_center, ncol = 2) 
plot_grid(hist_diff1, hist_adjust1, nrow = 2)

