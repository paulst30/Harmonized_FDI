
#### preparation of cv-fold ##### 

#defines the resampling method
fitControl <- trainControl(method = "repeatedcv",    # method for resampling 
                           number = 10,              # k in k-fold resampling 
                           repeats = 1,              # repetition of resampling (3 times 10-fold crossvalidation)
                           savePredictions = TRUE    # save prediction during fitting process
)

#defines the tuning Grid
customGrid <-  expand.grid(nrounds = c(200), 
                           max_depth = c(4), 
                           eta = c(0.3),
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 0,
                           subsample = c(0.4)
)

#nrounds, max_depth, eta, gamma, colsample_bytree, min_child_weight, subsample


# k-fold crossvalidated boosted tree
boost_fdiff_cv <- train(y = dep_difffellow$diff_fellow, 
                 x = xgb_train_difffellow_sp, 
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

forest_fdiff_cv <- train(y = dep_difffellow$diff_fellow, 
                  x = xgb_train_difffellow_sp, 
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



prediction_train_fdiff <-data.frame(s_iso3c = dep_difffellow$s_iso3c,
                                    r_iso3c = dep_difffellow$r_iso3c,
                                       year = dep_difffellow$year,
                                       diff = dep_difffellow$diff_fellow)

# save predictions from cv-model
boost_fdiff_pred <- data.frame(rowIndex= boost_fdiff_cv$pred$rowIndex[boost_fdiff_cv$pred$nrounds==200 & boost_fdiff_cv$pred$max_depth==4 & boost_fdiff_cv$pred$eta==0.3],
                               pred= boost_fdiff_cv$pred$pred[boost_fdiff_cv$pred$nrounds==200 & boost_fdiff_cv$pred$max_depth==4 & boost_fdiff_cv$pred$eta==0.3])

forest_fdiff_pred <- data.frame(rowIndex= forest_fdiff_cv$pred$rowIndex[forest_fdiff_cv$pred$nrounds==1 & forest_fdiff_cv$pred$max_depth==20 & forest_fdiff_cv$pred$eta==1],
                                pred= forest_fdiff_cv$pred$pred[forest_fdiff_cv$pred$nrounds==1 & forest_fdiff_cv$pred$max_depth==20 & forest_fdiff_cv$pred$eta==1])

# combine with dependent variable
prediction_train_fdiff$boost[boost_fdiff_pred$rowIndex] <- boost_fdiff_pred$pred      
prediction_train_fdiff$forest[forest_fdiff_pred$rowIndex] <- forest_fdiff_pred$pred

#################measure and compare performance###############################

#performance measurement
prediction_summary_fdiff <- prediction_train_fdiff %>% pivot_longer(cols = c(boost, forest), names_to = "method", values_to = "prediction") %>%
  group_by(method) %>%
  summarize(rmse = round(sqrt(mean((diff-prediction)^2)), digits = 0),
            mae = round(mean(sqrt((diff-prediction)^2)), digits = 0),
            pseudo_rsq = round(1-(sum((diff-prediction)^2)/sum((diff)^2)), digits=3),
            N = n())

#quintile performance
quin_perf_fdiff <- prediction_train_fdiff %>% 
  mutate(quintile = ntile(boost, 10)) %>%
  group_by(quintile) %>%
  summarise(
    min = min(boost),
    max = max(boost),
    Errorred = round(1-(sum(abs(diff-boost), na.rm=T))/sum(abs(diff), na.rm=T), digits=2),
    pRsquared = round(1-(sum((diff-boost)^2, na.rm=T)/sum((diff)^2, na.rm=T)), digits=2)
  )
print(quin_perf_fdiff)
