
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
boost_tdiff_cv <- train(y = dep_tdiff$diff_inBMD4_outBMD4, 
                 x = xgb_train_tdiff_sp, 
                 method = "xgbTree", 
                 trControl = fitControl,
                 tuneGrid = customGrid,
                 #other parameters to pass to xgboost training function:
                 objective = "reg:squarederror",
                 tree_method = "approx",
                 verbose = FALSE
                 )
xgb.save(boost_tdiff_cv$finalModel, "boost_tdiff.model") 


# k-fold crossvalidated forest
customGrid <-  expand.grid(nrounds = 1, 
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
                  tuneGrid = customGrid,
                  #other parameters to pass to xgboost training function:
                  objective = "reg:squarederror",       #can be changed to "pseudohubererror"
                  tree_method = "approx",
                  colsample_bynode= 0.5,              # Share of random columns (features) used to fit one node
                  num_parallel_tree = 300,              # Number of trees fitted per round -> can be used to simulate RF
                  verbose = FALSE
)
xgb.save(forest_tdiff_cv$finalModel, "forest_tdiff.model") 

#save out-of-sample predictions
prediction_train_tdiff <-data.frame(s_iso3c = dep_tdiff$s_iso3c,
                            r_iso3c = dep_tdiff$r_iso3c,
                            year = dep_tdiff$year,
                            diff = dep_tdiff$diff_inBMD4_outBMD4)

# save predictions from cv-model
boost_tdiff_pred <- data.frame(rowIndex= boost_tdiff_cv$pred$rowIndex[boost_tdiff_cv$pred$nrounds==200 & boost_tdiff_cv$pred$max_depth==4 & boost_tdiff_cv$pred$eta==0.3],
                               pred= boost_tdiff_cv$pred$pred[boost_tdiff_cv$pred$nrounds==200 & boost_tdiff_cv$pred$max_depth==4 & boost_tdiff_cv$pred$eta==0.3])

forest_tdiff_pred <- data.frame(rowIndex= forest_tdiff_cv$pred$rowIndex[forest_tdiff_cv$pred$nrounds==1 & forest_tdiff_cv$pred$max_depth==20 & forest_tdiff_cv$pred$eta==1],
                                pred= forest_tdiff_cv$pred$pred[forest_tdiff_cv$pred$nrounds==1 & forest_tdiff_cv$pred$max_depth==20 & forest_tdiff_cv$pred$eta==1])

# combine with dependent variable
prediction_train_tdiff$boost[boost_tdiff_pred$rowIndex] <- boost_tdiff_pred$pred      
prediction_train_tdiff$forest[forest_tdiff_pred$rowIndex] <- forest_tdiff_pred$pred

#################measure and compare performance###############################

#performance measurement
prediction_summary_tdiff <- prediction_train_tdiff %>% pivot_longer(cols = c(boost, forest), names_to = "method", values_to = "prediction") %>%
  group_by(method) %>%
  summarize(rmse = round(sqrt(mean((diff-prediction)^2)), digits = 0),
            mae = round(mean(sqrt((diff-prediction)^2)), digits = 0),
            pseudo_rsq = round(1-(sum((diff-prediction)^2)/sum((diff)^2)), digits=3),
            N = n())

#quintile performance
quin_perf_tdiff <- prediction_train_tdiff %>% 
  mutate(quintile = ntile(boost, 10)) %>%
  group_by(quintile) %>%
  summarise(
    min = min(boost),
    max = max(boost),
    Errorred = round(1-(sum(abs(diff-boost), na.rm=T))/sum(abs(diff), na.rm=T), digits=2),
    pRsquared = round(1-(sum((diff-boost)^2, na.rm=T)/sum((diff)^2, na.rm=T)), digits=2)
  )
print(quin_perf_tdiff)

#graphical analysis of the performance

graph_data_tdiff <- data %>% select(s_iso3c, r_iso3c, year, group_id, IN_BMD4, OUT_BMD4, fin_center) %>%
                             merge(prediction_train_tdiff, by = c("s_iso3c", "r_iso3c", "year"), y.all = T ) %>%
                             mutate(adj_OUT_BMD4=OUT_BMD4+boost, 
                                    rel_error2=2*abs(IN_BMD4-OUT_BMD4)/(abs(IN_BMD4)+abs(OUT_BMD4)),
                                    adj_rel_error2=2*abs(IN_BMD4-adj_OUT_BMD4)/(abs(IN_BMD4)+abs(adj_OUT_BMD4)))

# graph Inward versus outward, distinction by fin_center, comparison to prediction
diff1 <- ggplot(data = graph_data_tdiff) + geom_jitter(aes(y=IN_BMD4, x=OUT_BMD4, color=rel_error2)) +
                                     facet_wrap(~ fin_center, ncol = 2) #+
#coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
#labs(title = "Differences between inward and outward stocks",
#      caption = "N=XX")
adjust1 <- ggplot(data = graph_data_tdiff) + geom_jitter(aes(y=IN_BMD4, x=adj_OUT_BMD4, color=adj_rel_error2)) +
                                       facet_wrap(~ fin_center, ncol = 2) #+
# coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
plot_grid(diff1, adjust1, nrow = 2)                            


#graph histogram of rel measurement error
hist_diff1 <- ggplot(data = graph_data_tdiff) + geom_histogram(aes(x=rel_error2, y=after_stat(count/sum(count)*100))) +
  facet_wrap(~ fin_center, ncol = 2) 
hist_adjust1 <- ggplot(data =graph_data_tdiff) + geom_histogram(aes(x=adj_rel_error2, y=after_stat(count/sum(count)*100))) +
  facet_wrap(~ fin_center, ncol = 2) 
plot_grid(hist_diff1, hist_adjust1, nrow = 2)



