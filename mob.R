
  group_by(des_pair) %>%
  mutate(n_predictor = sum(!is.na(predictor) & !is.na(dep_var)),
         n_IIP = sum(!is.na(IIP_inward) & !is.na(dep_var)),
         n_PI = sum(!is.na(A_ti_T_T) &  !is.na(dep_var)),
         n_zero = sum(predictor==0, na.rm = T),
         mean_predictor = mean(predictor, na.rm = T),
         m_spot_share = mean(spot_share, na.rm =T),
         m_IIP_share = mean(IIP_share, na.rm = T),
         m_PI_share = mean(PI_share, na.rm = T),
         rsd_dep_var = sd(dep_var, na.rm = T)/mean(dep_var, na.rm = T),
         sd_spot_share = sd(spot_share, na.rm=T),
         sd_PI_share = sd(PI_share, na.rm = T),
         sd_IIP_share = sd(IIP_share, na.rm =T),
         rsd_predictor = sd(predictor, na.rm = T)/mean(predictor, na.rm=T)) %>%
  ungroup()


# restrict sample 
test_data <- data %>% filter(!is.na(dep_var) & target_var=="OECD_IN_BMD3" & !is.na(predictor)) %>% 
                      select(dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                             n_predictor, n_IIP, n_PI, mean_predictor, m_spot_share, m_IIP_share, m_PI_share,
                             rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)
#replacing NAs with Inf or zeros
test_data <- test_data %>% mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                                  across(starts_with("sd"), ~replace_na(.,Inf)),
                                  across(starts_with("m"), ~replace_na(.,-Inf)),
                                  across(starts_with("n"), ~replace_na(.,0))) %>%
                           mutate(across(starts_with("m_"), ~atan(.)*180/pi),
                                  across(starts_with("sd"), ~atan(.)*180/pi),
                                  across(starts_with("rsd"), ~replace_na(Inf))) %>%
                           mutate(across(starts_with("m_"), round,0),
                                  across(starts_with("sd"), round,0),
                                  across(starts_with("rsd"), round,1))
# simple mob
library(party)
formula = as.formula("dep_var ~ predictor + IIP_inward + A_ti_T_T -1 |
                                             rsd_dep_var + rsd_predictor +
                                             mis_IIP + mis_PI +
                                             sd_spot_share + sd_IIP_share + sd_PI_share + 
                                             m_spot_share + m_IIP_share + m_PI_share + n_predictor + n_IIP + n_PI")

#create folds
folds <- createFolds(train_data_tdiff$dep_var, k = 10, list = F)

for (alpha in c(0.001,0.005)){
  for (fold in 1:10) {
    #generate features on training data only
    analysis_set <- train_data_tdiff[folds!=fold,] %>% group_by(des_pair) %>%
                                                    mutate(n_predictor = sum(!is.na(predictor) & !is.na(dep_var)),
                                                         n_IIP = sum(!is.na(IIP_inward) & !is.na(dep_var)),
                                                         n_PI = sum(!is.na(A_ti_T_T) &  !is.na(dep_var)),
                                                         m_predictor = ntile(m_predictor, n=20),
                                                         m_spot_share = mean(spot_share, na.rm =T),
                                                         m_IIP_share = mean(IIP_share, na.rm = T),
                                                         m_PI_share = mean(PI_share, na.rm = T),
                                                         rsd_dep_var = sd(dep_var, na.rm = T)/mean(dep_var, na.rm = T),
                                                         sd_spot_share = sd(spot_share, na.rm=T),
                                                         sd_PI_share = sd(PI_share, na.rm = T),
                                                         sd_IIP_share = sd(IIP_share, na.rm =T),
                                                         rsd_predictor = sd(predictor, na.rm = T)/mean(predictor, na.rm=T)) %>%
                                                  ungroup() %>%
                                                  select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                                                         n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                                                         rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share) %>%
                                                  mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                                                         across(starts_with("sd"), ~replace_na(.,Inf)),
                                                         across(starts_with("m"), ~replace_na(.,-Inf)),
                                                         across(starts_with("n"), ~replace_na(.,0))) %>%
                                                  mutate(across(ends_with("share"), ~atan(.)*180/pi),
                                                         across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                                                  mutate(across(starts_with("m_"), round,0),
                                                         across(starts_with("sd"), round,0),
                                                         across(starts_with("rsd"), round,1))
    
    #fit the model
    simple_mob <- mob(formula = formula, 
                  data=analysis_set,
                  model = glinearModel,
                  na.action = na.pass,
                  control = mob_control(alpha = alpha,
                                        minsplit = 20,
                                        verbose = F))
    
    # generate assessment set
    merge <- analysis_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,
                                     m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                              unique()
    assessment_set <- train_data_tdiff[folds==fold,] %>% merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
                                                        mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                                                               across(starts_with("sd"), ~replace_na(.,90)),
                                                               across(starts_with("m"), ~replace_na(.,-90)),
                                                               across(starts_with("n"), ~replace_na(.,0)),
                                                               m_predictor = ntile(m_predictor, n=20)) %>%
                                                        select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                                                               n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                                                               rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)
    
    
    #save predictions on assessment set
    if (i==1 & fold==1 & alpha==0.001){
        prediction_train_tdiff <- assessment_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                                  mutate(prediction = predict(simple_mob, newdata=assessment_set),
                                         fold = fold,
                                         alpha = alpha,
                                         run = i)
    } else {
        merge <- assessment_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                 mutate(prediction = predict(simple_mob, newdata=assessment_set),
                        fold = fold,
                        alpha = alpha,
                        run = i)
        prediction_train_tdiff <- rbind(prediction_train_tdiff,merge)
    }
  }
}

# assessing performance
best_tune <- prediction_train_tdiff %>% filter(run == i) %>% group_by(fold, alpha) %>%
                           summarize(MAE = round(mean(abs(dep_var-prediction), na.rm=T), digits=0),
                                  RMSE =sqrt(mean((dep_var-prediction)^2, na.rm=T)),
                                  pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2)) %>%
                         group_by(alpha) %>% 
                          summarize(MAE=mean(MAE),
                                    RMSE=mean(RMSE),
                                    pRsquared=mean(pRsquared)) %>%
                          ungroup() %>% filter(RMSE==min(RMSE))
besttune[i] <- list(best_tune)

#only keep hold-out predictions for the best-tuned model
prediction_train_tdiff <- prediction_train_tdiff %>% filter(run!=i & (run==i & alpha==best_tune$alpha))

# train performance
train_performance <- prediction_train_tdiff %>% filter(run == i) %>% group_by(fold) %>%
                            summarize(MAE = round(mean(abs(dep_var-prediction), na.rm=T), digits=0),
                                      RMSE =sqrt(mean((dep_var-prediction)^2, na.rm=T)),
                                      pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2)) %>%
                            ungroup() %>% 
                            summarize(MAE=mean(MAE),
                                      RMSE=mean(RMSE),
                                      pRsquared=mean(pRsquared))
# quintile performance
prediction_train_tdiff %>% filter(run == i) %>% 
  mutate(quintile = ntile(predictor, 10)) %>%
  group_by(quintile) %>%
  summarise(
    min = min(predictor),
    max = max(predictor),
    ImpRate = round(mean(abs(dep_var-prediction)<=abs(dep_var-predictor)) ,digits = 2),
    Errorred = round(1-(sum(abs(dep_var-prediction), na.rm=T)/sum(abs(dep_var-predictor), na.rm=T)), digits=2),
    pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2)
  )
print(quin_perf_tdiff)
quin_perfs[i] <- list(quin_perf_tdiff)

# train best-tuned model on all the available training data to predict test set
training_set <- train_data_tdiff %>% group_by(des_pair) %>%
                        mutate(n_predictor = sum(!is.na(predictor) & !is.na(dep_var)),
                               n_IIP = sum(!is.na(IIP_inward) & !is.na(dep_var)),
                               n_PI = sum(!is.na(A_ti_T_T) &  !is.na(dep_var)),
                               m_predictor = ntile(m_predictor, n=20),
                               m_spot_share = mean(spot_share, na.rm =T),
                               m_IIP_share = mean(IIP_share, na.rm = T),
                               m_PI_share = mean(PI_share, na.rm = T),
                               rsd_dep_var = sd(dep_var, na.rm = T)/mean(dep_var, na.rm = T),
                               sd_spot_share = sd(spot_share, na.rm=T),
                               sd_PI_share = sd(PI_share, na.rm = T),
                               sd_IIP_share = sd(IIP_share, na.rm =T),
                               rsd_predictor = sd(predictor, na.rm = T)/mean(predictor, na.rm=T)) %>%
                        ungroup() %>%
                        select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                               n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                               rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share) %>%
                        mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                               across(starts_with("sd"), ~replace_na(.,Inf)),
                               across(starts_with("m"), ~replace_na(.,-Inf)),
                               across(starts_with("n"), ~replace_na(.,0))) %>%
                        mutate(across(ends_with("share"), ~atan(.)*180/pi),
                               across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                        mutate(across(starts_with("m_"), round,0),
                               across(starts_with("sd"), round,0),
                               across(starts_with("rsd"), round,1))

#fit the model
train_mob <- mob(formula = formula, 
                  data=training_set,
                  model = glinearModel,
                  na.action = na.pass,
                  control = mob_control(alpha = best_tune$alpha,
                                        minsplit = 20,
                                        verbose = F))

#add features to test set 
merge <- training_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,
                                 m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                                 unique()
test_set <- test_data_tdiff %>% merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
  mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
         across(starts_with("sd"), ~replace_na(.,90)),
         across(starts_with("m"), ~replace_na(.,-90)),
         across(starts_with("n"), ~replace_na(.,0)),
         across(starts_with("rsd_"), ~replace_na(.,Inf)),
         m_predictor = ntile(m_predictor, n=20)) %>%
  select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
         n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
         rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)

# predict test data and save predictions
if (i==1) {
 prediction_test_tdiff <- test_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                                      mutate(prediction=predict(train_mob, newdata=test_set),
                                             run = i) 
} else {
  merge <- test_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                        mutate(prediction=predict(train_mob, newdata=test_set),
                        run = i)
  prediction_test_tdiff <- rbind(prediction_test_tdiff, merge)
}

#calculate test performance
test_performance <- prediction_test_tdiff %>% filter(run==i) %>%
  summarize(test_MAE = round(mean(abs(dep_var-prediction), na.rm=T), digits=0),
            test_RMSE =sqrt(mean((dep_var-prediction)^2, na.rm=T)),
            test_pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2))

#combine performances on test and training sets
prediction_summary_tdiff[i,]<- c(train_performance, test_performance)


#generate features based on modelling data (test and training data combined) for final prediction
modelling_set <- modelling_data %>% group_by(des_pair) %>%
                      mutate(n_predictor = sum(!is.na(predictor) & !is.na(dep_var)),
                             n_IIP = sum(!is.na(IIP_inward) & !is.na(dep_var)),
                             n_PI = sum(!is.na(A_ti_T_T) &  !is.na(dep_var)),
                             m_predictor = ntile(m_predictor, n=20),
                             m_spot_share = mean(spot_share, na.rm =T),
                             m_IIP_share = mean(IIP_share, na.rm = T),
                             m_PI_share = mean(PI_share, na.rm = T),
                             rsd_dep_var = sd(dep_var, na.rm = T)/mean(dep_var, na.rm = T),
                             sd_spot_share = sd(spot_share, na.rm=T),
                             sd_PI_share = sd(PI_share, na.rm = T),
                             sd_IIP_share = sd(IIP_share, na.rm =T),
                             rsd_predictor = sd(predictor, na.rm = T)/mean(predictor, na.rm=T)) %>%
                      ungroup() %>%
                      select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                             n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                             rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share) %>%
                      mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                             across(starts_with("sd"), ~replace_na(.,Inf)),
                             across(starts_with("m"), ~replace_na(.,-Inf)),
                             across(starts_with("n"), ~replace_na(.,0))) %>%
                      mutate(across(ends_with("share"), ~atan(.)*180/pi),
                             across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                      mutate(across(starts_with("m_"), round,0),
                             across(starts_with("sd"), round,0),
                             across(starts_with("rsd"), round,1))
# train final model
final_mob <- mob(formula = formula, 
                 data=modelling_set,
                 model = glinearModel,
                 na.action = na.pass,
                 control = mob_control(alpha = best_tune$alpha,
                                       minsplit = 20,
                                       verbose = F))

#add features to prediction set 
merge <- modelling_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,
                                 m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                              unique()
prediction_set <- prediction_data %>% merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
                              mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                                     across(starts_with("sd"), ~replace_na(.,90)),
                                     across(starts_with("m"), ~replace_na(.,-90)),
                                     across(starts_with("n"), ~replace_na(.,0)),
                                     across(starts_with("rsd_"), ~replace_na(.,Inf)),
                                     m_predictor = ntile(m_predictor, n=20)) %>%
                              select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                                     n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                                     rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)
                            
# predict test data and save predictions
if (i==1) {
prediction_tdiff <- prediction_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                                      mutate(prediction=predict(final_mob, newdata=prediction_set),
                                             target = target,
                                             predictor = predictor)
} else {
merge <- prediction_set %>% select(s_iso3c, r_iso3c, year, dep_var, predictor) %>%
                      mutate(prediction=predict(final_mob, newdata=prediction_set),
                             target = target,
                             predictor = predictor)
prediction_test_tdiff <- rbind(prediction_test_tdiff, merge)
}