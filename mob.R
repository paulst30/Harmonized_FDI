#+ A_ti_T_T
#### define formula for the linear regression in the terminal nodes ###
formula = as.formula("dep_var ~ predictor   -1 |
                                             mis_IIP + mis_PI +
                                             sd_spot_share +  sd_PI_share + 
                                             m_spot_share + n_predictor + m_PI_share + m_predictor + n_PI")

#### ten-fold cross validation ####

#create folds
folds <- createFolds(train_data_tdiff$dep_var, k = 10, list = F) # creates ten stratified folds

for (alpha in c(0.0005,0.01)){    # two different values for alpha are considered
  for (fold in 1:10) {            # for each fold, a new model is fitted to 90% of the data to predict the remaining 10%
    print(paste0("run: ", i, "; fold: ", fold)) #prints the progress during running time
    
    #generate features on training data only to aviod data leaking in from the assessment set
    analysis_set <- train_data_tdiff[folds!=fold,] %>% group_by(des_pair) %>%         #analysis set contains 90% of the training data
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
                                                  mutate(across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),    #missing values are replaced by zero for predictors in the regression
                                                         across(starts_with("sd"), ~replace_na(.,Inf)),                         #missing values for sds are replace by infinity
                                                         across(starts_with("m"), ~replace_na(.,1)),                            #missing values for share are replaced by 1
                                                         across(starts_with("n"), ~replace_na(.,0))) %>%
                                                  mutate(across(ends_with("share"), ~atan(.)*180/pi),                  #shares are transformed to degrees...
                                                         across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                                                  mutate(across(starts_with("m_"), round,0),                           #...and rounded to one digit
                                                         across(starts_with("sd"), round,0),
                                                         across(starts_with("rsd"), round,1))
    
    #fit the model
    simple_mob <- lmtree(formula = formula,
                  data=analysis_set,
                  na.action = na.pass,
                  alpha = alpha,
                  prune = "AIC",
                  minsplit = 20,
                  verbose = F)
    
    #generate assessment set
    #predictor variables used to group the dataset are not reestimated but taken from the analysis set
    merge <- analysis_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,
                                     m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                              unique()
    #where the target/predictor share is missing in the assessment set, an alternative share using the lagged target value is used instead
    assessment_set <- train_data_tdiff[folds==fold,] %>% group_by(des_pair) %>%
                    mutate(pl1_spot_share = case_when(is.na(lag(dep_var, n=1L)) & L1_dep_var!=0 & predictor!=0 ~ L1_dep_var/predictor,
                                                      is.na(lag(dep_var, n=1L)) & L1_dep_var==0 ~ NA),
                           pl2_spot_share = case_when(is.na(lag(dep_var, n=2L)) & L2_dep_var!=0 & predictor!=0 ~ L2_dep_var/predictor,
                                                      is.na(lag(dep_var, n=2L)) & L2_dep_var==0 ~ NA),
                           pl3_spot_share = case_when(is.na(lag(dep_var, n=3L)) & L3_dep_var!=0 & predictor!=0 ~ L3_dep_var/predictor,
                                                      is.na(lag(dep_var, n=3L)) & L3_dep_var==0 ~ NA),
                           pf1_spot_share = case_when(is.na(lead(dep_var, n=1L)) & F1_dep_var!=0 & predictor!=0 ~ F1_dep_var/predictor,
                                                      is.na(lead(dep_var, n=1L)) & F1_dep_var==0 ~ NA),
                           pf2_spot_share = case_when(is.na(lead(dep_var, n=2L)) & F2_dep_var!=0 & predictor!=0 ~ F2_dep_var/predictor,
                                                      is.na(lead(dep_var, n=2L)) & F2_dep_var==0 ~ NA),
                           pf3_spot_share = case_when(is.na(lead(dep_var, n=3L)) & F3_dep_var!=0 & predictor!=0 ~ F3_dep_var/predictor,
                                                      is.na(lead(dep_var, n=3L)) & F3_dep_var==0 ~ NA),
                           across(starts_with(c("pl","pf")), ~ mean(., na.rm=T)),
                           m_p_spot_share = case_when(!is.na(pl1_spot_share) ~ pl1_spot_share,  #more recent lags and leads are preferred over more distant ones
                                                      !is.na(pf1_spot_share) ~ pf1_spot_share,
                                                      !is.na(pl2_spot_share) ~ pl2_spot_share,
                                                      !is.na(pf2_spot_share) ~ pf2_spot_share,
                                                      !is.na(pl3_spot_share) ~ pl3_spot_share,
                                                      !is.na(pf3_spot_share) ~ pf3_spot_share)) %>%
                    ungroup() %>% 
                    merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
                    mutate(m_spot_share = case_when(is.na(m_spot_share) ~ atan(m_p_spot_share)*180/pi,  #replace missings with alternative share and transform it accordingly
                                                    .default = m_spot_share),
                           across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
                           across(starts_with("sd"), ~replace_na(.,90)),        #equivalent to replacing sds with Inf in the untransformed data
                           across(starts_with("m"), ~replace_na(.,45)),         #equivalent to replacing missing share with 1 in the untransformed data
                           across(starts_with("n"), ~replace_na(.,0)),        
                           m_predictor = ntile(m_predictor, n=20)) %>%
                           select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
                                  n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
                                  rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)
    
    
    #save predictions on assessment set
    if (i==1 & fold==1 & alpha==0.0005){
        prediction_train_tdiff <- assessment_set %>% 
                                  mutate(prediction = predict(simple_mob, newdata=assessment_set),
                                         fold = fold,
                                         alpha = alpha,
                                         run = i)
    } else {
        merge <- assessment_set %>% 
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
                          ungroup() %>% filter(RMSE==min(RMSE))      #identify the alpha that led to the lowest RMSE
besttune[i] <- list(best_tune)                                       #save best tune in the dedicated list

#only keep hold-out predictions for the best-tuned model
prediction_train_tdiff <- prediction_train_tdiff %>% filter(run!=i | (run==i & alpha==best_tune$alpha))

# train performance
train_performance <- prediction_train_tdiff %>% filter(run == i) %>% group_by(fold) %>%
                            summarize(MAE = round(mean(abs(dep_var-prediction), na.rm=T), digits=0),
                                      RMSE =sqrt(mean((dep_var-prediction)^2, na.rm=T)),
                                      pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2)) %>%
                            ungroup() %>% 
                            summarize(RMSE=mean(RMSE),                 #mean RMSE over all folds
                                      pRsquared=mean(pRsquared),       #mean R squared over all folds
                                      MAE=mean(MAE))                   #mean MAE over all folds

# quintile performance by the predictor variable
quin_perf_tdiff <- prediction_train_tdiff %>% filter(run == i) %>% 
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
                        mutate(n_predictor = sum(!is.na(predictor) & !is.na(dep_var)),  #predictor variables have to be reestimated because 
                               n_IIP = sum(!is.na(IIP_inward) & !is.na(dep_var)),       #data from the assessment sets is now also available  
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
                               across(starts_with("m"), ~replace_na(.,1)),
                               across(starts_with("n"), ~replace_na(.,0))) %>%
                        mutate(across(ends_with("share"), ~atan(.)*180/pi),
                               across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                        mutate(across(starts_with("m_"), round,0),
                               across(starts_with("sd"), round,0),
                               across(starts_with("rsd"), round,1))

#fit the model
train_mob <- lmtree(formula = formula, 
                  data=training_set,
                  na.action = na.pass,
                  alpha = best_tune$alpha,  #use the best tune to fit the model
                  prune = "AIC",
                  minsplit = 20,
                  verbose = F)
train_models[i] <- list(train_mob)

#add features to test set 
merge <- training_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,                #add the predictor variables derived from the training set
                                 m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                                 unique()
test_set <- test_data_tdiff %>% group_by(des_pair) %>%
  mutate(pl1_spot_share = case_when(is.na(lag(dep_var, n=1L)) & L1_dep_var!=0 & predictor!=0 ~ L1_dep_var/predictor,  #prepare alternative measure for missing shares
                                    is.na(lag(dep_var, n=1L)) & L1_dep_var==0 ~ NA),
         pl2_spot_share = case_when(is.na(lag(dep_var, n=2L)) & L2_dep_var!=0 & predictor!=0 ~ L3_dep_var/predictor,
                                    is.na(lag(dep_var, n=2L)) & L2_dep_var==0 ~ NA),
         pl3_spot_share = case_when(is.na(lag(dep_var, n=3L)) & L3_dep_var!=0 & predictor!=0 ~ L3_dep_var/predictor,
                                    is.na(lag(dep_var, n=3L)) & L3_dep_var==0 ~ NA),
         pf1_spot_share = case_when(is.na(lead(dep_var, n=1L)) & F1_dep_var!=0 & predictor!=0 ~ F1_dep_var/predictor,
                                    is.na(lead(dep_var, n=1L)) & F1_dep_var==0 ~ NA),
         pf2_spot_share = case_when(is.na(lead(dep_var, n=2L)) & F2_dep_var!=0 & predictor!=0 ~ F2_dep_var/predictor,
                                    is.na(lead(dep_var, n=2L)) & F2_dep_var==0 ~ NA),
         pf3_spot_share = case_when(is.na(lead(dep_var, n=3L)) & F3_dep_var!=0 & predictor!=0 ~ F3_dep_var/predictor,
                                    is.na(lead(dep_var, n=3L)) & F3_dep_var==0 ~ NA),
         across(starts_with(c("pl","pf")), ~ mean(., na.rm=T)),
         m_p_spot_share = case_when(!is.na(pl1_spot_share) ~ pl1_spot_share,
                                    !is.na(pf1_spot_share) ~ pf1_spot_share,
                                    !is.na(pl2_spot_share) ~ pl2_spot_share,
                                    !is.na(pf2_spot_share) ~ pf2_spot_share,
                                    !is.na(pl3_spot_share) ~ pl3_spot_share,
                                    !is.na(pf3_spot_share) ~ pf3_spot_share)) %>%
  ungroup() %>%
  merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
  mutate(m_spot_share = case_when(is.na(m_spot_share) ~ atan(m_p_spot_share)*180/pi,
                                  .default = m_spot_share),
         across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
         across(starts_with("sd"), ~replace_na(.,90)),
         across(starts_with("m"), ~replace_na(.,45)),
         across(starts_with("n"), ~replace_na(.,0)),
         across(starts_with("rsd_"), ~replace_na(.,Inf)),
         m_predictor = ntile(m_predictor, n=20)) %>%
  select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
         n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
         rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)

# predict test data and save predictions
if (i==1) {
 prediction_test_tdiff <- test_set %>%
                          mutate(prediction=predict(train_mob, newdata=test_set),
                                 run = i) 
} else {
  merge <- test_set %>% 
           mutate(prediction=predict(train_mob, newdata=test_set),
           run = i)
  prediction_test_tdiff <- rbind(prediction_test_tdiff, merge)
}

#calculate test performance
test_performance <- prediction_test_tdiff %>% filter(run==i) %>%
  summarize(test_RMSE =sqrt(mean((dep_var-prediction)^2, na.rm=T)),
            test_pRsquared = round(1-(sum((dep_var-prediction)^2, na.rm=T)/sum((dep_var)^2, na.rm=T)), digits=2),
            test_MAE = round(mean(abs(dep_var-prediction), na.rm=T), digits=0))

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
                             across(starts_with("m"), ~replace_na(.,1)),
                             across(starts_with("n"), ~replace_na(.,0))) %>%
                      mutate(across(ends_with("share"), ~atan(.)*180/pi),
                             across(starts_with("rsd"), ~replace_na(.,Inf))) %>%
                      mutate(across(starts_with("m_"), round,0),
                             across(starts_with("sd"), round,0),
                             across(starts_with("rsd"), round,1))
# train final model
final_mob <- lmtree(formula = formula, 
                 data=modelling_set,
                 na.action = na.pass,
                 alpha = best_tune$alpha,
                 prune = "AIC",
                 minsplit = 20,
                 verbose = F)

#add features to prediction set 
merge <- modelling_set %>% select(s_iso3c, r_iso3c, n_predictor, n_IIP, n_PI,  m_spot_share,m_IIP_share,
                                 m_PI_share, rsd_dep_var, sd_spot_share, sd_PI_share,sd_IIP_share,rsd_predictor) %>%
                              unique()
prediction_set <- prediction_data %>% group_by(des_pair) %>%
  mutate(pl1_spot_share = case_when(is.na(lag(dep_var, n=1L)) & L1_dep_var!=0 & predictor!=0 ~ L1_dep_var/predictor,
                                    is.na(lag(dep_var, n=1L)) & L1_dep_var==0 ~ NA),
         pl2_spot_share = case_when(is.na(lag(dep_var, n=2L)) & L2_dep_var!=0 & predictor!=0 ~ L3_dep_var/predictor,
                                    is.na(lag(dep_var, n=2L)) & L2_dep_var==0 ~ NA),
         pl3_spot_share = case_when(is.na(lag(dep_var, n=3L)) & L3_dep_var!=0 & predictor!=0 ~ L3_dep_var/predictor,
                                    is.na(lag(dep_var, n=3L)) & L3_dep_var==0 ~ NA),
         pf1_spot_share = case_when(is.na(lead(dep_var, n=1L)) & F1_dep_var!=0 & predictor!=0 ~ F1_dep_var/predictor,
                                    is.na(lead(dep_var, n=1L)) & F1_dep_var==0 ~ NA),
         pf2_spot_share = case_when(is.na(lead(dep_var, n=2L)) & F2_dep_var!=0 & predictor!=0 ~ F2_dep_var/predictor,
                                    is.na(lead(dep_var, n=2L)) & F2_dep_var==0 ~ NA),
         pf3_spot_share = case_when(is.na(lead(dep_var, n=3L)) & F3_dep_var!=0 & predictor!=0 ~ F3_dep_var/predictor,
                                    is.na(lead(dep_var, n=3L)) & F3_dep_var==0 ~ NA),
         across(starts_with(c("pl","pf")), ~ mean(., na.rm=T)),
         m_p_spot_share = case_when(!is.na(pl1_spot_share) ~ pl1_spot_share,
                                    !is.na(pf1_spot_share) ~ pf1_spot_share,
                                    !is.na(pl2_spot_share) ~ pl2_spot_share,
                                    !is.na(pf2_spot_share) ~ pf2_spot_share,
                                    !is.na(pl3_spot_share) ~ pl3_spot_share,
                                    !is.na(pf3_spot_share) ~ pf3_spot_share)) %>%
  ungroup() %>%
  merge(.,merge, by=c("s_iso3c" ,"r_iso3c"), all.x = T) %>%
  mutate(m_spot_share = case_when(is.na(m_spot_share) ~ atan(m_p_spot_share)*180/pi,
                                  .default = m_spot_share),
         across(c("predictor", "A_ti_T_T", "IIP_inward"), ~replace_na(.,0)),
         across(starts_with("sd"), ~replace_na(.,90)),
         across(starts_with("m"), ~replace_na(.,45)),
         across(starts_with("n"), ~replace_na(.,0)),
         across(starts_with("rsd_"), ~replace_na(.,Inf)),
         m_predictor = ntile(m_predictor, n=20)) %>%
         select(s_iso3c, r_iso3c, year, dep_var, predictor, IIP_inward, A_ti_T_T, mis_IIP, mis_PI,
         n_predictor, n_IIP, n_PI, m_predictor, m_spot_share, m_IIP_share, m_PI_share,
         rsd_dep_var, rsd_predictor, sd_spot_share, sd_PI_share, sd_IIP_share)
                            
# predict test data and save predictions
if (i==1) {
prediction_tdiff <- prediction_set %>% 
                                      mutate(prediction=predict(final_mob, newdata=prediction_set),
                                             target = target,
                                             run = i)
} else {
merge <- prediction_set %>% 
                      mutate(prediction=predict(final_mob, newdata=prediction_set),
                             target = target,
                             run = i)
prediction_tdiff <- rbind(prediction_tdiff, merge)
}