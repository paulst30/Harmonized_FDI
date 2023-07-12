


#read in Stata data
data <- read_dta('quality_analysis_ml_data.dta')


# generate combination matrix for different algorithms
predictor_matrix <- data.frame(dep_var= c("OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_OUT_BMD3", "OECD_OUT_BMD3", "IN_BMD4"),
                      predictor = c("IN_BMD4", "OUT_BMD4", "OECD_OUT_BMD3", "IN_BMD4", "OUT_BMD4", "OUT_BMD4"))

 
#define dependent var and main predictor
target <- predictor_matrix[i,"dep_var"]                             #save dependent variable in a separate value
predictor <- predictor_matrix[i,"predictor"]

data <- data %>% mutate(inclusion= case_when(!is.na(OECD_IN_BMD3) ~ "IN_BMD3",
                                             is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ "OUT_BMD3",
                                             is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ "IN_BMD4",
                                             is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & is.na(IN_BMD4) & !is.na(OUT_BMD4) ~ "OUT_BMD4",
                                             .default = "")) %>%
                 rename(dep_var=!!target,                           #flexible dependent variable for all 6 mapping procedures 
                        predictor=!!predictor) %>%                  #flexible main predictor
                        group_by(des_pair) %>%
                        mutate(target_var = case_when(as.logical(max(inclusion=="IN_BMD3", na.rm = T)) ~ "OECD_IN_BMD3",
                                                      as.logical(max(inclusion=="IN_BMD3", na.rm=T)==0 & max(inclusion=="OUT_BMD3" , na.rm = T)) ~ "OECD_OUT_BMD3",
                                                      as.logical(max(inclusion=="IN_BMD3", na.rm=T)==0 & max(inclusion=="OUT_BMD3", na.rm=T)==0 & max(inclusion=="IN_BMD4" , na.rm = T)) ~ "IN_BMD4",
                                                      as.logical(max(inclusion=="IN_BMD3", na.rm=T)==0 & 
                                                                 max(inclusion=="OUT_BMD3", na.rm=T)==0 & 
                                                                 max(inclusion=="IN_BMD4" , na.rm = T)==0 &
                                                                 max(inclusion=="OUT_BMD4" , na.rm = T)) ~ "OUT_BMD4"))


#build additional features that are the same for test and training sample
data <- data  %>% mutate(spot_share = case_when(predictor!=0 & dep_var!=0 ~ dep_var/predictor,
                                                predictor==0 ~ 1,
                                                .default = NA),
                         IIP_share = case_when(IIP_inward != 0 ~ dep_var/IIP_inward,
                                               IIP_inward == 0 ~ 0),
                         PI_share = case_when(A_ti_T_T != 0 ~ dep_var/A_ti_T_T,
                                              A_ti_T_T == 0 ~ 0)) %>%
                  group_by(des_pair) %>%
                  mutate(lag1 = lag(dep_var, n=1L),
                         lag2 = lag(dep_var, n=2L),
                         delta = lead(dep_var, n=1L)-dep_var,
                         delta_pred = lead(dep_var,n=1L)-lead(delta, n=1L),
                         lead1 = lead(dep_var, n=1L),
                         lead2 = lead(dep_var, n=2L),
                         lag1_share = lag(spot_share, n=1L)*predictor,
                         lag2_share = lag(spot_share, n=2L)*predictor,
                         lead1_share = lead(spot_share, n=1L)*predictor,
                         lead2_share = lead(spot_share, n=2L)*predictor,
                         lead3_share = lead(spot_share, n=3L)*predictor,
                         lead4_share = lead(spot_share, n=4L)*predictor,
                         n_group = sum(!is.na(predictor)),
                         n_zero = sum(predictor==0, na.rm = T),
                         m_predictor = mean(predictor, na.rm = T),
                         sd_predictor = sd(predictor, na.rm = T)) %>%
                         ungroup() 


# detect "outliers", i.e. pairs in which inward stocks are likely not better than outward stocks. 

# outlier_ind <- data %>% group_by(des_pair) %>% mutate(sd_inward= sd(IN_BMD4, na.rm = T),
#                                        sd_outward= sd(OUT_BMD4, na.rm = T),
#                                        n = sum(!is.na(diff_inBMD4_outBMD4)),
#                                        mean_diff = mean(diff_inBMD4_outBMD4/OUT_BMD4, na.rm = T),
#                                        sd_diff = sd(diff_inBMD4_outBMD4/OUT_BMD4, na.rm = T),
#                                        mean_diff_missings = mean(diff_inBMD4_outBMD4, na.rm = T),
#                                        sd_diff_missings = sd(diff_inBMD4_outBMD4, na.rm = T)) %>%
#                                        ungroup() %>%
#                                        mutate(sd_diff = abs((diff_inBMD4_outBMD4/OUT_BMD4-mean_diff)/sd_diff),
#                                               sd_diff = if_else(is.na(sd_diff),abs((diff_inBMD4_outBMD4-mean_diff_missings)/sd_diff_missings),0,missing = F),
#                                               sd_diff = if_else(is.na(sd_diff) & !is.na(diff_inBMD4_outBMD4), 0, sd_diff, missing = F),
#                                        ind = if_else(!is.na(diff_inBMD4_outBMD4) & is.na(sd_diff) & diff_inBMD4_outBMD4!=0, 1,0, missing = F)) %>%
#                                        ungroup() %>%
#                                        transmute(outlier = if_else((sd_inward==0 & sd_outward!=0) | sd_diff >=2, 1,0, missing = F))
# outlier_ind <- which(outlier_ind$outlier == 1)
# data[outlier_ind, "IN_BMD4"] <- NA




#####################select variables to work with##############################
set.seed(100)
working_data_wm <- data %>% select(-Type, -re_IMF_IN, -re_OECD_IN_BMD4) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(predictor))                             # main predictor variable cannot be missing

#transform numeric data
# factor_data_wm <- working_data_wm %>% select(where(is.factor))
# numeric_data_wm <- working_data_wm %>% select(where(is.numeric)) %>% apply(.,2,function(x) asinh(x)) %>% as.data.frame()
# working_data_wm <- cbind(factor_data_wm, numeric_data_wm)

# train-to-the-test: propensity score matching
# prop_data <- working_data_wm %>% mutate( prediction = !is.na(OUT_BMD4) & is.na(IN_BMD4)) %>%
#                                  group_by(des_pair) %>%
#                                  summarize(prediction= as.factor(max(prediction, na.rm = T)),
#                                            mOUT_BMD4 = mean(OUT_BMD4, na.rm = T ),
#                                            s_GDPcurr = mean(s_GDPcurr, na.rm = T),
#                                            r_GDPcurr = mean(r_GDPcurr, na.rm = T)) %>%
#                                 ungroup() %>% 
#                                 mutate(across(where(is.numeric), ~ as.numeric(scale(.))))
# 
# prop_data$r_GDPcurr[is.na(prop_data$r_GDPcurr)] <- min(prop_data$r_GDPcurr)
# 
# 
# # calculate proximity and pick N nearset neighbors of each prediction observation
# dist <- as.matrix(dist(prop_data[3:5], method = "euclidean"))
# 
# diag(dist) = NA
# 
# dist <- dist[,prop_data$prediction==1] %>%
#         .[prop_data$prediction==0,] %>%
#         apply(.,2,function(x) which.minn(x, n = 10)) %>%     # n nearest neighbors in the training data
#         as.numeric() %>%
#         unique()
#         
# training_pairs <- as.matrix(prop_data[dist,"des_pair"])  # list of nearest neigbors in the training data
                                 
modelling_data <- working_data_wm %>% filter( !is.na(dep_var)) #[working_data_wm$des_pair %in% training_pairs,]!is.na(OUT_BMD4) &

prediction_indep_vars <- modelling_data %>% group_by(des_pair) %>%
                         summarize(m_dep_var = mean(dep_var, na.rm=T),
                                  sd_dep_var =sd(dep_var, na.rm=T),
                                  const_share_pred = mean(spot_share, na.rm=T),
                                  sd_spot_share =sd(spot_share, na.rm=T),
                                  IIP_share_pred = mean(IIP_share, na.rm=T),
                                  sd_IIP_share = sd(IIP_share, na.rm=T),
                                  PI_share_pred = mean(PI_share, na.rm=T),
                                  sd_PI_share = sd(PI_share, na.rm=T))
prediction_data <- working_data_wm %>% filter(is.na(dep_var) & target_var==target) %>%
                   merge(.,prediction_indep_vars, by=c("des_pair"),all.x=T) 




# # check the missingness of data
# p_data <- prediction_data %>%
#   select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
#           -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair, -re_des_pair,
#           -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
#           -OECD_OUT_BMD4, IMF_OUT_net_fellow ) %>%
#   summarise(across(everything(),~ mean(!is.na(.x))))
# rownames(p_data) <- c("Prediction")
# p_data <- t(p_data)
# 
# #training data
# t_data <- modelling_data %>%
#   select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
#           -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair,-re_des_pair,
#           -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
#           -OECD_OUT_BMD4, IMF_OUT_net_fellow )  %>%
#   summarise(across(everything(),~ mean(!is.na(.x))))
# rownames(t_data) <- c("Training")
# t_data <- t(t_data)
# 
# #combining both
# coverage <- cbind(t_data,p_data)
# coverage_30 <- coverage[coverage[,2]>.3,]  # exclude variables that are missing more than 70 percent of the time
# 
# coverage_all[i] <- coverage_30

#split data into training and test sets
training_indices <- createDataPartition(modelling_data$dep_var,               #dependent variable diff_inBMD4_outBMD4
                                        groups = 10,                          #stratified over 10 quintiles
                                        p = 0.8,                              # 80 percent used for training data
                                        list = F)                             # output should be indicies not a list
train_data_tdiff <- modelling_data[training_indices,]  %>% #replace with "total_train_data_tdiff" for feature selection
                    group_by(des_pair) %>% 
                    mutate(m_dep_var = case_when(sum(!is.na(dep_var)) > 1 & !is.na(dep_var) ~ (sum(dep_var, na.rm = T)-dep_var)/(n()-1),
                                                 sum(!is.na(dep_var)) > 1 & is.na(dep_var) ~ mean(dep_var, na.rm = T),
                                                 sum(!is.na(dep_var)) <= 1 ~ NA),
                          sd_dep_var = sd(dep_var, na.rm=T),
                          n_spot_share = sum(!is.na(spot_share)),
                          const_share_pred = case_when(sum(!is.na(spot_share)) > 1 & !is.na(spot_share) ~ (sum(spot_share, na.rm = T)-spot_share)/(n_spot_share-1),
                                                       sum(!is.na(spot_share)) >= 1 & is.na(spot_share) ~ mean(spot_share, na.rm = T),
                                                       sum(!is.na(spot_share)) < 1 ~ NA),
                         sd_spot_share =sd(spot_share, na.rm=T),
                         n_IIP_share = sum(!is.na(IIP_share)),
                         IIP_share_pred = case_when(sum(!is.na(IIP_share)) > 1 & !is.na(IIP_share)  ~ (sum(IIP_share, na.rm = T)-IIP_share)/(n_IIP_share-1),
                                                    sum(!is.na(IIP_share)) >= 1 & is.na(IIP_share) ~ mean(IIP_share, na.rm = T),
                                                    sum(!is.na(IIP_share)) < 1 | is.infinite(IIP_share) ~ NA),
                         sd_IIP_share = sd(IIP_share, na.rm=T),
                         n_PI_share = sum(!is.na(A_ti_T_T)),
                         PI_share_pred = case_when(sum(!is.na(PI_share)) > 1 & !is.na(PI_share)  ~ (sum(PI_share, na.rm = T)-PI_share)/(n_PI_share-1),
                                                       sum(!is.na(PI_share)) >= 1 & is.na(PI_share) ~ mean(PI_share, na.rm = T),
                                                       sum(!is.na(PI_share)) < 1 | is.infinite(PI_share) ~ NA),
                         sd_PI_share = sd(PI_share, na.rm=T)
                         ) %>%
                         ungroup() 

test_indep_vars <- train_data_tdiff %>% group_by(des_pair) %>%
                   summarize(m_dep_var = mean(dep_var, na.rm=T),
                             sd_dep_var =sd(dep_var, na.rm=T),
                             const_share_pred = mean(spot_share),
                             sd_spot_share =sd(spot_share, na.rm=T),
                             IIP_share_pred = mean(IIP_share, na.rm=T),
                             sd_IIP_share = sd(IIP_share, na.rm=T),
                             PI_share_pred = mean(PI_share, na.rm=T),
                             sd_PI_share = sd(PI_share, na.rm=T))
  
test_data_tdiff <- modelling_data[-training_indices,] %>%
                   merge(.,test_indep_vars, by=c("des_pair"), all.x=T)

# #split total training data into feature selection data and training data
# feature_indices <- createDataPartition(total_train_data_tdiff$dep_var,        #dependent var
#                                        p = 1/8,
#                                        list = F)
# feature_selection_data <- total_train_data_tdiff[feature_indices,]
# train_data_tdiff <- total_train_data_tdiff[-feature_indices,]

#record training sample and prediction sample
prediction_tasks[i,1] <- target
prediction_tasks[i,2] <- predictor
prediction_tasks[i,3] <- nrow(modelling_data)
prediction_tasks[i,4] <- nrow(prediction_data)
prediction_tasks[i,5] <- NA #coverage["const_share_pred",2]

# #Select features
# 
# #numeric features with a correlation of at least 0.5
# numeric_wd <- feature_selection_data %>% select(rownames(coverage_30), IN_BMD4, diff_inBMD4_outBMD4, new_diff) %>%
#                                 select(where(is.numeric)) %>%
#                                 cor(., use = "pairwise.complete.obs", method = "spearman")
# correlation_2 <- rownames(numeric_wd[numeric_wd[,"new_diff"]>.5 |  numeric_wd[,"new_diff"] < -.5,])
# correlation_2 <- correlation_2[correlation_2 != "new_diff" & correlation_2 != "IN_BMD4"  ]
# 
# 
# #factor variables
# 
# #convert data to pair level
# factor_vars <- feature_selection_data %>% select(starts_with("s_IMF"), starts_with("r_IMF"), des_pair) %>%
#                    unique()
# dep_factors_vars <- feature_selection_data %>% select(new_diff, des_pair) %>%
#                     group_by(des_pair) %>%
#                     summarize(new_diff = mean(new_diff, na.rm = T)) %>%
#                     unique()
# #convert the dependent variable into degrees deviation from the expected 45 degree line.
# factor_vars <- cbind(factor_vars, dep_factors_vars[, "new_diff"])
# 
# 
# #perform a kruskal test to assess whether at least one of the factor levels is assciated with different OUT-IN ratios
# kruskal_results <-  lapply(factor_vars[,1:58], function(nm)
#   kruskal.test(new_diff ~ nm, data = factor_vars)  #extracts p-value from levene-tests
# )
# kruskal <- data.frame(matrix(unlist(kruskal_results), nrow=length(kruskal_results), byrow=TRUE))
# rownames(kruskal) <- colnames(factor_vars[,1:58])
# names(kruskal)[names(kruskal) == "X3"] <- "pvalue"
# kruskal$pvalue <- as.numeric(kruskal$pvalue)
# kruskal <- kruskal %>% filter(pvalue<0.001)

# # vector of selected features
# selected_features <- c( coverage_30 ,rownames(kruskal), "lag1", "lag2", "OUT_BMD4") #correlation_2
# 
# # Algorithm based selection

# # imputation
# feature <- feature_selection_data %>% select(all_of(rownames(coverage_30)))
# knn_impute <- preProcess(as.data.frame(feature),
#                             method = c("knnImpute"),
#                             k = 5,
#                             knnSummary = mean)
# 
# feature <- predict(knn_impute, newdata = feature , na.action = na.pass)
# feature_selection_data <- feature_selection_data %>% select(dep_var) %>%         # dep_var
#                                                      cbind(.,feature)
# 
# 
# #interactionForest for detecting interaction effects
# 
# selection_intforest <- interactionfor(dep_var ~.,                    # dependent var
#                                       data = feature_selection_data, 
#                                       importance = "both",
#                                       simplify.large.n = TRUE)  #adjusts tree depth and number of trees accroding to number of variables
# str_replace <- c(" AND "= "*", " small" = "", " large"="")
# quant_interactions <- as.data.frame(selection_intforest$eim.quant.sorted) %>% head(5) %>% 
#                                     rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
# qual_interactions <- as.data.frame(selection_intforest$eim.qual.sorted) %>% head(5) %>% 
#                                    rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
# uni_var <- as.data.frame(selection_intforest$eim.univ.sorted) %>% head(10) %>% 
#                                    rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
# form <- c(uni_var, quant_interactions, qual_interactions) %>% unique() %>% 
#                                            paste(., collapse = " + ") %>% 
#                                            c("dep_var ~ ", ., "-1") %>%          #dependent var
#                                            paste(., collapse= "") %>%
#                                            as.formula()

form <- as.formula(dep_var ~ m_dep_var + sd_dep_var +
                             const_share_pred  + sd_spot_share + predictor +
                             IIP_share_pred + sd_IIP_share + IIP_inward +
                             m_predictor + sd_predictor + n_group + delta_pred + 
                             PI_share_pred + sd_PI_share + A_ti_T_T +
                             year + r_conduit + s_conduit + r_sink + s_conduit -1)


###############Training data################

#########training sample for total difference#############
#exclude variables that should not be used for prediction
dep_tdiff <- train_data_tdiff %>% select(dep_var, predictor, s_iso3c, r_iso3c, des_pair, year, const_share_pred)

train_data_tdiff <- model.matrix.lm(form, data = train_data_tdiff, na.action="na.pass") %>% as.matrix()

xgb_train_tdiff_sp <- as(train_data_tdiff, "dgCMatrix")

#######training sample for difference in fellows###########

#train_data_difffellow <- working_data_wm %>% 
#  filter(!is.na(diff_fellow)) %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
#  select(-s_iso3c,-r_iso3c,-year,-IN_BMD4,-diff_fellow)

#dep_difffellow <- working_data_wm %>%  filter(!is.na(diff_fellow)) %>% select(diff_fellow, s_iso3c, r_iso3c, year) #, max_OUT_BMD4)

#one-hot endcoding
#xgb_train_num <- train_data_difffellow %>% select(where(is.numeric))
#xgb_train_fac <- train_data_difffellow %>% select(where(is.factor))
#xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
#xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
#xgb_train_difffellow <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
#xgb_train_difffellow_sp <- as(xgb_train_difffellow, "dgCMatrix")

################# test sample ################################
dep_test_tdiff <- test_data_tdiff %>% select(dep_var, predictor, s_iso3c, r_iso3c, year, const_share_pred)

test_data_tdiff <- model.matrix.lm(form, data = test_data_tdiff, na.action="na.pass") %>% as.matrix()

xgb_test_tdiff_sp <- as(test_data_tdiff, "dgCMatrix")

################ prediction data ############################
dep_pred_tdiff <- prediction_data %>% select(dep_var, predictor, s_iso3c, r_iso3c, des_pair, year, const_share_pred)

prediction_data <- model.matrix.lm(form, data = prediction_data, na.action="na.pass") %>% as.matrix()

xgb_pred_tdiff_sp <- as(prediction_data, "dgCMatrix")
