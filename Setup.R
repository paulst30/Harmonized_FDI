#Setup 
 
#define dependent var and main predictor
target <- predictor_matrix[i,"dep_var"]                             #save dependent variable in a separate value
predictor <- predictor_matrix[i,"predictor"]

# data$dep_var <- data[,target]                      #flexible dependent variable for all 6 mapping procedures 
# data$predictor <- c(data[,predictor])                 #flexible main predictor


#build additional features
data <-  mutate(data, dep_var=case_when(i<=3 ~ OECD_IN_BMD3,
                                        3<i & i<=5 ~ OECD_OUT_BMD3,
                                        i==6 ~ IN_BMD4),
                      predictor=case_when(i==1 ~ IN_BMD4,
                                          i==2 ~ OUT_BMD4,
                                          i==3 ~ OECD_OUT_BMD3,
                                          i==4 ~ IN_BMD4,
                                          i>=5 ~ OUT_BMD4),
                         spot_share = case_when(predictor!=0 & dep_var!=0 ~ dep_var/predictor,
                                                predictor==0 ~ 1),
                         IIP_share = case_when(IIP_inward != 0 ~ dep_var/IIP_inward,
                                               IIP_inward == 0 ~ 0),
                         PI_share = case_when(A_ti_T_T != 0 ~ dep_var/A_ti_T_T,
                                              A_ti_T_T == 0 ~ 0),
                         mis_predictor = is.na(predictor),
                         mis_IIP = is.na(IIP_inward),
                         mis_PI = is.na(A_ti_T_T),
                         across(starts_with("mis"), as.numeric)
                ) %>%
                  group_by(des_pair) %>%
                  mutate(m_predictor = mean(predictor, na.rm = T),
                         across(c("predictor", "dep_var"), 
                                list(L1 = ~lag(.x, n=1L), L2 = ~lag(.x, n=2L), L3 = ~lag(.x, n=3L),
                                     F1 = ~lead(.x, n=1L), F2 = ~ lead(.x, n=2L), F3 = ~lead(.x, n=3L)), 
                                .names="{.fn}_{.col}")) %>%
                         ungroup() 

                         # delta_predictor = predictor-lag(predictor,n = 1L, order_by = year),
                         # ag_predictor = mean(delta_predictor, na.rm =T),
                         # p_spot_share = case_when(lag(predictor, n=1L)!=0 & dep_var!=0 ~ dep_var/(lag(predictor, n =1L)),
                         #                          lag(predictor, n=1L)==0 ~ 1),
                         # p2_spot_share = case_when(lag(predictor, n=2L)!=0 & dep_var!=0 ~ dep_var/(lag(predictor, n =2L)),
                         #                           lag(predictor, n=2L)==0 ~ 1)) %>%
                         # ungroup() 

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
working_data_wm <- data %>%
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
prediction_data <- working_data_wm %>% filter(is.na(dep_var) & target_var==target & !is.na(predictor))  #!is.na(OUT_BMD4) &

# save length of prediction
prediction_tasks[i,1] <- target
prediction_tasks[i,2] <- predictor
prediction_tasks[i,3] <- nrow(modelling_data)
prediction_tasks[i,4] <- nrow(prediction_data)
prediction_tasks[i,5] <- length(intersect(unique(prediction_data$des_pair),
                                          unique(modelling_data$des_pair)))/length(unique(prediction_data$des_pair))

# check the missingness of data
p_data <- prediction_data %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_iso3c, -r_iso3c, -des_pair) %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(p_data) <- c("Prediction")
p_data <- t(p_data)

#training data
t_data <- modelling_data %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_iso3c, -r_iso3c, -des_pair)  %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(t_data) <- c("Training")
t_data <- t(t_data)

#combining both
coverage <- cbind(t_data,p_data)
coverage_30 <- coverage[coverage[,2]>.3,]  # exclude variables that are missing more than 70 percent of the time

#split data into training and test sets
training_indices <- createDataPartition(modelling_data$dep_var,               #dependent variable diff_inBMD4_outBMD4
                                        groups = 10,                          #stratified over 10 quintiles
                                        p = 0.8,                              # 80 percent used for training data
                                        list = F)                             # output should be indicies not a list
train_data_tdiff <- modelling_data[training_indices,] 
test_data_tdiff <- modelling_data[-training_indices,]


# form <- as.formula(dep_var ~ predictor + m_dep_var + const_share_pred + const_share_pred:predictor + sd_spot_share + 
#                      IIP_share_pred + sd_IIP_share + m_predictor + sd_predictor + n_group + delta_pred + 
#                      year + r_conduit + s_conduit + r_sink + s_conduit -1)


###############Training data################

# #########training sample for total difference#############
# #exclude variables that should not be used for prediction
# dep_tdiff <- train_data_tdiff %>% select(dep_var, predictor, s_iso3c, r_iso3c, des_pair, year, IN_BMD4, OUT_BMD4)
# 
# train_data_tdiff <- model.matrix.lm(form, data = train_data_tdiff, na.action="na.pass") %>% as.matrix()
# 
# xgb_train_tdiff_sp <- as(train_data_tdiff, "dgCMatrix")
# 
# 
# ################# test sample ################################
# dep_test_tdiff <- test_data_tdiff %>% select(dep_var, predictor, s_iso3c, r_iso3c, year, IN_BMD4, OUT_BMD4)
# 
# test_data_tdiff <- model.matrix.lm(form, data = test_data_tdiff, na.action="na.pass") %>% as.matrix()
# 
# xgb_test_tdiff_sp <- as(test_data_tdiff, "dgCMatrix")
