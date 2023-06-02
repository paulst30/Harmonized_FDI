#Setup 
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("haven")
#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("caret")
#install.packages("expss")
#install.packages("randomForest")
#install.packages("gbm")
#install.packages("randomForestSRC")
#install.packages("missForest")
#install.packages("foreign")
#install.packages("mlbench")
#install.packages("xtable")
#install.packages("xgboost")
#install.packages("Matrix")
#install.packages("cowplot")
#install.packages("FactoMineR")
#install.packages("car")
#install.packages("glmnet")
#install.packages("RANN")
#install.packages("diversityForest")
#install.packages("stringr")


library(tidyverse)
library(ggplot2)
library(haven)
library(foreign)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(expss)
library(randomForest)
library(gbm)
library(randomForestSRC)
library(missForest)
library(leaps)
library(mlbench)
library(xtable)
library(xgboost)
library(Matrix)
library(cowplot)
library(corrplot)
library(FactoMineR)
library(car)
library(glmnet)
library(RANN)
library(diversityForest)
library(stringr)

#read in Stata data
data <- read_dta('quality_analysis_ml_data.dta')

#impute


#build additional features 

#calculate weights of observations
data <- data %>% group_by(des_pair) %>% mutate(max_OUT=max(OUT_BMD4,na.rm=T)+1,
                                               OUT_BMD4=OUT_BMD4,
                                               re_IN_BMD4=re_IN_BMD4,
                                               new_diff=diff_inBMD4_outBMD4) %>% ungroup()



data <- data %>% mutate( s_out_fellow = case_when(!is.na(OUT_N_fellow/OUT_BMD4) ~ OUT_N_fellow/OUT_BMD4 ,OUT_N_fellow==0 & OUT_BMD4==0 ~ 0),
                         s_out_fellow2 = IMF_OUT_net_fellow/OUT_BMD4,
                         s_re_in_fellow = re_IMF_IN_net_fellow/re_IN_BMD4,
                         s_out_spe = OECD_OUT_SPE/OUT_BMD4,
                         s_re_in_spe = re_OECD_IN_SPE/re_IN_BMD4,
                         s_in_spe_IIP = SPE_IIP_inward/IIP_inward,
                         s_out_spe_IIP =SPE_IIP_outward/IIP_outward) %>%
                         group_by(des_pair) %>%
                         mutate(lag1 = lag(new_diff, n=1L),
                         lag2 = lag(new_diff, n=2L),
                         lead1 = lead(new_diff, n=1L),
                         lead2 = lead(new_diff, n=2L),
                         n_group = sum(!is.na(OUT_BMD4)),
                         n_zero = sum(OUT_BMD4==0, na.rm = T),
                         mean_group = mean(OUT_BMD4, na.rm = T),
                         sd_group = sd(OUT_BMD4, na.rm = T)) %>%
                         ungroup()

#additional numeric features


#####################select variables to work with##############################
set.seed(100)
working_data_wm <- data %>% select(-Type, -re_IMF_IN, -re_OECD_IN_BMD4) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(OUT_BMD4))   # Outward stocks data needs to be non-missing

modelling_data <- working_data_wm %>% filter(!is.na(OUT_BMD4) & !is.na(IN_BMD4))
prediction_data <- working_data_wm %>% filter(!is.na(OUT_BMD4) & is.na(IN_BMD4))


# check the missingness of data
p_data <- prediction_data %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair, -re_des_pair,
          -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
          -OECD_OUT_BMD4, IMF_OUT_net_fellow ) %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(p_data) <- c("Prediction")
p_data <- t(p_data)

#training data
t_data <- modelling_data %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair,-re_des_pair,
          -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
          -OECD_OUT_BMD4, IMF_OUT_net_fellow )  %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(t_data) <- c("Training")
t_data <- t(t_data)

#combining both
coverage <- cbind(t_data,p_data)
coverage_30 <- coverage[coverage[,2]>.3,]  # exclude variables that are missing more than 70 percent of the time

#split data into training and test sets
training_indices <- createDataPartition(modelling_data$diff_inBMD4_outBMD4,  #dependent variable
                                        groups = 10,                          #stratified over 10 quintiles
                                        p = 0.8,                              # 80 percent used for training data
                                        list = F)                             # output should be indicies not a list
total_train_data_tdiff <- modelling_data[training_indices,] #%>% filter(new_diff < 2 & new_diff >-2)
test_data_tdiff <- modelling_data[-training_indices,]

#split total training data into feature selection data and training data
feature_indices <- createDataPartition(total_train_data_tdiff$diff_inBMD4_outBMD4,
                                       p = 1/8,
                                       list = F)
feature_selection_data <- total_train_data_tdiff[feature_indices,]
train_data_tdiff <- total_train_data_tdiff[-feature_indices,]


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

# imputation
feature <- feature_selection_data %>% select(all_of(rownames(coverage_30)))
knn_impute <- preProcess(as.data.frame(feature),
                            method = c("knnImpute"),
                            k = 20,
                            knnSummary = mean)

feature <- predict(knn_impute, newdata = feature , na.action = na.pass)
feature_selection_data <- feature_selection_data %>% select(diff_inBMD4_outBMD4) %>%
                                                     cbind(.,feature)


# # fit control
# selection_fitControl <- trainControl(method = "none",    # method for resampling 
#                            savePredictions = F    # save prediction during fitting process
#                            )
# #glmnet
# selection_glmnet <- train(diff_inBMD4_outBMD4 ~ .,
#                           data = (feature_selection_data %>% select(all_of(rownames(coverage_30)), diff_inBMD4_outBMD4)),
#                           method = "glmnet", 
#                           tune_grid = expand.grid(alpha = 1),
#                           trControl = selection_fitControl,
#                           preProcess = c("knnImpute"),
#                           na.action = na.pass
#                           )
# var_sel_glmnet <- varImp(selection_glmnet)$importance %>% arrange(desc(Overall)) %>% head(4) %>% rownames()
# 
# selection2_glmnet <- train(diff_inBMD4_outBMD4 ~ .,
#                           data = feature_selection_data %>% select(starts_with("r_IMF"), 
#                                                                    starts_with("s_IMF"),
#                                                                    diff_inBMD4_outBMD4),
#                           method = "glmnet", 
#                           trControl = selection_fitControl,
#                           preProcess = c("knnImpute"),
#                           na.action = na.pass)
# varImp(selection2_glmnet)$importance %>% arrange(desc(Overall))


#interactionForest for detecting interaction effects

selection_intforest <- interactionfor(diff_inBMD4_outBMD4 ~., 
                                      data = (feature_selection_data %>% select(all_of(rownames(coverage_30)), diff_inBMD4_outBMD4)), 
                                      importance = "both",
                                      simplify.large.n = TRUE)  #adjusts tree depth and number of trees accroding to number of variables
str_replace <- c(" AND "= "*", " small" = "", " large"="")
quant_interactions <- as.data.frame(selection_intforest$eim.quant.sorted) %>% head(5) %>% 
                                    rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
qual_interactions <- as.data.frame(selection_intforest$eim.qual.sorted) %>% head(5) %>% 
                                   rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
uni_var <- as.data.frame(selection_intforest$eim.univ.sorted) %>% head(10) %>% 
                                   rownames() %>% str_replace_all(.,str_replace) %>% gsub("[ ].*","" ,.)
form <- c(uni_var, quant_interactions, qual_interactions) %>% unique() %>% 
                                           paste(., collapse = " + ") %>% 
                                           c("diff_inBMD4_outBMD4 ~ ", ., "-1") %>% 
                                           paste(., collapse= "") %>%
                                           as.formula()


###############Training data################

#########training sample for total difference#############
#exclude variables that should not be used for prediction
dep_tdiff <- train_data_tdiff %>% select(diff_inBMD4_outBMD4, new_diff, s_iso3c, r_iso3c, year, IN_BMD4, OUT_BMD4)

train_data_tdiff <- model.matrix.lm(form, data = train_data_tdiff, na.action="na.pass") %>% as.matrix()


#one-hot endcoding
#xgb_train_num <- train_data_tdiff %>% select(where(is.numeric))
#xgb_train_fac <- train_data_tdiff %>% select(where(is.factor))
#xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
#xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
#xgb_train_tdiff <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
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
dep_test_tdiff <- test_data_tdiff %>% select(diff_inBMD4_outBMD4, new_diff, s_iso3c, r_iso3c, year, IN_BMD4, OUT_BMD4)

test_data_tdiff <- model.matrix.lm(form, data = test_data_tdiff, na.action="na.pass") %>% as.matrix()


# #one-hot endcoding
# xgb_test_num <- test_data_tdiff %>% select(where(is.numeric))
# xgb_test_fac <- test_data_tdiff %>% select(where(is.factor))
# xgb_dummy <- dummyVars(" ~ .", data = xgb_test_fac)
# xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_test_fac))
# 
# #combine to xgb Matrix
# xgb_test_tdiff <- cbind(xgb_test_num, xgb_single_answers) %>% as.matrix()
xgb_test_tdiff_sp <- as(test_data_tdiff, "dgCMatrix")
