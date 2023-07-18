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
#install.packages("MatchIt")
#install.packages("doBy")
#install.packages("neuralnet")



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
library(MatchIt)
library(doBy)
library(neuralnet)
library(party)


#read in Stata data
data <- read_dta('quality_analysis_ml_data.dta') 
data <- as.data.frame(do.call(cbind, data))
data[,-c(1,2,8)] <- data[,-c(1,2,8)] %>% mutate(across(everything(), as.numeric))

# generate combination matrix for different algorithms
predictor_matrix <- data.frame(dep_var= c("IN_BMD4", "IN_BMD4", "IN_BMD4", "OECD_IN_BMD3", "OECD_IN_BMD3", "OUT_BMD4"),
                      predictor = c("OUT_BMD4", "OECD_OUT_BMD3", "OECD_IN_BMD3", "OUT_BMD4", "OECD_OUT_BMD3", "OECD_OUT_BMD3"))

 
#define dependent var and main predictor
target <- predictor_matrix[1,"dep_var"]                             #save dependent variable in a separate value
predictor <- predictor_matrix[1,"predictor"]

data$dep_var <- data[,target]                      #flexible dependent variable for all 6 mapping procedures 
data$predictor <- c(data[,predictor])                 #flexible main predictor

data <- data %>% mutate(inclusion= case_when(!is.na(IN_BMD4) ~ "IN_BMD4",
                                             is.na(IN_BMD4) & !is.na(OECD_IN_BMD3) ~ "IN_BMD3",
                                             is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & !is.na(OUT_BMD4) ~ "OUT_BMD4",
                                             is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OUT_BMD4) & !is.na(OECD_OUT_BMD3) ~ "OUT_BMD3",
                                             .default = "")) %>%
                        group_by(des_pair) %>%
                        mutate(target_var = case_when(as.logical(max(inclusion=="IN_BMD4", na.rm = T)) ~ "IN_BMD4",
                                                      as.logical(max(inclusion!="IN_BMD4" & inclusion=="IN_BMD3" , na.rm = T)) ~ "IN_BMD3",
                                                      as.logical(max(inclusion!="IN_BMD4" & inclusion!="IN_BMD3" & inclusion=="OUT_BMD4" , na.rm = T)) ~ "OUT_BMD4"))


#build additional features
data <-  mutate(data, spot_share = case_when(predictor!=0 & dep_var!=0 ~ dep_var/predictor,
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
                  mutate(m_predictor = mean(predictor, na.rm = T)) %>%
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
