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

#read in Stata data
data <- read_dta('quality_analysis_ml_data.dta')


#prediction data
p_data <- data %>% filter(!is.na(OUT_BMD4) & is.na(IN_BMD4)) %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair, -re_des_pair,
          -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
          -OECD_OUT_BMD4, IMF_OUT_net_fellow ) %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(p_data) <- c("Prediction")
p_data <- t(p_data)
#training data
t_data <- data %>% filter(!is.na(OUT_BMD4) & !is.na(IN_BMD4)) %>%
  select( -starts_with("IMF"), -IIA, -PTA, -DTT, -BIT, -exchange_rate, -ind_exchange_rate,
          -fin_center, -s_id, -r_id, -group_id, -s_answers, -r_answers, -s_iso3c, -r_iso3c, -des_pair,-re_des_pair,
          -ends_with("14"), -ends_with("17"), -ends_with("18"), -ends_with("19"), -ends_with("20"),
          -OECD_OUT_BMD4, IMF_OUT_net_fellow )  %>%
  summarise(across(everything(),~ mean(!is.na(.x))))
rownames(t_data) <- c("Training")
t_data <- t(t_data)

#combining both
coverage <- cbind(t_data,p_data)
coverage_30 <- coverage[coverage[,2]>.3,]


#####################select variables to work with##############################
set.seed(100)
working_data_wm <- data %>% select(s_iso3c, r_iso3c, year, diff_inBMD4_outBMD4, re_diff_inBMD4_outBMD4, 
                                   IN_BMD4,OUT_BMD4, rownames(coverage_30), re_OUT_BMD4, diff_fellow,
                                   -Type, -re_IMF_IN, -re_OECD_IN_BMD4) %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(OUT_BMD4)) %>%    # Outward stocks data needs to be non-missing
  group_by(s_iso3c, r_iso3c) %>%
  mutate( lag1_diff = lag(diff_inBMD4_outBMD4, n=1, default=NA,order_by = year),
          lag2_diff = lag(diff_inBMD4_outBMD4, n=2, default=NA,order_by = year),
          lag3_diff = lag(diff_inBMD4_outBMD4, n=3, default=NA,order_by = year),
          lag4_diff = lag(diff_inBMD4_outBMD4, n=4, default=NA,order_by = year),
          lead1_diff = lead(diff_inBMD4_outBMD4, n=1, default=NA, order_by = year),
          lead2_diff = lead(diff_inBMD4_outBMD4, n=2, default=NA, order_by = year),
          lead3_diff = lead(diff_inBMD4_outBMD4, n=3, default=NA, order_by = year),
          lead4_diff = lead(diff_inBMD4_outBMD4, n=4, default=NA, order_by = year),
          lag1_rediff = lag(re_diff_inBMD4_outBMD4, n=1, default=NA,order_by = year),
          lag2_rediff = lag(re_diff_inBMD4_outBMD4, n=2, default=NA,order_by = year),
          lag3_rediff = lag(re_diff_inBMD4_outBMD4, n=3, default=NA,order_by = year),
          lag4_rediff = lag(re_diff_inBMD4_outBMD4, n=4, default=NA,order_by = year),
          lead1_rediff = lead(re_diff_inBMD4_outBMD4, n=1, default=NA, order_by = year),
          lead2_rediff = lead(re_diff_inBMD4_outBMD4, n=2, default=NA, order_by = year),
          lead3_rediff = lead(re_diff_inBMD4_outBMD4, n=3, default=NA, order_by = year),
          lead4_rediff = lead(re_diff_inBMD4_outBMD4, n=4, default=NA, order_by = year)
  ) %>%
  ungroup()


###############Training data################

#########training sample for total difference#############
#exclude variables that should not be used for prediction
train_data_tdiff <- working_data_wm %>% 
  filter(!is.na(diff_inBMD4_outBMD4)) %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
  select(-s_iso3c,-r_iso3c,-year,-IN_BMD4,-diff_inBMD4_outBMD4)

dep_tdiff <- working_data_wm %>%  filter(!is.na(diff_inBMD4_outBMD4)) %>% select(diff_inBMD4_outBMD4, s_iso3c, r_iso3c, year)

#one-hot endcoding
xgb_train_num <- train_data_tdiff %>% select(where(is.numeric))
xgb_train_fac <- train_data_tdiff %>% select(where(is.factor))
xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
xgb_train_tdiff <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
xgb_train_tdiff_sp <- as(xgb_train_tdiff, "dgCMatrix")

#######training sample for difference in fellows###########

train_data_difffellow <- working_data_wm %>% 
  filter(!is.na(diff_fellow)) %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
  select(-s_iso3c,-r_iso3c,-year,-IN_BMD4,-diff_fellow)

dep_difffellow <- working_data_wm %>%  filter(!is.na(diff_fellow)) %>% select(diff_fellow, s_iso3c, r_iso3c, year)

#one-hot endcoding
xgb_train_num <- train_data_difffellow %>% select(where(is.numeric))
xgb_train_fac <- train_data_difffellow %>% select(where(is.factor))
xgb_dummy <- dummyVars(" ~ .", data = xgb_train_fac)
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_train_fac))

#combine to xgb Matrix
xgb_train_difffellow <- cbind(xgb_train_num, xgb_single_answers) %>% as.matrix()
xgb_train_difffellow_sp <- as(xgb_train_difffellow, "dgCMatrix")

