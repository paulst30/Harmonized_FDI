
#install.packages("leaps")
#install.packages("corrplot")
#install.packages("plm")

library(tidyverse)
library(ggplot2)
library(haven)
library(rpart)
library(rpart.plot)
library(lattice)
library(caret)
library(expss)
library(leaps)
library(corrplot)
library(plm)

###read in data###
data <- read_dta('C:/Users/SHK/Documents/Projekte/Data-Quality_Paper/Do files/quality_analysis_data.dta')
single_answers <- read_dta('C:/Users/SHK/Documents/Projekte/Data/Bilateral FDI/Metadata/CDIS survey/IMF_Metadata_single_dummies.dta')
# find linear dependencies
#single_answers <- single_answers %>% select(-s_iso3c,-r_iso3c,-des_pair)
#detect.lindep(single_answers)

#gen diff
#reverse_data <- data %>% 
#  select(IN_BMD4,OUT_BMD4,s_iso3c,r_iso3c,year) %>%
#  rename(re_IN_BMD4=IN_BMD4, re_OUT_BMD4=OUT_BMD4, r_iso3c=s_iso3c, s_iso3c=r_iso3c, year=year) %>%
#  mutate(re_diff_inBMD4_outBMD4=re_IN_BMD4-re_OUT_BMD4)

#data <- merge(data,reverse_data,by=c("s_iso3c","r_iso3c","year"), all.x=T) %>%
#  mutate(diff_inBMD4_outBMD4 = IN_BMD4-OUT_BMD4) %>%
#  group_by(des_pair) %>%
#  mutate(mean_IN_BMD4=mean(IN_BMD4), p_diff_inBMD4_outBMD4=sqrt((diff_inBMD4_outBMD4/mean_IN_BMD4*100)^2)) %>%
#  ungroup()

#subset for regression analysis

data_reg <- data %>%
  select(diff_inBMD4_outBMD4,re_diff_inBMD4_outBMD4,
         starts_with("s_IMF_10"), starts_with("s_IMF_11"), 
         #starts_with("s_IMF_12"),
         starts_with("s_IMF_13"),
         starts_with("s_IMF_14"), starts_with("s_IMF_15"), starts_with("s_IMF_16_1"), starts_with("s_IMF_16_2"), 
         starts_with("s_IMF_17"), starts_with("s_IMF_18"), 
         #starts_with("s_IMF_19"), 
         #starts_with("s_IMF_1_1"), 
         starts_with("s_IMF_1_2"), 
         #starts_with("s_IMF_1_3"), 
         starts_with("s_IMF_1_4"), 
         #starts_with("s_IMF_20"), 
         starts_with("s_IMF_2_1"), starts_with("s_IMF_2_2"), starts_with("s_IMF_3"), starts_with("s_IMF_4"), 
         #starts_with("s_IMF_5"), 
         starts_with("s_IMF_6"), 
         starts_with("s_IMF_7"), 
         #starts_with("s_IMF_8_1"), 
         #starts_with("s_IMF_8_2"), 
         #starts_with("s_IMF_8_3"), 
         #starts_with("s_IMF_8_4"), 
         starts_with("s_IMF_9_1"), 
         starts_with("s_IMF_9_2"), 
         starts_with("r_IMF_10"), 
         starts_with("r_IMF_11"), 
         #starts_with("r_IMF_12"), 
         starts_with("r_IMF_13"), starts_with("r_IMF_14"), starts_with("r_IMF_15"), starts_with("r_IMF_16_1"), 
         starts_with("r_IMF_16_2"), starts_with("r_IMF_17"), starts_with("r_IMF_18"),
         #starts_with("r_IMF_19"), 
         #starts_with("r_IMF_1_1"),
         #starts_with("r_IMF_1_2"), 
         #starts_with("r_IMF_1_3"),
         starts_with("r_IMF_1_4"), 
         starts_with("r_IMF_20"), starts_with("r_IMF_2_1"), starts_with("r_IMF_2_2"), starts_with("r_IMF_3"), 
         starts_with("r_IMF_4"), starts_with("r_IMF_5"), starts_with("r_IMF_6"), starts_with("r_IMF_7"), 
         #starts_with("r_IMF_8_1"), starts_with("r_IMF_8_2"), starts_with("r_IMF_8_3"), starts_with("r_IMF_8_4"), 
         starts_with("r_IMF_9_1"), starts_with("r_IMF_9_2")
                                                          ) %>%
  filter(!is.na(diff_inBMD4_outBMD4) & !is.na(re_diff_inBMD4_outBMD4)) %>%
  mutate_if(is.character,as.factor)

data_reg2 <- data %>%
  select(diff_inBMD4_outBMD4,re_diff_inBMD4_outBMD4, diff_val_change_BMD4,
         starts_with("s_IMF_10"), starts_with("s_IMF_11"), starts_with("s_IMF_12"), starts_with("s_IMF_13"),
         #starts_with("s_IMF_14"),starts_with("s_IMF_15"),
         starts_with("s_IMF_16_1"),
         #starts_with("s_IMF_16_2"), 
         #starts_with("s_IMF_17"), starts_with("s_IMF_18"), 
         #starts_with("s_IMF_19"), 
         #starts_with("s_IMF_1_1"), 
         starts_with("s_IMF_1_2"), 
         #starts_with("s_IMF_1_3"), 
         starts_with("s_IMF_1_4"), 
         starts_with("s_IMF_20"), 
         #starts_with("s_IMF_2_1"), starts_with("s_IMF_2_2"), starts_with("s_IMF_3"), starts_with("s_IMF_4"), 
         #starts_with("s_IMF_5"), 
         #starts_with("s_IMF_6"), 
         #starts_with("s_IMF_7"), 
         starts_with("s_IMF_8_1"), 
         #starts_with("s_IMF_8_2"), 
         #starts_with("s_IMF_8_3"), 
         starts_with("s_IMF_8_4"), 
         #starts_with("s_IMF_9_1"), 
         #starts_with("s_IMF_9_2"), 
         starts_with("r_IMF_10"), 
         starts_with("r_IMF_11"), 
         starts_with("r_IMF_12"), 
         starts_with("r_IMF_13"), 
         #starts_with("r_IMF_14"), starts_with("r_IMF_15"), 
         starts_with("r_IMF_16_1"), 
         #starts_with("r_IMF_16_2"), starts_with("r_IMF_17"), starts_with("r_IMF_18"),
         #starts_with("r_IMF_19"), 
         #starts_with("r_IMF_1_1"),
         starts_with("r_IMF_1_2"), 
         #starts_with("r_IMF_1_3"),
          starts_with("r_IMF_1_4"), 
         starts_with("r_IMF_20"),
         #starts_with("r_IMF_2_1"), starts_with("r_IMF_2_2"), starts_with("r_IMF_3"), 
         #starts_with("r_IMF_4"), starts_with("r_IMF_5"), starts_with("r_IMF_6"), starts_with("r_IMF_7"), 
         starts_with("r_IMF_8_1"), 
         #starts_with("r_IMF_8_2"), starts_with("r_IMF_8_3"), 
         starts_with("r_IMF_8_4"), 
         #starts_with("r_IMF_9_1"), starts_with("r_IMF_9_2")
  ) %>%
  filter(!is.na(diff_inBMD4_outBMD4) & !is.na(re_diff_inBMD4_outBMD4) & !is.na(diff_val_change_BMD4) & !is.na(diff_SPE)) %>%
  mutate_if(is.character,as.factor)

#subetting
#sample <- sample(rep(c(1,2,3,4),nrow(data_reg2)/4))
#part1 <- data_reg2[sample == 1,] 
#part2 <- data_reg2[sample == 2,]
#part3 <- data_reg2[sample == 3,] 
#part4 <- data_reg2[sample == 4,] 

#setting up model selection algorithm
gc()
forward <- regsubsets(diff_inBMD4_outBMD4 ~ ., data_reg,nvm=70, method = "forward")
#default <- regsubsets(diff_inBMD4_outBMD4 ~ ., data_reg,nvm=70, really.big=T)

results <- summary(forward)
coef(forward,12) #shows coefficients of respective model
which.max(summary(forward)$adjr2) #shows how many variables maximize adjusted r squared
which.min(summary(forward)$bic)

#creating graphs for r2, AIC, BIC
tibble(predictors = 1:70,
       adj_R2 = results$adjr2,
       #Cp = results$cp,
       #BIC = results$bic
       ) %>%
  gather(statistic, value, -predictors) %>%
  ggplot(aes(predictors, value, color = statistic)) +
  geom_line(show.legend = F) +
  geom_point(show.legend = F)+
  facet_wrap(~ statistic, scales = "free")



