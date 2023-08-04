#Setup 
 
#define dependent var and main predictor
target <- predictor_matrix[i,"dep_var"]                             #save dependent variable in a separate value
predictor <- predictor_matrix[i,"predictor"]

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


#### define train, test, and prediction data #####
set.seed(100)
working_data_wm <- data %>%
  mutate_if(is.character, as.factor) %>%
  filter(!is.na(predictor))                             # main predictor variable cannot be missing
                                 
modelling_data <- working_data_wm %>% filter(!is.na(dep_var)) #training and test set cannot have missing target vintages
prediction_data <- working_data_wm %>% filter(is.na(dep_var) & target_var==target & !is.na(predictor))  #prediction set

# save length of prediction
prediction_tasks[i,1] <- target
prediction_tasks[i,2] <- predictor
prediction_tasks[i,3] <- nrow(modelling_data)
prediction_tasks[i,4] <- nrow(prediction_data)
prediction_tasks[i,5] <- length(intersect(unique(prediction_data$des_pair),
                                          unique(modelling_data$des_pair)))/length(unique(prediction_data$des_pair))

#split data into training and test sets
training_indices <- createDataPartition(modelling_data$dep_var,               #dependent variable
                                        groups = 10,                          #stratified over deciles
                                        p = 0.8,                              # 80 percent used for training data
                                        list = F)                             # output should be indicies not a list
train_data_tdiff <- modelling_data[training_indices,] 
test_data_tdiff <- modelling_data[-training_indices,]
