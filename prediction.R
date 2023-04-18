###################predict total difference#######################
predict_data_tdiff <- working_data_wm %>% 
                        filter(is.na(diff_inBMD4_outBMD4)) %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
                        select(-s_iso3c,-r_iso3c,-year,-IN_BMD4,-diff_inBMD4_outBMD4) 

prediction_ident_tdiff <- working_data_wm %>% 
                    filter(is.na(diff_inBMD4_outBMD4)) %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
                    select(s_iso3c,r_iso3c,year)

#one-hot endcoding
xgb_predict_num <- predict_data_tdiff %>% select(where(is.numeric))
xgb_predict_fac <- predict_data_tdiff %>% select(where(is.factor))
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_predict_fac))

#combine to xgb Matrix
xgb_predict_tdiff <- cbind(xgb_predict_num, xgb_single_answers) %>% as.matrix()


#prediction
prediction_tdiff <- predict(boost_tdiff_cv, xgb_predict_tdiff)
prediction_tdiff <- cbind(prediction_ident_tdiff, prediction_tdiff)


##############prediction fellow difference##############################
#update the working data for prediction by estimates of the total difference 
predict_data_fdiff <- working_data_wm %>% 
  filter(is.na(diff_fellow)) %>% 
  left_join(prediction_tdiff, by = c("s_iso3c", "r_iso3c", "year")) %>%
  mutate( diff_inBMD4_outBMD4 = case_when(
                                is.na(diff_inBMD4_outBMD4) ~ prediction_tdiff)) %>%
  select(-prediction_tdiff) %>%
  left_join(prediction_tdiff, by = c("s_iso3c"="r_iso3c","r_iso3c"="s_iso3c","year"="year")) %>%
  mutate( re_diff_inBMD4_outBMD4 = case_when(
                                is.na(re_diff_inBMD4_outBMD4) ~ prediction_tdiff)) %>%
  select(-prediction_tdiff) 


prediction_ident_fdiff <- predict_data_fdiff %>%                            #& OUT_BMD4!=0 & IN_BMD4!=0 & OUT_BMD4<10000)
  select(s_iso3c,r_iso3c,year)

predict_data_fdiff <- predict_data_fdiff %>% select(-s_iso3c,-r_iso3c,-year,-IN_BMD4,-diff_fellow) 

#one-hot endcoding
xgb_predict_num <- predict_data_fdiff %>% select(where(is.numeric))
xgb_predict_fac <- predict_data_fdiff %>% select(where(is.factor))
xgb_single_answers <- as.data.frame(predict(xgb_dummy, newdata = xgb_predict_fac))

#combine to xgb Matrix
xgb_predict_fdiff <- cbind(xgb_predict_num, xgb_single_answers) %>% as.matrix()


#prediction
prediction_fdiff <- predict(boost_fdiff_cv, xgb_predict_fdiff)
prediction_fdiff <- cbind(prediction_ident_fdiff, prediction_fdiff)
