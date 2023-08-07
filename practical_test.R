#### combine data with predicted observations #####
data <- read_dta('quality_analysis_ml_data.dta')

#check for duplicates (some observations could be predicted by multiple predictor vintages)
prediction_tdiff %>% group_by(s_iso3c, r_iso3c, year) %>%
                     mutate(new=n()>1) %>% filter(new==1) %>% arrange(s_iso3c,r_iso3c,year)
prediction_obs <- prediction_tdiff %>% distinct(s_iso3c, r_iso3c, year , .keep_all = TRUE) #delete duplicates


#isolate US deflator
deflator <- data %>% filter(r_iso3c=="USA") %>% select(year, r_GDPdef) %>% group_by(year) %>% 
                     summarize(deflator_USD=mean(r_GDPdef))

### generate harmonized dataset ###
harmonized_data <- data %>% merge(.,prediction_obs, by = c("s_iso3c", "r_iso3c","year"), all.x=T, all.y = T) %>% 
                            mutate(naive_FDI = case_when(!is.na(OECD_IN_BMD3) ~ OECD_IN_BMD3,
                                                     is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ OECD_OUT_BMD3,
                                                     is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ IN_BMD4,
                                                     is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(OUT_BMD4) ~ OUT_BMD4),
                                   naive_vintage =  case_when(!is.na(OECD_IN_BMD3) ~ "OECD_IN_BMD3",
                                                               is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ "OECD_OUT_BMD3",
                                                               is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ "IN_BMD4",
                                                               is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(OUT_BMD4) ~ "OUT_BMD4"),
                                   harmonized_FDI = case_when(!is.na(naive_FDI) & is.na(prediction) ~ naive_FDI,
                                                              !is.na(naive_FDI) & !is.na(prediction) ~ prediction),
                                   harmonized_vintage = case_when(!is.na(naive_FDI) & is.na(prediction) ~ naive_vintage,
                                                                  !is.na(naive_FDI) & !is.na(prediction) ~ target),
                                   adjusted = case_when(!is.na(prediction) ~ 1,
                                                        is.na(prediction) ~ 0)) %>%
                                   select(s_iso3c, r_iso3c, year, 
                                          IN_BMD4, OUT_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3, 
                                          naive_FDI, naive_vintage, harmonized_FDI, harmonized_vintage, adjusted) %>%
                                   filter(!is.na(IN_BMD4)| !is.na(OUT_BMD4) | !is.na(OECD_IN_BMD3) | !is.na(OECD_OUT_BMD3))
#save harmonized FDI data set
write.csv(harmonized_data, paste(getwd(),"harmonized data.csv",sep="/"), row.names=FALSE)

### build data set for practical tests
practical_test_data <- data %>% merge(.,prediction_obs, by = c("s_iso3c", "r_iso3c","year"), all.x=T, all.y = T) %>% 
                                merge(., deflator, by= "year", all.x=T) %>%
                                select(s_iso3c, r_iso3c, year, des_pair,
                                       IN_BMD4, OUT_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3,
                                       prediction, target, fin_center, deflator_USD,
                                       IIA, PTA, DTT, BIT, exchange_rate, run,
                                       r_GDPcurr, r_GDPgrow, r_GDPpercap, r_pop, r_Trade, 
                                       s_GDPcurr, s_GDPgrow, s_GDPpercap, s_pop, s_Trade) %>% 
                                filter(!is.na(IN_BMD4)| !is.na(OUT_BMD4) | !is.na(OECD_IN_BMD3) | !is.na(OECD_OUT_BMD3)) %>%
                                 rename(receiver = r_iso3c,
                                        sender = s_iso3c,
                                        pair = des_pair) %>%
                                 mutate(naive = case_when(!is.na(OECD_IN_BMD3) ~ OECD_IN_BMD3*1000000,
                                                          is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ OECD_OUT_BMD3*1000000,
                                                          is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ IN_BMD4*1000000,
                                                          is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(OUT_BMD4) ~ OUT_BMD4*1000000),
                                        adjusted = case_when(!is.na(naive) & is.na(prediction) ~ naive,
                                                             !is.na(naive) & !is.na(prediction) ~ prediction*1000000
                                                            ),
                                        r_GDPcurr=r_GDPcurr*100/deflator_USD,
                                        s_GDPcurr=s_GDPcurr*100/deflator_USD,
                                        r_GDPpercap=r_GDPpercap*100/deflator_USD,
                                        s_GDPpercap=s_GDPpercap*100/deflator_USD,
                                        naive=naive*100/deflator_USD,
                                        adjusted=adjusted*100/deflator_USD,
                                        IN_BMD4=IN_BMD4*1000000*100/deflator_USD,
                                        across(c("r_GDPcurr", "s_GDPcurr"), ~ log(.x*1000000)),
                                        across(c("r_GDPpercap", "s_GDPpercap"), ~ log(.x*1000000)),
                                        t_naive=asinh(naive),
                                        t_adjusted=asinh(adjusted),
                                        ln_naive=log(naive),
                                        ln_adjusted=log(adjusted),
                                        diff_GDPcap = s_GDPpercap - r_GDPpercap
                                        ) %>%
                                        group_by(pair) %>%
                                        fill(c("target","run"), .direction = "downup") %>%
                                        group_by(sender, year) %>%
                                        mutate(s_sum_BIT = sum(BIT, na.rm=T)) %>%
                                        group_by(receiver , year) %>%
                                        mutate(r_sum_BIT = sum( BIT, na.rm = T ))


#### plausibility check #####
practical_test_data[practical_test_data$fin_center==0 & practical_test_data$target=="OECD_IN_BMD3" ,] %>% group_by(year) %>% 
               summarize(mean_normal=mean(IN_BMD4), mean_adjusted=mean(adjusted), mean_naive=mean(naive), sum_BIT=sum(BIT)) %>% 
               ggplot() + geom_line(aes(y=mean_naive, x=year)) + geom_line(aes(y=mean_adjusted, x=year, color="adjusted")) + 
               geom_line(aes(y=mean_normal, x=year, color="normal"))


#### practical research test #####

# whole sample comparison
simple_model <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT   | pair + receiver^year + sender^year, 
                      data = practical_test_data)

simple_model_r <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT +
                              r_sum_BIT + r_GDPcurr + r_Trade + r_GDPpercap  
                              | pair + receiver + sender^year + year, 
                        data = practical_test_data)

simple_model_rs <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT  +
                           r_sum_BIT + r_GDPcurr + r_Trade + r_GDPpercap +
                           s_sum_BIT + s_GDPcurr + s_Trade + s_GDPpercap
                        | pair + receiver + sender + year, 
                        data = practical_test_data)

write.csv(etable(simple_model, simple_model_r, simple_model_rs, 
                 headers = rep(c("Case 1", "Case 2", "Case 3"), each=2),
                 style.df = style.df(fixef.title = "",
                             fixef.suffix = " fixed effect", yesNo = "yes")))
