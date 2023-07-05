data <- read_dta('quality_analysis_ml_data.dta')
#combine data with predicted observations

# check for duplicates 
prediction_tdiff %>% group_by(s_iso3c, r_iso3c, year) %>%
                     mutate(new=n()>1) %>% filter(new==1) %>% arrange(s_iso3c,r_iso3c,year)
prediction_obs <- prediction_tdiff %>% distinct(s_iso3c, r_iso3c, year , .keep_all = TRUE) #delete duplicates


#predictor var muss mit getrackt werden

deflator <- data %>% filter(r_iso3c=="USA") %>% select(year, r_GDPdef) %>% group_by(year) %>% 
                     summarize(deflator_USD=mean(r_GDPdef))

#merging data 
practical_test_data <- data %>% merge(.,prediction_obs, by = c("s_iso3c", "r_iso3c","year"), all.x=T, all.y = T) %>% 
                                merge(., exclusion_ind, by = c("s_iso3c", "r_iso3c"), all.x=T) %>%
                                merge(., deflator, by= "year", all.x=T) %>%
                                select(s_iso3c, r_iso3c, year, des_pair,
                                       IN_BMD4, OUT_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3, IMF_IN, IMF_OUT,
                                       boost, target, error_imp, fin_center, deflator_USD,
                                       IIA, PTA, DTT, BIT, exchange_rate, war, civil_war,
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
                                        adjusted = case_when(!is.na(naive) & is.na(boost) ~ naive,
                                                             !is.na(naive) & !is.na(boost) ~ boost*1000000,
                                                             error_imp==1 ~ naive),
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
                                        ) #%>%
                                        #filter(fin_center==0)

practical_test_data[is.na(practical_test_data$war), "war"] <- 0
practical_test_data[is.na(practical_test_data$civil_war), "civil_war"] <- 0

#########plausibility check#####################
practical_test_data[!is.na(practical_test_data$IN_BMD4) & practical_test_data$fin_center==0 ,] %>% group_by(year) %>% 
               summarize(mean_normal=sum(IN_BMD4), mean_adjusted=sum(adjusted), mean_naive=sum(naive), sum_BIT=sum(BIT)) %>% 
               ggplot() + geom_line(aes(y=mean_naive, x=year)) + geom_line(aes(y=mean_adjusted, x=year, color="adjusted")) + 
               geom_line(aes(y=mean_normal, x=year, color="normal"))


#########practical research test################

#comparing sample of just BMD3 in/outflows
IN_BMD3_comparison <- feols(fml = c(t_naive ,t_adjusted) ~ IIA + PTA + DTT  | pair + receiver^year + sender^year, 
                            data = practical_test_data[!is.na(practical_test_data$OECD_OUT_BMD3),])
summary(IN_BMD3_comparison)

#compariinf sample of just BMD4 in/outflows
IN_BMD4_comparison <- feols(fml = c(t_naive ,t_adjusted) ~ IIA + PTA + DTT  | pair + receiver^year + sender^year, 
                            data = practical_test_data)

simple_model <- feols(fml = c(t_naive ,t_adjusted) ~ BIT    | pair + receiver^year + sender^year, 
                      data = practical_test_data)

simple_model_r <- feols(fml = c(t_naive ,t_adjusted) ~ BIT  +
                              r_GDPcurr + r_Trade + r_GDPpercap  
                              | pair + receiver + sender^year + year, 
                        data = practical_test_data)

simple_model_rs <- feols(fml = c(t_naive ,t_adjusted) ~ BIT   +
                          r_GDPcurr + r_Trade + r_GDPpercap +
                          s_GDPcurr + s_Trade + s_GDPpercap
                        | pair + receiver + sender + year, 
                        data = practical_test_data)

write.csv(etable(simple_model, simple_model_r, simple_model_rs, 
                 headers = rep(c("Case 1", "Case 2", "Case 3"), each=2),
                 style.df = style.df(fixef.title = "",
                             fixef.suffix = " fixed effect", yesNo = "yes")))
