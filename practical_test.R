#combine data with predicted observations

# check for duplicates 
prediction_tdiff %>% group_by(s_iso3c, r_iso3c, year) %>%
                     mutate(new=n()>1) %>% filter(new==1) %>% arrange(s_iso3c,r_iso3c,year)
prediction_obs <- prediction_tdiff %>% distinct(s_iso3c, r_iso3c, year , .keep_all = TRUE) #delete duplicates


# inclusion as-is muss noch eingearbeitet werden. Einige obs. werden doppelt predicted. predictor var muss mit getrackt werden

#merging data 
practical_test_data <- data %>% merge(.,prediction_obs, by = c("s_iso3c", "r_iso3c","year"), all.x=T, all.y = T) %>% 
                                select(s_iso3c, r_iso3c, year, des_pair,
                                       IN_BMD4, OUT_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3, IMF_IN, IMF_OUT,
                                       boost, target,
                                       IIA, PTA, DTT, BIT, exchange_rate, war, civil_war,
                                       r_GDPcurr, r_GDPgrow, r_GDPpercap, r_pop, r_Trade, 
                                       s_GDPcurr, s_GDPgrow, s_GDPpercap, s_pop, s_Trade) %>% 
                                filter(!is.na(IN_BMD4)| !is.na(OUT_BMD4) | !is.na(OECD_IN_BMD3) | !is.na(OECD_OUT_BMD3)) %>%
                                 rename(receiver = r_iso3c,
                                        sender = s_iso3c,
                                        pair = des_pair) %>%
                                 mutate(naive = case_when(!is.na(IN_BMD4) ~ IN_BMD4,
                                                          is.na(IN_BMD4) & !is.na(OECD_IN_BMD3) ~ OECD_IN_BMD3,
                                                          is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & !is.na(OUT_BMD4) ~ OUT_BMD4,
                                                          is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OUT_BMD4) & !is.na(OECD_OUT_BMD3) ~ OECD_OUT_BMD3),
                                        adjusted = case_when(!is.na(naive) & is.na(boost) ~ naive,
                                                             !is.na(naive) & !is.na(boost) ~ boost),
                                        t_naive=asinh(naive),
                                        t_adjusted=asinh(adjusted),
                                        ln_naive=log(naive),
                                        ln_adjusted=log(adjusted))

practical_test_data[is.na(practical_test_data$war), "war"] <- 0
practical_test_data[is.na(practical_test_data$civil_war), "civil_war"] <- 0

#########estimation################

simple_model <- feols(fml = c(ln_naive ,ln_adjusted) ~ BIT + PTA + DTT + exchange_rate | pair + receiver^year + sender^year, 
                      data = practical_test_data)

simple_model_r <- feols(fml = c(ln_naive ,ln_adjusted) ~ BIT + PTA + DTT + exchange_rate +
                              r_GDPcurr + r_GDPpercap + r_Trade + war + civil_war    
                              | pair + receiver + sender^year + year, 
                        data = practical_test_data)

simple_model_rs <- feols(fml = c(ln_naive ,ln_adjusted) ~ BIT + PTA + DTT + exchange_rate +
                          r_GDPcurr + r_GDPpercap + r_Trade + war + civil_war +
                          s_GDPcurr + s_GDPpercap + s_Trade
                        | pair + receiver + sender + year, 
                        data = practical_test_data)

write.csv(etable(simple_model, simple_model_r, simple_model_rs, 
                 headers = rep(c("Case 1", "Case 2", "Case 3"), each=2),
                 style.df = style.df(fixef.title = "",
                             fixef.suffix = " fixed effect", yesNo = "yes")))
