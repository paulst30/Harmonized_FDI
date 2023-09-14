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
                                 mutate(naive1 = case_when(!is.na(OECD_IN_BMD3) ~ OECD_IN_BMD3*1000000,
                                                          is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ OECD_OUT_BMD3*1000000,
                                                          is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ IN_BMD4*1000000,
                                                          is.na(IN_BMD4) & is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(OUT_BMD4) ~ OUT_BMD4*1000000),
                                        adjusted1 = case_when(!is.na(naive1) & is.na(prediction) ~ naive1,
                                                             !is.na(naive1) & !is.na(prediction) ~ prediction*1000000
                                                            ),
                                        r_GDPcurr=r_GDPcurr*100/deflator_USD,
                                        s_GDPcurr=s_GDPcurr*100/deflator_USD,
                                        r_GDPpercap=r_GDPpercap*100/deflator_USD,
                                        s_GDPpercap=s_GDPpercap*100/deflator_USD,
                                        naive=naive1*100/deflator_USD,
                                        adjusted=adjusted1*100/deflator_USD,
                                        IN_BMD4=IN_BMD4*1000000*100/deflator_USD,
                                        across(c("r_GDPcurr", "s_GDPcurr"), ~ log(.x*1000000),.names = "ln_{.col}"),
                                        across(c("r_GDPpercap", "s_GDPpercap"), ~ log(.x*1000000),.names = "ln_{.col}"),
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
list_plot <- list()
list_plot[[1]] <- practical_test_data[practical_test_data$fin_center==0 ,] %>% group_by(year) %>%
                  summarize(naive = sum(naive),
                            harmonized = sum(adjusted)) %>%
                  pivot_longer(c(naive, harmonized), names_to = "series", values_to = "FDI") %>%
                  ggplot(aes(x=year, y=FDI/1000000000)) +  #FDI stock in billion USD
                  geom_line(aes(linetype=series)) +  theme(legend.position = "none") + ylab("FDI")

list_plot[[2]] <- practical_test_data[practical_test_data$fin_center==1 ,] %>% group_by(year) %>%
                  summarize(naive = sum(naive),
                            harmonized = sum(adjusted)) %>%
                  pivot_longer(c(naive, harmonized), names_to = "series", values_to = "FDI") %>%
                  ggplot(aes(x=year, y=FDI/1000000000)) +  
                  geom_line(aes(linetype=series)) + labs(linetype="Series") + theme(axis.title.y=element_blank()) #remove y axis labels
  

list_plot[[3]] <- practical_test_data[practical_test_data$fin_center==0 & !is.na(practical_test_data$target) ,] %>% 
                  mutate(task = case_when(run==1 ~ "IN BMD4 -> IN BMD3",
                                                     run==2 ~ "OUT BMD4 -> IN BMD3",
                                                     run==3 ~ "OUT BMD3 -> IN BMD3",
                                                     run==4 ~ "IN BMD4 -> OUT BMD3",
                                                     run==5 ~ "OUT BMD4 -> OUT BMD3",
                                                     run==6 ~ "OUT BMD4 -> IN BMD4")) %>%
                  group_by(year, task) %>%
                  summarize(diff = sum(adjusted-naive)) %>% 
                  ggplot(aes(x=year, y=diff/1000000000, fill=task)) + 
                  geom_col(position = "stack") + theme(legend.position = "none") + labs(y="Adjustment")


list_plot[[4]] <- practical_test_data[practical_test_data$fin_center==1 & !is.na(practical_test_data$target) ,] %>% 
                  mutate(task = case_when(run==1 ~ "IN BMD4 -> IN BMD3",
                                          run==2 ~ "OUT BMD4 -> IN BMD3",
                                          run==3 ~ "OUT BMD3 -> IN BMD3",
                                          run==4 ~ "IN BMD4 -> OUT BMD3",
                                          run==5 ~ "OUT BMD4 -> OUT BMD3",
                                          run==6 ~ "OUT BMD4 -> IN BMD4")) %>%
                  group_by(year, task) %>%
                  summarize(diff = sum(adjusted-naive)) %>% 
                  ggplot(aes(x=year, y=diff/1000000000, fill=task)) + 
                  geom_col(position = "stack") + theme(axis.title.y=element_blank()) + labs(fill="Prediction task")

#combine all graphs
legend1 <- get_legend(list_plot[[2]])
legend2 <- get_legend(list_plot[[4]])
legends <- plot_grid(legend1,
                     legend2,
                     ncol=1, align="v")
all <- plot_grid(list_plot[[1]]+ theme(axis.text.x=element_blank(),  #remove y axis labels
                                       axis.ticks.x=element_blank(),
                                       axis.title.x=element_blank()),  #remove y axis ticks,
                 list_plot[[2]]+ theme(axis.text.x=element_blank(),  #remove y axis labels
                                       axis.ticks.x=element_blank(),
                                       axis.title.x=element_blank(),
                                       legend.position = "none"),
                 list_plot[[3]],
                 list_plot[[4]]+ theme(legend.position = "none"),
                 align = "v",
                 labels = c("A","B"))
plot_grid(all,
          legends,
          align = "vh",
          ncol=2,
          rel_widths = c(2.05, .6))

# counting country pairs
length(unique(practical_test_data$pair[practical_test_data$fin_center==1]))
#5183
length(unique(practical_test_data$pair[practical_test_data$fin_center==0]))
#24863

#### top 5 affected sender and receiver ####
#sender
stop5 <- practical_test_data %>% group_by(sender) %>% 
                        summarize(naive2=sum(naive)/10000000,
                                  harmonized=sum(adjusted)/10000000,
                                  abs_adjustment=sum(abs(adjusted-naive)),
                                  rel_abs_adjustment=round(sum(abs(adjusted-naive))/sum(abs(naive))*100, digits=1)) %>%
                        arrange(desc(abs_adjustment)) %>%
                        head(n=10)
#receiver
rtop5 <- practical_test_data %>% group_by(receiver) %>% 
                        summarize(naive2=sum(naive)/10000000,
                                  harmonized=sum(adjusted)/10000000,
                                  abs_adjustment=sum(abs(adjusted-naive)),
                                  rel_abs_adjustment=round(sum(abs(adjusted-naive))/sum(abs(naive))*100, digits=1)) %>%
                        arrange(desc(abs_adjustment)) %>%
                        head(n=10)

#combine both
top_5_affect <- cbind(stop5[-4],rtop5[-4])
colnames(top_5_affect) <- c("Sender", "Naive", "Harmonized", "abs. Adjust.", "Receiver", "Naive", "Harmonized", "abs. Adjust.")
write.csv(top_5_affect, row.names=F) #output for the paper

### proportions of vintages
harmonized_data %>% mutate(across(c("harmonized_vintage","naive_vintage"), ~ as.factor(.x))) %>% group_by(harmonized_vintage) %>%
                    summarize(n = n()) %>% ungroup() %>% mutate(n=n/sum(n))
harmonized_data %>% mutate(across(c("harmonized_vintage","naive_vintage"), ~ as.factor(.x))) %>% group_by(naive_vintage) %>%
                    summarize(n = n()) %>% ungroup() %>% mutate(n=n/sum(n))

#### practical research test #####

# whole sample comparison
simple_model <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT   | pair + receiver^year + sender^year, 
                      data = practical_test_data)

simple_model_r <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT +
                              r_GDPcurr + r_Trade + r_GDPpercap  
                              | pair + receiver + sender^year + year, 
                        data = practical_test_data)

simple_model_rs <- feols(fml = c(t_naive ,t_adjusted) ~ BIT + PTA + DTT  +
                           r_GDPcurr + r_Trade + r_GDPpercap +
                           s_GDPcurr + s_Trade + s_GDPpercap
                        | pair + receiver + sender + year, 
                        data = practical_test_data)

write.csv(etable(simple_model, simple_model_r, simple_model_rs, 
                 headers = rep(c("Case 1", "Case 2", "Case 3"), each=2),
                 style.df = style.df(fixef.title = "",
                             fixef.suffix = " fixed effect", yesNo = "yes")))

# significant effects

#create additional features
sinks <- unique(data$s_iso3c[data$s_sink==1])
conduits <- unique(data$r_iso3c[data$r_conduit==1])
eu_offshore_centres <- c(
  "AND", "ATG", "AIA", "ABW", "BRB", "BHR", "BMU", "BHS", "BLZ", "COK",
  "CUW", "CYM", "DMA", "GRD", "GGY", "GIB", "HKG", "IMN", "JEY", "KNA",
  "LBN", "LCA", "LIE", "LBR", "MHL", "MSR", "MUS", "NRU", "NIU", "PAN",
  "PHL", "SYC", "SGP", "SXM", "TCA", "VCT", "VGB", "VIR", "VUT", "WSM"
)
eastern_eu_member_states <- c(
  "BGR", "HRV", "CZE", "EST", "HUN", "LTU", "LVA", "POL", "ROU", "SVK", "SVN"
)
eu_member_states <- c(
  "AUT", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA",
  "DEU", "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "ROU", "SVK", "SVN", "ESP", "SWE"
)

# practical_test_data <- mutate(practical_test_data, 
#                                s_GDPpercap = s_GDPcurr*1000000/s_pop,
#                                r_GDPpercap = r_GDPcurr*1000000/r_pop)

HIC <- unique(practical_test_data$sender[practical_test_data$year==2015 & practical_test_data$s_GDPpercap>=12275])
LIC <- unique(practical_test_data$receiver[practical_test_data$year==2015 & practical_test_data$r_GDPpercap<=4035])

practical_test_data <- mutate(practical_test_data,
                        s_fin_center = case_when(sender %in% conduits |  sender %in% sinks ~ 1,
                                                 .default = 0),
                        r_fin_center = case_when(receiver %in% conduits ~ 1,
                                                 .default = 0),
                        EU = case_when(sender %in% eu_member_states | receiver %in% eu_member_states ~ 1,
                                       .default = 0),
                        r_EasternEU = case_when( receiver %in% eastern_eu_member_states ~1,
                                              .default = 0),
                        r_offshore = case_when(receiver %in% eu_offshore_centres ~1,
                                               .default = 0),
                        s_HIC = case_when(sender %in% HIC ~ 1,
                                          .default = 0),
                        r_HIC = case_when(receiver %in% HIC ~ 1,
                                         .default = 0),
                        s_LIC = case_when(sender %in%  LIC ~ 1,
                                          .default = 0),
                        r_LIC = case_when(receiver %in% LIC ~ 1,
                                          .default = 0),
                        year2009 = case_when(year >= 2009 ~ 1,
                                            .default = 0)) 



# testing for significance with 'stacked' equation

stacked <- practical_test_data %>% select(t_naive, t_adjusted, BIT, PTA, DTT, r_fin_center, s_fin_center, r_offshore, s_HIC, r_HIC, r_LIC, EU, r_EasternEU, sender, receiver, pair, year) %>%
  pivot_longer(cols=c("t_naive","t_adjusted"), names_to = "series", values_to = "FDI") %>%
  mutate(ind = case_when(series=="t_adjusted" ~1,
                                  .default =0),
         across(c("BIT", "PTA", "DTT", "r_fin_center", "s_fin_center", "s_HIC", "r_HIC", "r_LIC", "EU"), ~case_when(ind==1 ~ .x,
                                   .default = 0), .names = "ad_{.col}"))

# simple_model_stacked <- feols(fml = FDI ~ BIT + BIT:r_fin_center + BIT:s_fin_center + PTA + DTT +
#                                           ad_BIT + ad_BIT:ad_r_fin_center + ad_BIT:ad_s_fin_center + ad_PTA + ad_DTT | 
#                                           ind^pair + ind^receiver^year + ind^sender^year, 
#                               data = stacked)

indices <- stacked %>% ungroup() %>% transmute(fullsample = 1,
                                               EU = EU,
                                               #EU_after_2009 = ifelse(EU==1 & year >=2009,1,0),
                                               HIC_HIC = ifelse(s_HIC==1 & r_HIC==1,1,0),
                                               #HIC_HIC_2009 = ifelse(s_HIC==1 & r_HIC==1 & year >=2009,1,0),
                                               HIC_LIC = ifelse(s_HIC==1 & r_LIC==1,1,0) ,
                                               #HIC_LIC_2009 = ifelse(s_HIC==1 & r_LIC==1 & year>=2009,1,0)
                                               s_fin_center = ifelse(s_fin_center==1,1,0),
                                               r_fin_center = ifelse(r_fin_center==1,1,0),
                                               r_offshore = r_offshore
                                               )
vars <- c("BIT", "DTT", "PTA")
test <- matrix(nrow=ncol(indices),ncol=length(vars))

for (i in 1:ncol(indices)) {
  subset <- as.numeric(indices[[i]])
  simple_model_stacked <- feols(fml = FDI ~ BIT + BIT:r_fin_center + BIT:s_fin_center + PTA + DTT + 
                                ind:BIT + ind:BIT:r_fin_center + ind:BIT:s_fin_center + ind:PTA + ind:DTT | 
                                ind^pair + ind^receiver^year + ind^sender^year, 
                              data = stacked[subset==1,],
                              vcov = ~pair)




for (j in 1:length(vars)) {
 #naive coefficient 
if (simple_model_stacked$coeftable[vars[j],4] > 0.1) {
  stars1 <- ""
} else if (simple_model_stacked$coeftable[vars[j],4] < 0.1 & simple_model_stacked$coeftable[vars[j],4] >= 0.05) {
  stars1 <- "."
} else if (simple_model_stacked$coeftable[vars[j],4] < 0.05 & simple_model_stacked$coeftable[vars[j],4] >= 0.01) {
  stars1 <- "*"
} else if (simple_model_stacked$coeftable[vars[j],4] < 0.01 & simple_model_stacked$coeftable[vars[j],4] >= 0.001) {
  stars1 <- "**"
} else if (simple_model_stacked$coeftable[vars[j],4] < 0.001) {
  stars1 <- "***"
}

#adjusted coefficient

#how about adjustment of degrees of freedom? -> how is clustered cov-variance matrix obtained? 
coef_sum <- simple_model_stacked$coeftable[vars[j],1]+simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),1]
se_sum <- sqrt(simple_model_stacked$cov.scaled[vars[j],vars[j]]+simple_model_stacked$cov.scaled[paste(vars[j],":ind", sep=""),paste(vars[j],":ind", sep="")] -2*simple_model_stacked$cov.scaled[vars[j],paste(vars[j],":ind", sep="")])
p_value <- 2*(1 - pt(abs(coef_sum/se_sum), simple_model_stacked$nobs-simple_model_stacked$nparams))

if (p_value > 0.1) {
  stars2 <- ""
} else if (p_value < 0.1 & p_value >= 0.05) {
  stars2 <- "."
} else if (p_value < 0.05 & p_value >= 0.01) {
  stars2 <- "*"
} else if (p_value < 0.01 & p_value >= 0.001) {
  stars2 <- "**"
} else if (p_value < 0.001) {
  stars2 <- "***"
}



#difference
if (simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] > 0.1) {
  stars3 <- ""
} else if (simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] < 0.1 & simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] >= 0.05) {
  stars3 <- "."
} else if (simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] < 0.05 & simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] >= 0.01) {
  stars3 <- "*"
} else if (simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] < 0.01 & simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] >= 0.001) {
  stars3 <- "**"
} else if (simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),4] < 0.001) {
  stars3 <- "***"
}

  
test[i,j] <- paste(round(simple_model_stacked$coeftable[vars[j],1], digits = 3)," [", round(simple_model_stacked$coeftable[vars[j],2], digits = 3),"]", stars1, " ",
                   round(coef_sum, digits = 3)," [", round(se_sum, digits = 3),"]", stars2, " ",
                   round(simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),1], digits = 3)," [", round(simple_model_stacked$coeftable[paste(vars[j],":ind", sep=""),2], digits = 3),"]", stars3,
                   sep="")  
  }
}

colnames(test) <- vars
rownames(test) <- colnames(indices)

write.csv(test, row.names = T)


