#### Introduction graph #####

world_graph_data <- select(data, OECD_IN_BMD4, OECD_IN_BMD3, OECD_OUT_BMD4, OECD_OUT_BMD3, year, des_pair) %>%
  group_by(des_pair) %>%
  mutate(ind_in_bmd4 = max(!is.na(OECD_IN_BMD4)), 
         ind_in_bmd3 = max(!is.na(OECD_IN_BMD3)),
         ind_out_bmd4 = max(!is.na(OECD_OUT_BMD4)),
         ind_out_bmd3 = max(!is.na(OECD_OUT_BMD3)),
         indicator = ind_in_bmd4 + ind_in_bmd3 + ind_out_bmd4 + ind_out_bmd3) %>%
  ungroup() %>%
  filter(indicator==4)

world_graph <- world_graph_data %>% group_by(year) %>%
  summarise(IN_BMD4=sum(OECD_IN_BMD4, na.rm = T), 
            IN_BMD3=sum(OECD_IN_BMD3, na.rm = T),
            OUT_BMD3=sum(OECD_OUT_BMD3, na.rm = T),
            OUT_BMD4=sum(OECD_OUT_BMD4, na.rm = T))

# sample world-graph
length(unique(world_graph_data$des_pair))

transition <- select(data, OECD_IN_BMD4, OECD_IN_BMD3, OECD_OUT_BMD4, OECD_OUT_BMD3, year, des_pair) %>%
  group_by(des_pair) %>%
  mutate(ind_in_bmd4 = max(!is.na(OECD_IN_BMD4)), 
         ind_in_bmd3 = max(!is.na(OECD_IN_BMD3)),
         ind_out_bmd4 = max(!is.na(OECD_OUT_BMD4)),
         ind_out_bmd3 = max(!is.na(OECD_OUT_BMD3)),
         indicator = ind_in_bmd4 + ind_in_bmd3 + ind_out_bmd4 + ind_out_bmd3) %>%
  ungroup() %>%
  filter(indicator==4 & !is.na(OECD_IN_BMD4) & !is.na(OECD_IN_BMD3) & !is.na(OECD_IN_BMD4) & !is.na(OECD_IN_BMD3)) %>%
  group_by(year) %>%
  summarise(IN_BMD4=sum(OECD_IN_BMD4, na.rm = T), 
            IN_BMD3=sum(OECD_IN_BMD3, na.rm = T),
            OUT_BMD4=sum(OECD_OUT_BMD4, na.rm = T), 
            OUT_BMD3=sum(OECD_OUT_BMD3, na.rm = T))


world_graph$IN_BMD3[world_graph$year>2012]<-NA
world_graph$OUT_BMD3[world_graph$year>2012]<-NA
world_graph$IN_BMD4[world_graph$year<2013]<-NA
world_graph$OUT_BMD4[world_graph$year<2013]<-NA



world_graph  %>% pivot_longer(!year,  names_to = "vintage", values_to = "FDI") %>%
  mutate(vintage=gsub("_", " ", vintage)) %>%
  ggplot() + 
  geom_line(mapping = aes(x=year, y=FDI, linetype = vintage), linewidth=1 ) +
  geom_vline(xintercept=2012.5) +
  annotate("text",x=2013 ,y=Inf ,label="BMD4", vjust="inward", hjust="outward") +
  annotate("text",x=2012 ,y=Inf ,label="BMD3", vjust="inward", hjust="inward") +
  labs(x="Year", y="FDI") 

#Note: Aggregate FDI stock of 845 country pairs by vintage. Sample only includes country pairs that have non-missing observations in all vintages.
#Source: OECD FDI statistics. 


##### comparison of different vintages #####
graph_data_comparison <- data %>% mutate(SPE_ind=ifelse(s_iso3c=="LUX" | s_iso3c=="NLD" |
                                                          s_iso3c=="BEL" | s_iso3c=="HUN" |
                                                          r_iso3c=="LUX" | r_iso3c=="HUN" |
                                                          r_iso3c=="NLD" | r_iso3c=="BEL","LUX, NLD, BEL, HUN","Other")) %>%
  select(r_iso3c, s_iso3c, IN_BMD4, OUT_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3, SPE_ind, fin_center)


sum_BMD4 <- graph_data_comparison %>% summarize(n=sum(!is.na(IN_BMD4) & !is.na(OUT_BMD4)),
                                                p=round(cor(IN_BMD4, OUT_BMD4, use="pairwise.complete.obs"), digits = 2)) %>%
  mutate(label = paste("n =", n, "\n\rp =", p),
         position = 5000) %>%
  select(label, position)
sum_BMD3 <- graph_data_comparison %>% summarize(n=sum(!is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3)),
                                                p=round(cor(OECD_IN_BMD3, OECD_OUT_BMD3, use="pairwise.complete.obs"),digits = 2)) %>%
  mutate(label = paste("n =", n, "\n\rp =", p),
         position = 5000) %>%
  select(label, position)
sum_comp_IN <- graph_data_comparison %>% summarize(n=sum(!is.na(IN_BMD4) & !is.na(OECD_IN_BMD3)),
                                                   p=round(cor(IN_BMD4, OECD_IN_BMD3, use="pairwise.complete.obs"), digits = 2)) %>%
  mutate(label = paste("n =", n, "\n\rp =", p),
         position = 5000) %>%
  select(label, position)
sum_comp_OUT <- graph_data_comparison %>% summarize(n=sum(!is.na(OUT_BMD4) & !is.na(OECD_OUT_BMD3)),
                                                    p=round(cor(OUT_BMD4, OECD_OUT_BMD3, use="pairwise.complete.obs"), digits = 2)) %>%
  mutate(label = paste("n =", n, "\n\rp =", p),
         position = 5000) %>%
  select(label, position)

graph_BMD4 <- ggplot(data = graph_data_comparison) + 
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center==1,], aes(x=OUT_BMD4, y=IN_BMD4), shape=6) +
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center!=1,], aes(x=OUT_BMD4, y=IN_BMD4)) +
  labs(x="OUT BMD4", y="IN BMD4") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1500000)) +
  geom_text(data=sum_BMD4, aes(x=position, label=label), y=1480000,hjust = 0 , size=3)

graph_BMD3 <- ggplot(data = graph_data_comparison) + 
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center==1,], aes(x=OECD_OUT_BMD3, y=OECD_IN_BMD3), shape=6) +
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center!=1,], aes(x=OECD_OUT_BMD3, y=OECD_IN_BMD3)) +
  labs(x="OUT BMD3", y="IN BMD3") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1500000)) +
  geom_text(data=sum_BMD3, aes(x=position, label=label), y=1480000,hjust = 0, size=3)


graph_comparison_IN <- ggplot(data = graph_data_comparison) +
  geom_point(data = graph_data_comparison[graph_data_comparison$r_iso3c!="LUX" & graph_data_comparison$r_iso3c!="NLD",], aes(y=IN_BMD4, x=OECD_IN_BMD3)) +
  geom_text(aes(label="LUX"),y=500000,x=-10000, size=3)+
  geom_text(aes(label="NLD"),y=650000,x=170000, size=3)+
  geom_point(data= graph_data_comparison[graph_data_comparison$r_iso3c=="LUX",], aes(y=IN_BMD4, x=OECD_IN_BMD3), shape=6) + 
  geom_point(data= graph_data_comparison[graph_data_comparison$r_iso3c=="NLD",], aes(y=IN_BMD4, x=OECD_IN_BMD3), shape=7) +
  labs(x="IN BMD3", y="IN BMD4") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1000000)) +
  geom_text(data=sum_comp_IN, aes(x=position, label=label), y=980000,hjust = 0, size=3)

graph_comparison_OUT <- ggplot() + 
  geom_point(data = graph_data_comparison[graph_data_comparison$s_iso3c!="LUX" & graph_data_comparison$s_iso3c!="NLD",], aes(y=OUT_BMD4, x=OECD_OUT_BMD3))  + 
  geom_text(aes(label="LUX"),y=575000,x=-30000, size=3)+
  geom_text(aes(label="NLD"),y=670000,x=160000, size=3)+
  geom_point(data= graph_data_comparison[graph_data_comparison$s_iso3c=="LUX",], aes(y=OUT_BMD4, x=OECD_OUT_BMD3), shape=6) + 
  geom_point(data= graph_data_comparison[graph_data_comparison$s_iso3c=="NLD",], aes(y=OUT_BMD4, x=OECD_OUT_BMD3), shape=7) +
  labs(x="OUT BMD3", y="OUT BMD4") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1000000)) +
  geom_text(data=sum_comp_OUT, aes(x=position, label=label), y=980000,hjust = 0, size=3)


plot_grid(graph_BMD4, graph_BMD3,graph_comparison_IN,graph_comparison_OUT,  nrow = 2, labels = c('A', 'B', 'C', 'D'), align="hv")


##### visualization of finflows technique #####
#example is based on harmonizing IN BMD4 with OUT BMD4
finflow_example <- data %>% select(r_iso3c, s_iso3c, year, IN_BMD4, OUT_BMD4, IIP_inward) %>%
                            filter(!is.na(IN_BMD4) & !is.na(OUT_BMD4)) %>%
                            mutate(spot_share = case_when(OUT_BMD4!=0 & IN_BMD4!=0 ~ IN_BMD4/OUT_BMD4,
                                                          OUT_BMD4==0 ~ 0,
                                                          .default = NA),
                                   IIP_share = case_when(IIP_inward != 0 ~ IN_BMD4/IIP_inward,
                                                         IIP_inward == 0 ~ 0),)
finflow_metrics <- matrix(nrow = 11, ncol=2) %>% as.data.frame()
for (i in 1:11) {
if (i==1) {
finflow_metrics <- finflow_example %>% group_by(r_iso3c, s_iso3c) %>%
                           mutate(n = row_number(),
                                  pred_share = case_when(n==i ~ spot_share,
                                                         .default = NA),
                                  training_sample = case_when(n==i ~ 1,
                                                              .default = 0)
                                  ) %>%
                           fill(.,pred_share, .direction="updown") %>%
                           mutate(prediction = pred_share*OUT_BMD4) %>%
                           ungroup() %>%
                           filter(training_sample==0 ) %>% #& pred_share < 2 & pred_share > -2
                           select(r_iso3c, s_iso3c, year, IN_BMD4, OUT_BMD4, prediction)
                           # summarize(MAE_naive = round(mean(abs(IN_BMD4-OUT_BMD4), na.rm=T),digits = 2),
                           #           MAE_growth = round(mean(abs(IN_BMD4-prediction), na.rm=T), digits = 2))
} else {
    merge <- finflow_example %>% group_by(r_iso3c, s_iso3c) %>%
      mutate(n = row_number(),
             pred_share = case_when(n==i ~ spot_share,
                                    .default = NA),
             training_sample = case_when(n==i ~ 1,
                                         .default = 0)
      ) %>%
      fill(.,pred_share, .direction="updown") %>%
      mutate(prediction = pred_share*OUT_BMD4) %>%
      ungroup() %>%
      filter(training_sample==0 ) %>% #& pred_share < 2 & pred_share > -2
      select(r_iso3c, s_iso3c, year, IN_BMD4, OUT_BMD4, prediction)
    finflow_metrics <- rbind(finflow_metrics,merge)
  }
}
finflow_metrics <- finflow_metrics %>% group_by(r_iso3c, s_iso3c, year) %>%
                   summarize(across(c("IN_BMD4", "OUT_BMD4", "prediction"), ~ mean(.x, na.rm=T))) %>%
                   group_by(s_iso3c, r_iso3c) %>%
                   mutate(growth_rate=(prediction-lag(prediction, n=1L))/lag(prediction,n=1L),
                          ind_growth=case_when(growth_rate>1 | growth_rate<(-1) ~1,
                                               .default = 0))


#plot finflows
plot1 <- ggplot(data=finflow_metrics, aes(y=IN_BMD4,x=OUT_BMD4)) + geom_point() +
                labs(x="OUT BMD4",y="IN BMD4") + coord_cartesian(xlim=c(-500000,2000000))
plot2 <- ggplot(data=finflow_metrics, aes(y=IN_BMD4,x=prediction)) + geom_point() +
                labs(x="prediction",y="") + coord_cartesian(xlim=c(-500000,2000000)) + theme(axis.text.y=element_blank(),  #remove y axis labels
                                                  axis.ticks.y=element_blank()  #remove y axis ticks
                     )
plot_grid(plot1,plot2,ncol = 2,
          rel_widths = c(1.1, 1))




############### overview series ###################################
#data <- read_dta('quality_analysis_ml_data.dta')

ultimate_data_sources <- data %>% select(s_iso3c, r_iso3c, year,des_pair , OUT_BMD4, IN_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3) %>%
                                 filter(!is.na(OUT_BMD4) | !is.na(IN_BMD4) | !is.na(OECD_IN_BMD3) | !is.na(OECD_OUT_BMD3))

ultimate_data_sources$final_series[!is.na(ultimate_data_sources$OECD_IN_BMD3)] <- "Inward BMD3"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$OECD_IN_BMD3) & 
                                  !is.na(ultimate_data_sources$OECD_OUT_BMD3)] <- "Outward BMD3"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$OECD_IN_BMD3) & 
                                     is.na(ultimate_data_sources$OECD_OUT_BMD3) &
                                     !is.na(ultimate_data_sources$IN_BMD4)] <- "Inward BMD4"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$IN_BMD4) & 
                                     is.na(ultimate_data_sources$OECD_IN_BMD3) &
                                     is.na(ultimate_data_sources$OECD_OUT_BMD3) &
                                     !is.na(ultimate_data_sources$OUT_BMD4)] <- "Outward BMD4"
ultimate_data_sources$final_series <- as.factor(ultimate_data_sources$final_series)

ultimate_data_sources<-ultimate_data_sources %>% group_by(des_pair) %>% mutate(inbmd4 = if_else(final_series=="Inward BMD4","Inward BMD4", ''),
                                                        outbmd4 = if_else(final_series=="Outward BMD4","Outward BMD4", ''),
                                                        inbmd3 = if_else(final_series=="Inward BMD3","Inward BMD3", ''),
                                                        outbmd3 = if_else(final_series=="Outward BMD3", "Outward BMD3", '')) %>%
                                                 mutate(inbmd4=max(inbmd4),
                                                           outbmd4=max(outbmd4),
                                                           inbmd3=max(inbmd3),
                                                           outbmd3=max(outbmd3)) %>%
                                                mutate(series = as.factor(paste(inbmd4, outbmd4, inbmd3, outbmd3))) %>%
                                                ungroup() %>% group_by(series) %>%
                                                summarize(pairs = n_distinct(des_pair),
                                                          obs = n(),
                                                          no_IN_BMD4 = sum(final_series=="Inward BMD4"),
                                                          no_OUT_BMD4 = sum(final_series=="Outward BMD4"),
                                                          no_IN_BMD3 = sum(final_series=="Inward BMD3"),
                                                          no_OUT_BMD3 = sum(final_series=="Outward BMD3")) # how many obs end up being taken from the respective series

ultimate_data_sources$series <- str_trim(ultimate_data_sources$series)
ultimate_data_sources <- ultimate_data_sources[order(nchar(ultimate_data_sources$series)),]

overview_series <- ultimate_data_sources %>% mutate(sum = sum(no_IN_BMD4, no_OUT_BMD4, no_IN_BMD3, no_OUT_BMD3)) %>%
                                             group_by(series) %>%
                                             transmute(series = series,
                                                       pairs = pairs,
                                                       observations = sum(no_IN_BMD4, no_OUT_BMD4, no_IN_BMD3, no_OUT_BMD3),
                                                       share = round(observations/sum, digits=2))
write.csv(overview_series, row.names = F) #output for the paper


#### visualization of MOBs #####

plot(train_models[[1]], tp_args = list(yline=2,margins = c(1.5, 1.2, 1.5, 2.5)))

