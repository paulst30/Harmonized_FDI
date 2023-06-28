######### Introduction graph ##########

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
  ggplot() + 
  geom_line(mapping = aes(x=year, y=FDI, linetype = vintage), linewidth=1 ) +
  geom_vline(xintercept=2012.5) +
  annotate("text",x=2013 ,y=Inf ,label="BMD4", vjust="inward", hjust="outward") +
  annotate("text",x=2012 ,y=Inf ,label="BMD3", vjust="inward", hjust="inward") +
  labs(x="Year", y="FDI")

#Note: Aggregate FDI stock of 845 country pairs by vintage. Sample only includes country pairs that have non-missing observations in all vintages.
#Source: OECD FDI statistics. 

transition %>% pivot_longer(!year,  names_to = "vintage", values_to = "FDI") %>% 
  ggplot() + 
  geom_line(mapping = aes(x=year, y=FDI, linetype = vintage), linewidth=1 ) +
  geom_vline(xintercept=2012.5) +
  annotate("text",x=2013 ,y=Inf ,label="BMD4", vjust="inward", hjust="outward") +
  annotate("text",x=2012 ,y=Inf ,label="BMD3", vjust="inward", hjust="inward") +
  labs(x="Year", y="FDI")

################# scatter plots chapter 1 #####################################
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
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center==1,], aes(x=OUT_BMD4, y=IN_BMD4), colour="orange2") +
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center!=1,], aes(x=OUT_BMD4, y=IN_BMD4)) +
  labs(x="Outward (BMD4)", y="Inward (BMD4)") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1500000)) +
  geom_text(data=sum_BMD4, aes(x=position, label=label), y=1480000,hjust = 0 , size=3)

graph_BMD3 <- ggplot(data = graph_data_comparison) + geom_jitter(aes(x=OECD_OUT_BMD3, y=OECD_IN_BMD3)) +
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center==1,], aes(x=OECD_OUT_BMD3, y=OECD_IN_BMD3), colour="orange2") +
  geom_point(data = graph_data_comparison[graph_data_comparison$fin_center!=1,], aes(x=OECD_OUT_BMD3, y=OECD_IN_BMD3)) +
  labs(x="Outward (BMD3)", y="Inward (BMD3)") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1500000)) +
  geom_text(data=sum_BMD3, aes(x=position, label=label), y=1480000,hjust = 0, size=3)


graph_comparison_IN <- ggplot(data = graph_data_comparison) +
  geom_point(aes(y=IN_BMD4, x=OECD_IN_BMD3)) +
  geom_text(aes(label="LUX"),y=500000,x=-10000,colour="orange2", size=3)+
  geom_text(aes(label="NLD"),y=650000,x=170000,colour="orangered2", size=3)+
  geom_point(data= graph_data_comparison[graph_data_comparison$r_iso3c=="LUX",], aes(y=IN_BMD4, x=OECD_IN_BMD3), colour="orange2") + 
  geom_point(data= graph_data_comparison[graph_data_comparison$r_iso3c=="NLD",], aes(y=IN_BMD4, x=OECD_IN_BMD3), colour="orangered2") +
  labs(x="Inward (BMD3)", y="Inward (BMD4)") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1000000)) +
  geom_text(data=sum_comp_IN, aes(x=position, label=label), y=980000,hjust = 0, size=3)

graph_comparison_OUT <- ggplot(data = graph_data_comparison) + geom_point(aes(y=OUT_BMD4, x=OECD_OUT_BMD3))  + 
  geom_text(aes(label="LUX"),y=575000,x=-30000,colour="orange2", size=3)+
  geom_text(aes(label="NLD"),y=670000,x=160000,colour="orangered2", size=3)+
  geom_point(data= graph_data_comparison[graph_data_comparison$s_iso3c=="LUX",], aes(y=OUT_BMD4, x=OECD_OUT_BMD3), colour="orange2") + 
  geom_point(data= graph_data_comparison[graph_data_comparison$s_iso3c=="NLD",], aes(y=OUT_BMD4, x=OECD_OUT_BMD3), colour="orangered2") +
  labs(x="Outward (BMD3)", y="Outward (BMD4)") +
  coord_cartesian(xlim=c(-50000,1000000), ylim =c(-50000,1000000)) +
  geom_text(data=sum_comp_OUT, aes(x=position, label=label), y=980000,hjust = 0, size=3)


plot_grid(graph_BMD4, graph_BMD3,graph_comparison_IN,graph_comparison_OUT,  nrow = 2, labels = c('A', 'B', 'C', 'D'), align="hv")







#construct decision tree
reg.tree <- rpart(data_tree$diff_inBMD4_outBMD4~., data=data_tree, method='anova')
rpart.plot(reg.tree)
printcp(reg.tree)

###########data coverage for ML-sample########################################## 
#visualizations
count <- data %>% filter(s_iso3c!="" & r_iso3c!="" & s_iso3c!="unalloc") %>%
  group_by(s_iso3c, r_iso3c) %>% 
  summarise(IN=sum(!is.na(IN_BMD4)), OUT=sum(!is.na(OUT_BMD4)), 
            r_GDPpc=mean(r_GDPcurr/r_pop), s_GDPpc=mean(s_GDPcurr/r_pop)) %>% 
  arrange(desc(s_GDPpc),desc(r_GDPpc)) %>% 
  select(s_iso3c, r_iso3c, IN) %>%
  pivot_wider(names_from = r_iso3c, values_from = IN)
count2 <- count[,count$s_iso3c] %>% as.matrix()
rownames(count2) <- count$s_iso3c


heatmap(count2,Rowv = NA,Colv = NA, labRow = "",labCol = "", xlab="receiver", ylab="sender", 
        main="Inward Stocks", margins = c(2,2) )


count <- data %>% filter(s_iso3c!="" & r_iso3c!="" & s_iso3c!="unalloc") %>%
  group_by(s_iso3c, r_iso3c) %>% 
  summarise(IN=sum(!is.na(IN_BMD4)), OUT=sum(!is.na(OUT_BMD4)), 
            r_GDPpc=mean(r_GDPcurr/r_pop), s_GDPpc=mean(s_GDPcurr/r_pop)) %>% 
  arrange(desc(s_GDPpc),desc(r_GDPpc)) %>% 
  select(s_iso3c, r_iso3c, OUT) %>%
  pivot_wider(names_from = r_iso3c, values_from = OUT)
count2 <- count[,count$s_iso3c] %>% as.matrix()
rownames(count2) <- count$s_iso3c


heatmap(count2,Rowv = NA,Colv = NA, labRow = "",labCol = "", xlab="receiver", ylab="sender", 
        main="Outward Stocks" ,margins = c(2,2))

############### heatmap for ultimate sample ###################################
#data <- read_dta('quality_analysis_ml_data.dta')

ultimate_data_sources <- data %>% select(s_iso3c, r_iso3c, year,des_pair , OUT_BMD4, IN_BMD4, OECD_IN_BMD3, OECD_OUT_BMD3) %>%
                                 filter(!is.na(OUT_BMD4) | !is.na(IN_BMD4) | !is.na(OECD_IN_BMD3) | !is.na(OECD_OUT_BMD3))

ultimate_data_sources$final_series[!is.na(ultimate_data_sources$IN_BMD4)] <- "Inward BMD4"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$IN_BMD4) & 
                                  !is.na(ultimate_data_sources$OECD_IN_BMD3)] <- "Inward BMD3"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$IN_BMD4) & 
                                     is.na(ultimate_data_sources$OECD_IN_BMD3) &
                                     !is.na(ultimate_data_sources$OUT_BMD4)] <- "Outward BMD4"
ultimate_data_sources$final_series[is.na(ultimate_data_sources$IN_BMD4) & 
                                     is.na(ultimate_data_sources$OECD_IN_BMD3) &
                                     is.na(ultimate_data_sources$OUT_BMD4) &
                                     !is.na(ultimate_data_sources$OECD_OUT_BMD3)] <- "Outward BMD3"
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

overview_prediction_INB4 <- ultimate_data_sources[ultimate_data_sources$no_IN_BMD4!=0,] %>% 
                              summarize(prOUT_BMD4 = sum(no_OUT_BMD4),
                                        prOECD_IN_BMD3 = sum(no_IN_BMD3),
                                        prOECD_OUT_BMD3 = sum(no_OUT_BMD3))
overview_prediction_INB3 <- ultimate_data_sources[ultimate_data_sources$no_IN_BMD4==0 & ultimate_data_sources$no_IN_BMD3!=0,] %>% 
                             summarize(prOUT_BMD4 = sum(no_OUT_BMD4),
                                       prOECD_IN_BMD3 = 0,
                                       prOECD_OUT_BMD3 = sum(no_OUT_BMD3))
overview_prediction_OUB4 <- ultimate_data_sources[ultimate_data_sources$no_IN_BMD4==0 & ultimate_data_sources$no_IN_BMD3==0 & ultimate_data_sources$no_OUT_BMD4!=0,] %>% 
                             summarize(prOUT_BMD4 = 0,
                                       prOECD_IN_BMD3 = 0,
                                       prOECD_OUT_BMD3 = sum(no_OUT_BMD3))
overview_prediction <- rbind(overview_prediction_INB4, overview_prediction_INB3, overview_prediction_OUB4)
overview_prediction$dep <- c("IN_BMD4", "OECD_IN_BMD3", "OUT_BMD4")

#prediction sample
overview_prediction <- pivot_longer(overview_prediction,cols=starts_with("pr"),
                                    names_prefix = "pr",
                                    names_to = "predictor", values_to = "prediction_sample" ) %>%
                                    filter(prediction_sample>0)
#training sample
overview_training <- matrix(nrow=6,ncol=1) %>% as.data.frame() 
for (i in 1:6) {
  overview_training[i,] <- sum(!is.na(data[,overview_prediction$dep[i]]) & !is.na(data[, overview_prediction$predictor[i]]))
}
colnames(overview_training) <- "training_sample" 
overview_prediction <- cbind(overview_prediction,overview_training)
overview_prediction <- overview_prediction[,c("dep", "predictor", "training_sample", "prediction_sample")]



ultimate_data_sources <- ultimate_data_sources %>% select(des_pair, year, final_series) %>%
                                 pivot_wider(names_from = year, values_from = final_series )
col_order <- c( "des_pair", "2009", "2010", "2011",
               "2012", "2013", "2014", "2015" ,"2016", "2017", "2018", "2019")
ultimate_data_sources <- ultimate_data_sources[,  col_order]
time_series <- as.matrix(ultimate_data_sources)

ggplot(data = ultimate_data_sources, aes(y=des_pair, x=year,color=final_series)) + geom_point()

##################infinity or overly large#####################################
graph_inf_data <- working_data_wm %>%
  select(IN_BMD4, OUT_BMD4, max_OUT_BMD4, diff_inBMD4_outBMD4) %>%
  mutate( ratio= IN_BMD4/OUT_BMD4,
    difficult =case_when(inrange(diff_inBMD4_outBMD4,-2,2) ~"easy",
                              diff_inBMD4_outBMD4>2 ~"difficult",
                              diff_inBMD4_outBMD4<(-2) ~"difficult"))
         
ggplot(data = graph_inf_data[graph_inf_data$ratio>5 | graph_inf_data$ratio<(.2),]) + geom_point(aes(x=OUT_BMD4*max_OUT_BMD4, y=IN_BMD4*max_OUT_BMD4 )) + 
  coord_cartesian(xlim=c(-25000,300000), ylim =c(-25000,300000) )

ggplot(data = working_data_wm) + geom_point(aes(x=OUT_BMD4, y=IN_BMD4 )) +  coord_cartesian(xlim=c(-2,2), ylim =c(-10,10) )



