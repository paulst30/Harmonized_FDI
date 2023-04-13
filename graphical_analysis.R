#install.packages("cowplot")

library(tidyverse)
library(ggplot2)
library(lattice)
library(haven)
library(rpart)
library(rpart.plot)
library(caret)
library(expss)
library(cowplot)

#read in Stata data
#data <- read_dta('quality_analysis_ml_data.dta')
load("~/Projekte/Data-Quality_Paper/R Analysis/outlier_analysis/prediction_all.rdata")

graph_data <- data %>% select(s_iso3c, r_iso3c, year, group_id, IN_BMD4, OUT_BMD4, fin_center)
graph_data <- merge(graph_data, prediction_all, by=c("s_iso3c", "r_iso3c", "year"), all.x = T)

graph_data <- graph_data %>% group_by(group_id) %>%
                             mutate(mean_FDI=mean(IN_BMD4, na.rm = T)) %>%
                             ungroup() %>%
                             mutate(rel_error=sqrt(((IN_BMD4-OUT_BMD4)/mean_FDI*100)^2),
                                    adj_OUT_BMD4=OUT_BMD4+prediction, 
                                    adj_rel_error=sqrt(((IN_BMD4-adj_OUT_BMD4)/mean_FDI*100)^2),
                                    rel_error2=2*abs(IN_BMD4-OUT_BMD4)/(abs(IN_BMD4)+abs(OUT_BMD4)),
                                    adj_rel_error2=2*abs(IN_BMD4-adj_OUT_BMD4)/(abs(IN_BMD4)+abs(adj_OUT_BMD4))) %>%
                            as.data.frame()
graph_data$rel_error[graph_data$rel_error>200 ] <- 200
graph_data$adj_rel_error[graph_data$adj_rel_error>200 ] <- 200

# graph Inward versus outward, distinction by fin_center, comparison to prediction
diff1 <- ggplot(data = graph_data) + geom_jitter(aes(y=IN_BMD4, x=OUT_BMD4, color=rel_error2)) +
                       facet_wrap(~ fin_center, ncol = 2) #+
  #coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
                       #labs(title = "Differences between inward and outward stocks",
                        #      caption = "N=XX")
adjust1 <- ggplot(data = graph_data) + geom_jitter(aes(y=IN_BMD4, x=adj_OUT_BMD4, color=adj_rel_error2)) +
  facet_wrap(~ fin_center, ncol = 2) #+
 # coord_cartesian(xlim=c(-500,1000), ylim =c(-500,1000) )
plot_grid(diff1, adjust1, nrow = 2)

#graph histogram of rel measurement error
hist_diff1 <- ggplot(data = graph_data) + geom_histogram(aes(x=rel_error2, y=after_stat(count/sum(count)*100))) +
  facet_wrap(~ fin_center, ncol = 2) 
hist_adjust1 <- ggplot(data =graph_data) + geom_histogram(aes(x=adj_rel_error2, y=after_stat(count/sum(count)*100))) +
  facet_wrap(~ fin_center, ncol = 2) 
plot_grid(hist_diff1, hist_adjust1, nrow = 2)

ggplot(data=subset(prediction_data_wm,prediction_data_wm$OUT_BMD4<25000)) + geom_histogram(aes(x=OUT_BMD4, y=after_stat(count/sum(count)*100)),bins=20)
ggplot(data=working_data_wm) + geom_histogram(aes(x=IN_BMD4, y=after_stat(count/sum(count)*100)),bins=10000)+
  coord_cartesian(xlim=c(0,10000))

#performance by quintile
quin_perf_data <- graph_data %>% 
                    mutate(quintile = ntile(OUT_BMD4, 10)) %>%
                    group_by(quintile) %>%
                    summarise(
                      min = min(OUT_BMD4),
                      max = max(OUT_BMD4),
                      Errorred = round(1-(sum(abs(IN_BMD4-OUT_BMD4-prediction), na.rm=T))/sum(abs(IN_BMD4-OUT_BMD4), na.rm=T), digits=2),
                      pRsquared = round(1-(sum((IN_BMD4-OUT_BMD4-prediction)^2, na.rm=T)/sum((IN_BMD4-OUT_BMD4)^2, na.rm=T)), digits=2)
                      )
print(quin_perf_data)  
  
  
  


#are most of the high-percentage deviations from small FDI inflow countries?
data <- data %>% mutate(quartile = ntile(mean_IN_BMD4, 7))
ggplot(data=data) + geom_boxplot(mapping=aes(y=p_diff_inBMD4_outBMD4,x=factor(quartile)))+ ylim(-300,300)
ggplot(data=data) + geom_histogram(mapping=(aes(x=p_diff_inBMD4_outBMD4, y=after_stat(count/sum(count)*100)) ), bins=20) +
    cord_cartesian(xlim=c(0,10000))

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

ultimate_data_sources <- ultimate_data_sources %>% select(des_pair, year, final_series) %>%
                                 pivot_wider(names_from = year, values_from = final_series )
col_order <- c( "2009", "2010", "2011",
               "2012", "2013", "2014", "2015" ,"2016", "2017", "2018", "2019")
ultimate_data_sources <- ultimate_data_sources[, col_order]
time_series <- as.matrix(ultimate_data_sources)

ggplot(data = ultimate_data_sources, aes(y=des_pair, x=year,color=final_series)) + geom_point()

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




