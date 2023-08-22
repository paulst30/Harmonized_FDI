#### prediction performance ####
#mean average difference (MAD) between target vintage and predictor vintage (naive approach)
addition <- prediction_train_tdiff %>% group_by(run) %>% summarize(NaiveR2 = 1-(sum((dep_var-predictor)^2)/sum((dep_var-mean(dep_var, na.rm=T))^2)),
                                                                   NaiveMAE = mean(abs(dep_var-predictor))) 

prediction_summary_tdiff <- cbind(prediction_summary_tdiff,addition)  %>%
  mutate(TrainR2 = round(TrainR2, digits = 2),
         TestR2 = round(TestR2, digits=2),
         NaiveR2 = round(NaiveR2, digits=2),
         across(c(NaiveMAE,TrainRMSE,TrainMAE,TestRMSE,TestMAE), ~round(.x,digits = 0)),
         target_vintage = c("IN BMD3", "IN BMD3", "IN BMD3", "OUT BMD3", "OUT BMD3", "IN BMD4"),
         predictor_vintage = c("IN BMD4", "OUT BMD4", "OUT BMD3", "IN BMD4", "OUT BMD4", "OUT BMD4")) 

prediction_summary_tdiff <- prediction_summary_tdiff[,c("target_vintage", "predictor_vintage", "NaiveR2","NaiveMAE","TrainR2" ,"TrainRMSE", "TrainMAE", "TestR2" ,"TestRMSE", "TestMAE")]
write.csv(prediction_summary_tdiff, row.names=F) #output for paper

#### best tunes ####
#extract performance values and alpha levels form the list they were stored in
overview_besttune <- besttune[[1]] 
for (i in 2:6){
  overview_besttune <- rbind(overview_besttune, besttune[[i]]) 
}
overview_besttune <- overview_besttune[,1:4] %>% 
                    mutate(target_vintage = c("IN BMD3", "IN BMD3", "IN BMD3", "OUT BMD3", "OUT BMD3", "IN BMD4"),
                           predictor_vintage = c("IN BMD4", "OUT BMD4", "OUT BMD3", "IN BMD4", "OUT BMD4", "OUT BMD4"))

overview_besttune <- overview_besttune[,c("target_vintage", "predictor_vintage", "alpha", "MAE", "RMSE", "pRsquared")]
write.csv(overview_besttune, row.names = F) #output for paper

#### prediction tasks ####
colnames(prediction_tasks) <- c("target_vintage", "predictor_vintage", "Training_set", "Prediction_set", "Training_example")
prediction_tasks <- as.data.frame(prediction_tasks) %>% 
                    mutate(target_vintage = c("IN BMD3", "IN BMD3", "IN BMD3", "OUT BMD3", "OUT BMD3", "IN BMD4"),
                           predictor_vintage = c("IN BMD4", "OUT BMD4", "OUT BMD3", "IN BMD4", "OUT BMD4", "OUT BMD4"), 
                           Training_example= round(as.numeric(Training_example), digits=2))  #share of country pairs that are in the training and prediction set
write.csv(prediction_tasks, row.names = F) #output for paper


#### graphical representation of the performance ####

list_plot_1 <- list() #first column in final graph
list_plot_2 <- list() #second column in final graph

#loop over each prediction task, plot two graphs and save in the lists
for (i in 1:6) { 
list_plot_1[[i]] <- ggplot(data = prediction_train_tdiff[prediction_train_tdiff$run==i,]) + geom_jitter(aes(y=dep_var, x=predictor)) +
  labs(x=prediction_tasks[i,2],y=prediction_tasks[i,1])

list_plot_2[[i]] <- ggplot(data = prediction_train_tdiff[prediction_train_tdiff$run==i,]) + geom_jitter(aes(y=dep_var, x=prediction)) +
  labs(x="prediction",y="") +
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank())  #remove y axis ticks
}

#combine all graphs
plot_grid(list_plot_1[[1]],
          list_plot_2[[1]],
          list_plot_1[[2]],
          list_plot_2[[2]],
          list_plot_1[[3]],
          list_plot_2[[3]],
          list_plot_1[[4]],
          list_plot_2[[4]],
          list_plot_1[[5]],
          list_plot_2[[5]],
          list_plot_1[[6]],
          list_plot_2[[6]],
          ncol = 2,
          rel_widths = c(1.1, 1))

#### calculate expected error reduction rate #####

#derive expected error reduction rate by weighting each error reduction rate by the corresponding obs in the prediction set
error_red <- matrix(ncol=1, nrow=6)

for (i in 1:6) {    #loop over prediction tasks
error_red[i] <- prediction_train_tdiff %>% filter(run==i) %>% 
    summarize(error_red = 1-(sum(abs(dep_var-prediction), na.rm = T)/sum(abs(dep_var-predictor)))) #calculate error reduction
}
#combine error reduction rates with observations in the prediction set and calculate the weighted average
error_red <- cbind(as.numeric(unlist(error_red)), as.numeric(prediction_tasks$Prediction_set))%>% as.data.frame() %>% 
             summarize(error_red = sum(V1*V2)/sum(V2)) %>% print()


#### error reduction by decile ####
quintile_perf <-  prediction_train_tdiff %>%  
                  mutate(quintile = ntile(dep_var, n=10)) %>%
                  group_by(quintile) %>%
                  summarize(min=min(dep_var),
                            max = max(dep_var),
                            MAE_naive=mean(abs(dep_var-predictor), na.rm=T),
                            MAE_prediction=mean(abs(dep_var-prediction), na.rm=T),
                            delta = (MAE_prediction-MAE_naive)/MAE_naive) 
                 
write.csv(quintile_perf)

