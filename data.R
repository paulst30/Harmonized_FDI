#### read in data ####
data <- read.csv('quality_analysis_ml_data.csv')


#### generate combination matrix for different algorithms ####
predictor_matrix <- data.frame(dep_var= c("OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_OUT_BMD3", "OECD_OUT_BMD3", "IN_BMD4"),
                               predictor = c("IN_BMD4", "OUT_BMD4", "OECD_OUT_BMD3", "IN_BMD4", "OUT_BMD4", "OUT_BMD4"))

#### establish target and predictor vintages ####
#"inclusion" marks which vintage would end up in a naively combined data set
#"target_var" identifies the available vintage with the highest priority
data <- data %>% mutate(inclusion= case_when(!is.na(OECD_IN_BMD3) ~ "IN_BMD3",          
                                             is.na(OECD_IN_BMD3) & !is.na(OECD_OUT_BMD3) ~ "OUT_BMD3",
                                             is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & !is.na(IN_BMD4) ~ "IN_BMD4",
                                             is.na(OECD_IN_BMD3) & is.na(OECD_OUT_BMD3) & is.na(IN_BMD4) & !is.na(OUT_BMD4) ~ "OUT_BMD4",
                                             .default = "")) %>%
                                             group_by(des_pair) %>%
                  mutate(target_var = case_when(as.logical(max(inclusion=="IN_BMD3", na.rm = T)) ~ "OECD_IN_BMD3",          
                                                as.logical(max(inclusion!="IN_BMD3" & inclusion=="OUT_BMD3" , na.rm = T)) ~ "OECD_OUT_BMD3",
                                                as.logical(max(inclusion!="IN_BMD3" & inclusion!="OUT_BMD3" & inclusion=="IN_BMD4" , na.rm = T)) ~ "IN_BMD4")) %>%
                                             ungroup()

