#read in Stata data
data1 <- read_dta('quality_analysis_ml_data.dta') 
data <- as.data.frame(do.call(cbind, data1))
data1 <- NULL
data[,-c(1,2,8)] <- data[,-c(1,2,8)] %>% mutate(across(everything(), as.numeric))

# generate combination matrix for different algorithms
predictor_matrix <- data.frame(dep_var= c("OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_IN_BMD3", "OECD_OUT_BMD3", "OECD_OUT_BMD3", "IN_BMD4"),
                               predictor = c("IN_BMD4", "OUT_BMD4", "OECD_OUT_BMD3", "IN_BMD4", "OUT_BMD4", "OUT_BMD4"))
#establish target vars
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

