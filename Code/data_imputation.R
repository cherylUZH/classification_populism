### data imputation (income variable) ###

# libraries
library(dplyr)
library(ranger)
library(missRanger)
library(ggplot2)

# data imputation for data_pop_1

# load data
data_pop_1 <- read.csv("data_pop_1.csv") %>%
  select(-"X")

# remove all NAs except for income
data_pop_1 <- data_pop_1 %>%
  drop_na(c(idno, cntry, agea, eisced, gndr, rlgdgr,       
            domicil, blgetmg, uemp3m, hincfel, stfdem, stfeco,       
            gincdif, sofrdst, lrscale, trstep, trstun, stfgov,       
            euftf, imsmetn, imdfetn, impcntr, imbgeco, imwbcnt,      
            polintr, psppsgva, psppipla, frprtpl, freehms, hmsacld,     
            atchctr, imueclt, impenv, vote_pop_1, elec_system,
            GDP, pop_choice_1, occup, vote_eu, pop_att))


# create formula for imputation, use all other predictor variables for imputation
formula_imp <- formula(hinctnta ~ cntry+ agea+ eisced+ gndr+ rlgdgr+       
                         domicil+ blgetmg+ uemp3m+ hincfel+ stfdem+ stfeco+       
                         gincdif+ sofrdst+ lrscale+ trstep+ trstun+ stfgov+       
                         euftf+ imsmetn+ imdfetn+ impcntr+ imbgeco+ imwbcnt+      
                         polintr+ psppsgva+ psppipla+ frprtpl+ freehms+ hmsacld+     
                         atchctr+ imueclt+ impenv+ elec_system+ 
                         GDP+ pop_choice_1+ occup+ vote_eu+ pop_att)

# imute missing values for income
set.seed(5647)
list_imputed <- list()
list_imputed[["default"]] <- missRanger::missRanger(
  data = data_pop_1,
  pmm.k = 3,
  verbose = 2,
  formula = formula_imp,
  returnOOB = TRUE)

# extract dataframe with imputed data
data_pop_1_imp <- list_imputed$default

# t-test

# add vairable to differentiate data
data_pop_1_imp$data <- "imputed"
data_pop_1$data <- "original"

# combine into one data frame
data_test <- rbind(data_imp_full, data_pop_1)%>%
  as.data.frame()

# perform t-test
t.test(data_test$hinctnta ~ data_test$data, mu = 0, alt = "two.sided",
       conf = 0.95, var.equal = FALSE)



# data imputation for data_pop_2

# load data
data_pop_2 <- read.csv("data_pop_2.csv")

# remove all NAs except for income
data_pop_2 <- data_pop_2 %>%
  drop_na(c(idno, cntry, agea, eisced, gndr, rlgdgr,       
            domicil, blgetmg, uemp3m, hincfel, stfdem, stfeco,       
            gincdif, sofrdst, lrscale, trstep, trstun, stfgov,       
            euftf, imsmetn, imdfetn, impcntr, imbgeco, imwbcnt,      
            polintr, psppsgva, psppipla, frprtpl, freehms, hmsacld,     
            atchctr, imueclt, impenv, vote_pop_2, elec_system,
            GDP, pop_choice_2, occup, vote_eu, pop_att))

# create formula for imputation, use all other predictor variables for imputation
formula_imp <- formula(hinctnta ~ cntry+ agea+ eisced+ gndr+ rlgdgr+       
                         domicil+ blgetmg+ uemp3m+ hincfel+ stfdem+ stfeco+       
                         gincdif+ sofrdst+ lrscale+ trstep+ trstun+ stfgov+       
                         euftf+ imsmetn+ imdfetn+ impcntr+ imbgeco+ imwbcnt+      
                         polintr+ psppsgva+ psppipla+ frprtpl+ freehms+ hmsacld+     
                         atchctr+ imueclt+ impenv+ elec_system+ 
                         GDP+ pop_choice_2+ occup+ vote_eu+ pop_att)

# imute missing values for income
set.seed(5678)
list_imputed_2 <- list()
list_imputed_2[["default"]] <- missRanger::missRanger(
  data = data_pop_2,
  pmm.k = 3,
  verbose = 2,
  formula = formula_imp,
  returnOOB = TRUE)

# extract dataframe with imputed data
data_pop_2_imp <- list_imputed_2$default


# save data
write.csv(data_pop_1_imp , "~/Uni/Master/Masterarbeit/Data/Clean Data/data_pop_1_imp.csv")
write.csv(data_pop_2_imp , "~/Uni/Master/Masterarbeit/Data/Clean Data/data_pop_2_imp.csv")


# plot original and imputed income variable for vote_pop_1

# create new data frame with original and imputed income
income = as.data.frame(cbind(inc = data_pop_1$hinctnta, 
                             inc_imp = data_pop_1_imp$hinctnta))

# plot
income %>%
  pivot_longer(cols = c("inc_imp", "inc"))%>%
  ggplot(aes(x = value, fill = name))+
  geom_histogram(position = "dodge", alpha = 0.8)+
  labs(
    x= "Income (Decile)",
    y = "Frequency",
    fill = " ")+
  scale_fill_grey(labels = c("Original", "Imputed"))+
  scale_x_continuous(breaks = seq(1,10, by =1))+
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

# save plot
ggsave("hist_income.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)
