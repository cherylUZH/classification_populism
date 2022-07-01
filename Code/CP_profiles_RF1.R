### CP profiles for RF1 ##

# libraries
library(tidyverse)
library(ggplot2)
library(dplyr)
library(DALEXtra)


# load data
data_imp <- read.csv("data_pop_1_imp.csv")%>%
  filter(vote_pop_1 != 4)%>% # exclude abstainers
  dplyr::select(-c(X))

# assign data to correct classes

# nominal variables
fac_vars <- c("cntry", "blgetmg", "uemp3m", "vote_pop_1", "gndr", "pop_choice_1",
              "occup", "elec_system", "vote_eu")

data_imp[,fac_vars] <- lapply(data_imp[,fac_vars], factor)

# numeric variables
num_vars <- c("agea", "rlgdgr", "hinctnta", "imbgeco", "lrscale", "sofrdst",
              "gincdif", "stfeco", "imwbcnt", "pop_att",
              "psppsgva", "psppipla", "frprtpl", "freehms", "hmsacld",
              "imueclt", "impenv", "trstep", "trstun", "stfgov", "euftf",
              "stfdem", "atchctr", "GDP")

data_imp[,num_vars] <- lapply(data_imp[,num_vars], as.numeric)

#ordinal variables
data_imp$domicil <- factor(data_imp$domicil, ordered=TRUE)

data_imp$hincfel <- factor(data_imp$hincfel, ordered=TRUE)

data_imp$imsmetn <- factor(data_imp$imsmetn, ordered =TRUE)

data_imp$imdfetn <- factor(data_imp$imdfetn, ordered =TRUE)

data_imp$impcntr <- factor(data_imp$impcntr, ordered =TRUE)

data_imp$polintr <- factor(data_imp$polintr, ordered =TRUE)

data_imp$eisced <- factor(data_imp$eisced, ordered = TRUE)

# split data
set.seed(678)
split <- initial_split(data_imp, strata = vote_pop_1)
train <- training(split)
test <- testing(split)


# build recipe
rf_rec <- recipe(vote_pop_1~., data = train)%>%
  update_role(idno, new_role = "ID") %>%
  themis::step_upsample(vote_pop_1) # upsample data

# prep the data
set.seed(765)
data_prep <-  prep(rf_rec)

# juice the data
juiced <- juice(data_prep)


# load final resuls
final_result <- readRDS("~/Uni/Master/Masterarbeit/Results/Final Results/RF1_result.rds")

# get final model
final_model <- final_result$.workflow[[1]]

# model interpretation with DALEX

# define colors for plots
col <- c("#000000", "#D37133", "#434DB1", "#D0B688")


# create explainer
total_explainer <- explain_tidymodels(final_model,
                                      data = dplyr::select(juiced, -vote_pop_1),
                                      y = as.integer(juiced$vote_pop_1))



# cp profiles and plots

# define new Labels
new_labs <- c("workflow.0" = "Non populist",
              "workflow.1" = "Left-wing populist",
              "workflow.2" = "Valence populist",
              "workflow.3" = "Right-wing populist")

# ceteris paribus profile lrscale

set.seed(123)
cp_lrscale <- model_profile(
  total_explainer,
  variables = "lrscale",
  N = 100
)

cp_lrscale %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Left (0) - Right (10) scale")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_lrscale.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)


# ceteris paribus profile imueclt

set.seed(234)
cp_imueclt <- model_profile(
  total_explainer,
  variables = "imueclt",
  N = 100
)

cp_imueclt %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Cultural life undermined (0) or enriched (10) by immigrants")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_imueclt.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)


# ceteris paribus profile stfgov

set.seed(345)
cp_stfgov <- model_profile(
  total_explainer,
  variables = "stfgov",
  N = 100
)

cp_stfgov %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Satisfaction with government,
    extremely dissatisfied (0) - extremely satisfied (10)")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_stfgov.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)



# ceteris paribus profile hmsacld

set.seed(456)
cp_hmsacld <- model_profile(
  total_explainer,
  variables = "hmsacld",
  N = 100
)

cp_hmsacld %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Gay couples right to adopt children, 
    agree strongly (1) - disagree strongly (5)")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_hmsacld.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)



# ceteris paribus profile pop_att

set.seed(567)
cp_pop_att <- model_profile(
  total_explainer,
  variables = "pop_att",
  N = 100
)

cp_pop_att %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Populist attitudes, 
    most populist (0) - least populist (10)")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_pop_att.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)


# ceteris paribus profile imwbcnt

set.seed(678)
cp_imwbcnt <- model_profile(
  total_explainer,
  variables = "imwbcnt",
  N = 100
)

cp_imwbcnt %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Immigrants make country worse (0) or better (10) place to live")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_imwbcnt.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)


# ceteris paribus profile imdfetn

set.seed(789)
cp_imdfetn <- model_profile(
  total_explainer,
  variables = "imdfetn",
  N = 100
)

cp_imdfetn %>%
  plot(geom = "profiles") +
  facet_wrap(~`_label_`, labeller = labeller(`_label_` = new_labs)) +
  scale_color_manual(values = col)+
  labs(title = NULL,
       subtitle = NULL,
       y = "Predicted probability of vote choice",
       x = "Allow many (1) /none (4) immigrants of different race/ethnic group from majority")+
  theme_light()+
  theme (strip.text = element_text(colour = "black"),
         legend.position = "none", 
         axis.text = element_text (size = 12),
         axis.title = element_text (size = 14))

ggsave("cp_imdfetn.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)


