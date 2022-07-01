### RF2 ###

# includes the model, model evaluation and interpretation with DALEX and LIME


# libraries
library(tidyverse)
library(tidymodels)
library(themis)
library(parsnip)
library(workflows)
library(rsample)
library(parallel)
library(tune)
library(ggplot2)
library(dials)
library(vip)
library(caret)
library(lime)
library(dplyr)
library(DALEXtra)
library(cowplot)
library(gridExtra)
library(grid)

# load data
data_imp <- read.csv("data_pop_1_imp.csv")%>%
  filter(vote_pop_1 != 4 & vote_pop_1 != 0)%>% # exclude abstainers and non-populist voters
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
set.seed(123)
split <- initial_split(data_imp, strata = vote_pop_1)
train <- training(split)
test <- testing(split)


# build recipe
rf_rec <- recipe(vote_pop_1~., data = train)%>%
  update_role(idno, new_role = "ID") %>%
  themis::step_upsample(vote_pop_1) # upsample data

# prep the data
set.seed(234)
data_prep <-  prep(rf_rec)

# juice the data
juiced <- juice(data_prep)


# set model specification for hyperparameter tuning
tune_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger", importance = "permutation")

# set workflow
tune_workflow <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(tune_spec)

# create folds for 5-fold cross validation
set.seed(345)
folds <- vfold_cv(train, v=5, strata = vote_pop_1)


# tune hyperparameters
# set up parallel processing
doParallel::registerDoParallel()
set.seed(765)
tune_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 10)

# best values
tune_res %>% select_best(metric = "accuracy") #mtry = 18 , min_n 39

# visualization area under the curve for the tuned parameters
tune_res %>%
  collect_metrics() %>%
  filter(.metric == "roc_auc") %>%
  dplyr::select(mean, mtry, min_n) %>%
  tidyr::pivot_longer(c(min_n, mtry),
                      values_to = "value",
                      names_to = "parameter") %>%
  ggplot(aes(value, mean, color = parameter))+
  geom_point(show.legend = FALSE)+
  facet_wrap(~parameter, scales = "free_x")

# define grid for new tuning of hyperparameters (based on visualization)
rf_grid <- grid_regular(
  mtry(range = c(10,20)),
  min_n(range = c(35,40)),
  levels = 10
)

# tune hyperparameters
# set up parallel processing
doParallel::registerDoParallel()
set.seed(543)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid)

# select the new best hyperparameters based on ROC-AUC
best_auc <- select_best(regular_res, "roc_auc") # mtry = 11, min_n = 39

# finalize the model with best hyperparameters
final_rf <- finalize_model(
  tune_spec,
  best_auc
)

# train rf model
final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(vote_pop_1 ~.,
      data = juice(data_prep) %>%dplyr::select(-idno)) %>%
  vip(geom = "point")

# finalize workflow
final_wf <- workflow() %>%
  add_recipe(rf_rec) %>%
  add_model(final_rf)

# write to disk
write_rds(final_wf, "~/Uni/Master/Masterarbeit/Results/Final Results/RF2_wf.rds")

# train and evaluate final model

# training on the train set and evaluate on the test set
set.seed(789)
final_result <- final_wf %>%
  last_fit(split) 


# write to disk
write_rds(final_result, "~/Uni/Master/Masterarbeit/Results/Final Results/RF2_result.rds")


###############################################################################

# model evaluation

# plot ROC curves 
final_result %>%
  collect_predictions() %>%
  roc_curve(vote_pop_1, .pred_1:.pred_3) %>%
  as.data.frame()%>%
  mutate(.level = recode(.level,
                         "1" = "Left-wing populist",
                         "2" = "Valance populist",
                         "3" = "Right-wing populist")) %>%
  ggplot(aes(x = 1-specificity, y = sensitivity))+
  geom_abline(color = "gray", linetype = "dashed")+
  geom_path()+
  facet_wrap(vars(.level), ncol = 2)+
  coord_equal()+
  labs(x = "1 - Specificity",
       y = "Sensitivity")+
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        strip.text = element_text(colour = "black"),
        axis.text = element_text(size =12),
        axis.title = element_text(size = 14))

# save plot
ggsave("roc_curve_RF2.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 8,
       dpi = 400)



# plot variable importance
final_result %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 10)+
  scale_x_discrete(labels = c("pop_choice_1" = "Choice of populist party types",
                              "GDP" = "GDP growth",
                              "cntry" = "Country",
                              "lrscale" = "Left-right scale",
                              "elec_system" = "Electoral system",
                              "imueclt" = "Cultural life undermined \n or enriched by immigrants",
                              "imwbcnt" = "Immigrants make country \n better or worse",
                              "hmsacld" = "Rights for gay couples \n to adopt children",
                              "imdfetn" = "Allow many/few immigrants \n of different race",
                              "impcntr" = "Allow many/few immigrants from \n poorer countries outside Europe"))+
  theme_light()+
  theme(axis.text = element_text (size = 12),
        axis.title = element_text(size = 14))

ggsave("var_imp_RF2.png", 
       plot = last_plot(),
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400)

# variable importance table 
var_imp <- final_result %>%
  extract_fit_parsnip()

var_imp_table <- var_imp$fit$variable.importance %>%
  as.data.frame() %>%
  arrange(desc(.))


# confusion matrix, model performance measures

# extract predicted classes
pred <- final_result %>%
  collect_predictions() %>%
  dplyr::select(.pred_class)

# extract true classes
true <- final_result %>%
  collect_predictions() %>%
  dplyr::select(vote_pop_1)

# generate confusion matrix
conf <- caret::confusionMatrix(pred$.pred_class, true$vote_pop_1)

# extract performance measures
class <- as.data.frame(conf$byClass)

# model performance table
performance_RF2 <- class%>%
  select(c(Sensitivity, Specificity, 
           Precision, F1, "Balanced Accuracy")) %>%
  t()%>%
  as.data.frame() %>%
  rename(
         "Left-wing populist" = "Class: 1",
         "Valance populist" = "Class: 2",
         "Right-wing populist" = "Class: 3")

###############################################################################

# model interpretation with DALEX

# define colors for plots
col <- c("#D37133", "#434DB1", "#D0B688")

# extract model from results

final_model <- final_result$.workflow[[1]]


# create explainer
total_explainer <- explain_tidymodels(final_model,
                                      data = dplyr::select(juiced, -vote_pop_1),
                                      y = as.integer(juiced$vote_pop_1))

# partial dependence profiles and plots

# partial dependence lrscale
pdp_lrscale <- model_profile(
  total_explainer,
  variables = "lrscale",
  N = NULL
)

# plot
PDP_LRSCALE <- as_tibble(pdp_lrscale$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col, labels = c(
                                              "Left-wing populist",
                                              "Valence populist",
                                              "Right-wing populist"))+
  labs(
    title = "a)",
    x = "Left (0) - Right (10) scale",
    y = NULL,
    color = NULL,
  )+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))


# partial dependence imueclt
pdp_imueclt <- model_profile(
  total_explainer,
  variables = "imueclt",
  N = NULL
)


# plot
PDP_IMUECLT <- as_tibble(pdp_imueclt$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col, labels = c( 
                                              "Left-wing populist",
                                              "Valence populist",
                                              "Right-wing populist"))+
  labs(
    title = "b)",
    x = "Cultural life undermined (0) or enriched (10)
    by immigrants",
    y = NULL,
    color = NULL,
  )+
  scale_y_continuous(limits = c(0.27, 0.4))+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))

# partial dependence imwbcnt
pdp_imwbcnt <- model_profile(
  total_explainer,
  variables = "imwbcnt",
  N = NULL
)

# plot
PDP_IMWBCNT <- as_tibble(pdp_imwbcnt$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col, labels = c(
                                              "Left-wing populist",
                                              "Valence populist",
                                              "Right-wing populist"))+
  labs(
    title = "c)",
    x = "Immigrants make country worse (0)
    or better (10) place to live",
    y = NULL,
    color = NULL,
  )+
  scale_y_continuous(limits = c(0.27, 0.4))+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))



# partial dependence hmsacld
pdp_hmsacld <- model_profile(
  total_explainer,
  variables = "hmsacld",
  N = NULL
)

# plot
PDP_HMSACLD <- as_tibble(pdp_hmsacld$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, color = `_label_`)) +
  geom_line(size = 1.2) +
  scale_color_manual(values = col, labels = c(
                                              "Left-wing populist",
                                              "Valence populist",
                                              "Right-wing populist"))+
  labs(
    title = "d)",
    x = "Gay couples right to adopt children, 
    agree strongly (1) - disagree strongly (5)",
    y = NULL,
    color = NULL,
  )+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))




# partial dependence imdfetn
pdp_imdfetn <- model_profile(
  total_explainer,
  variables = "imdfetn",
  N = NULL
)

# plot
PDP_IMDFETN <- as_tibble(pdp_imdfetn$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, fill = `_label_`)) +
  geom_col(width = 0.8 , position = "dodge") +
  scale_fill_manual(values = col, labels = c(
                                             "Left-wing populist",
                                             "Valence populist",
                                             "Right-wing populist"))+
  scale_x_discrete(labels = c("Many", "Some", 
                              "A few", "None"))+
  labs(
    title = "e)",
    x = "Allow many/few immigrants of different
    race/ethnic group from majority",
    y = NULL,
    color = NULL,
  )+
  scale_y_continuous(
    labels = label_number(accuracy = 0.01))+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))

# partial dependence impcntr
pdp_impcntr <- model_profile(
  total_explainer,
  variables = "impcntr",
  N = NULL
)

# plot
PDP_IMPCNTR <- as_tibble(pdp_impcntr$agr_profiles) %>%
  ggplot(aes(`_x_`, `_yhat_`, fill = `_label_`)) +
  geom_col(width = 0.8 , position = "dodge") +
  scale_fill_manual(values = col, labels = c(
    "Left-wing populist",
    "Valence populist",
    "Right-wing populist"))+
  scale_x_discrete(labels = c("Many", "Some", 
                              "A few", "None"))+
  labs(
    title = "f)",
    x =  "Allow many/few immigrants from
    poorer countries outside Europe",
    y = NULL,
    color = NULL,
  )+
  scale_y_continuous(
    labels = label_number(accuracy = 0.01))+
  theme_light()+
  theme(legend.position = "none",
        legend.title = element_blank(),
        axis.text = element_text (size = 10),
        axis.title = element_text (size = 12))



# combine plots 

# extract legend (run PDP_IMDFETEN once with a legend to extract it)
legend_b <- cowplot::get_legend(PDP_IMDFETN)

# combine plots in grid
PDP_grid <- plot_grid(PDP_LRSCALE + scale_y_continuous(
  labels = label_number(accuracy = 0.01)), PDP_IMUECLT,
  PDP_IMWBCNT, PDP_HMSACLD, 
  PDP_IMDFETN, PDP_IMPCNTR, 
  align = "vh",
  ncol = 2, nrow = 3)

# add legend to combined plot
PDP_grid_2 <- plot_grid(PDP_grid, legend_b, ncol = 1, rel_heights = c(1, 0.04))

# add universal y-axis title
PDP_grid_3 <- grid.arrange(arrangeGrob(PDP_grid_2,
                                       left = textGrob("Predicted probability of vote choice",
                                                       rot = 90, vjust = 1)))
# save plot
ggsave("PDP_RF2.png", 
       plot = PDP_grid_3,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 9,
       dpi = 400)


###############################################################################

# model interpretation with LIME


# extract model from results
final_model <- final_result %>%
  extract_fit_engine()

# turn final model into model type "classification"
final_model_lime <- as_classifier(final_model, labels = NULL)

# ceate lime explainer
expl <- lime(juiced %>% dplyr::select(-vote_pop_1), final_model_lime)

# apply lime algorithm
doParallel::registerDoParallel()
set.seed(901)
explanation <- lime::explain(x = dplyr::select(test, -vote_pop_1), 
                             explainer = expl,
                             n_labels = 3,
                             n_features = 10, 
                             feature_select = "lasso_path")


explanation <- explanation %>%
  mutate(label = recode(label,
                        "1" = "1: Left-wing populist",
                        "2" = "2: Valance populist",
                        "3" = "3: Right-wing populist")) 


# plot full explanation for individual predictors

lime_lrscale <- plot_explanations(explanation %>% dplyr::filter(feature == "lrscale"))+
  labs(title = "a) Left (0) - Right (10) scale")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))


ggsave("lime_lrscale.png", 
       plot = lime_lrscale,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)



lime_euftf <- plot_explanations(explanation %>% dplyr::filter(feature == "euftf"))+
  labs(title = "b) European unification gone too far (0) or go further (10)")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))


ggsave("lime_euftf.png", 
       plot = lime_euftf,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)



lime_imdfetn <- plot_explanations(explanation %>% dplyr::filter(feature == "imdfetn"))+
  labs(title = "c) Allow many (1) / none (4) immigrants of different
    race/ethnic group from majority")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))



ggsave("lime_imdfetn.png", 
       plot = lime_imdfetn,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)


lime_impcntr <- plot_explanations(explanation %>% dplyr::filter(feature == "impcntr"))+
  labs(title = "d) Allow many (1) / none (4) immigrants from
    poorer countries outside Europe")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))



ggsave("lime_impcntr.png", 
       plot = lime_impcntr,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)




lime_imwbcnt <- plot_explanations(explanation %>% dplyr::filter(feature == "imwbcnt"))+
  labs(title = "e) Immigrants make country worse (0) or 
       better (10) place to live")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))



ggsave("lime_imwbcnt.png", 
       plot = lime_imwbcnt,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)


lime_hmsacld <- plot_explanations(explanation %>% dplyr::filter(feature == "hmsacld"))+
  labs(title = "f) Gay couples right to adopt children,
    agree strongly (1) - disagree strongly (5)")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))


ggsave("lime_hmsacld.png", 
       plot = lime_hmsacld,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 4.94,
       dpi = 400,
       limitsize = FALSE)




lime_vote_eu <- plot_explanations(explanation %>% dplyr::filter(feature == "vote_eu"))+
  labs(title = "g) Would vote for country to remain/become member (1)
       of European Union or leave/remain outside (2)")+
  theme(axis.text.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_text (size = 10),
        axis.title.x = element_text(size = 12))



ggsave("lime_vote_eu.png", 
       plot = lime_vote_eu,
       path ="C:/Users/chery/Documents/Uni/Master/Masterarbeit/Graphs",
       width = 8,
       height = 2.47,
       dpi = 400,
       limitsize = FALSE)



