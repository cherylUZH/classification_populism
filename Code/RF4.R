## RF4 ##

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
library(dplyr)

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
set.seed(102)
split <- initial_split(data_imp, strata = vote_pop_1)
train <- training(split)
test <- testing(split)


# build recipe
rf_rec <- recipe(vote_pop_1~., data = train)%>%
  update_role(idno, new_role = "ID") %>%
  themis::step_downsample(vote_pop_1) # upsample data

# prep the data
set.seed(293)
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
set.seed(384)
folds <- vfold_cv(train, v=5, strata = vote_pop_1)


# tune hyperparameters
# set up parallel processing
doParallel::registerDoParallel()
set.seed(475)
tune_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = 10)

# best values
tune_res %>% select_best(metric = "accuracy") #mtry = 18 , min_n 21

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
  min_n(range = c(5,25)),
  levels = 10
)

# tune hyperparameters
# set up parallel processing
doParallel::registerDoParallel()
set.seed(564)
regular_res <- tune_grid(
  tune_workflow,
  resamples = folds,
  grid = rf_grid)

# select the new best hyperparameters based on ROC-AUC
best_auc <- select_best(regular_res, "roc_auc") # mtry = 12, min_n = 16

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
write_rds(final_wf, "~/Uni/Master/Masterarbeit/Results/Final Results/RF4_wf.rds")

# train and evaluate final model

# training on the train set and evaluate on the test set
set.seed(473)
final_result <- final_wf %>%
  last_fit(split) 


# write to disk
write_rds(final_result, "~/Uni/Master/Masterarbeit/Results/Final Results/RF4_result.rds")


###############################################################################

# model evaluation


# plot variable importance
final_result %>%
  extract_fit_parsnip() %>%
  vip(geom = "point", num_features = 10)+
  theme_light()+
  theme(axis.text = element_text (size = 12),
        axis.title = element_text(size = 14))

ggsave("var_imp_RF4.png", 
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
performance_RF4 <- class%>%
  select(c(Sensitivity, Specificity, 
           Precision, F1, "Balanced Accuracy")) %>%
  t()%>%
  as.data.frame() %>%
  rename(
    "Left-wing populist" = "Class: 1",
    "Valance populist" = "Class: 2",
    "Right-wing populist" = "Class: 3")

