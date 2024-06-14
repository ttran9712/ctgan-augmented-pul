library(purrr)
pacman::p_load(tidyverse, gtsummary, PRROC, caret, pROC, Metrics, yardstick)
library(splitTools)
library(h2o)
h2o.init()

source('/Users/ttran/Desktop/courses/STATS C263/functions.R')
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/feats.RData")
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/baseline_data.Rdata")
load(file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/final_df.rda")

### 1. AutoML - naive supervised baseline
seed_num = 20224766
train_step1 = train_step1 %>%
  rename(diabetes=labels) %>%
  select(c(1:13), all_of(sig_feats_step2)) %>%
  rename(labels = diabetes)
train_step1$labels = as.factor(train_step1$labels)
train_step1_h2o = as.h2o(train_step1)
predictors = colnames(train_step1)[-c(1:3)]
response = 'labels'
aml.balance = h2o.automl(x = predictors, y = response, training_frame = train_step1_h2o, 
                         balance_classes = TRUE, max_models = 10, nfolds = 5, 
                         seed = seed_num, sort_metric = "AUCPR",
                         exclude_algos = c("DeepLearning", "StackedEnsemble"))
lb.balance = h2o.get_leaderboard(aml.balance, extra_columns = "ALL")
# Print all rows (instead of default 6 rows)
print(lb.balance, n = nrow(lb.balance))

# Get the "best" for each type of model
best_glm_id = as.character(as.data.frame(lb.balance$model_id[grep("GLM", as.vector(lb.balance$model_id))[1]]))
best_gbm_id = as.character(as.data.frame(lb.balance$model_id[grep("GBM", as.vector(lb.balance$model_id))[1]]))
best_xgb_id = as.character(as.data.frame(lb.balance$model_id[grep("XGBoost", as.vector(lb.balance$model_id))[1]]))
best_drf_id = as.character(as.data.frame(lb.balance$model_id[grep("DRF", as.vector(lb.balance$model_id))[1]]))

best_glm = h2o.getModel(best_glm_id) 
best_gbm = h2o.getModel(best_gbm_id)
best_xgb = h2o.getModel(best_xgb_id) 
best_drf = h2o.getModel(best_drf_id)

val_h2o = val_norm %>% rename(labels = diabetes)
val_h2o$labels = as.factor(val_h2o$labels)

step1_models_auc = get_auc_models(val_h2o)[[2]]
step1_models_aucpr = get_auc_models(val_h2o)[[1]]

if(which.max(unlist(step1_models_aucpr))==1){
  best_model = best_glm
} else if(which.max(unlist(step1_models_aucpr))==2){
  best_model = best_gbm
} else if(which.max(unlist(step1_models_aucpr))==3){
  best_model = best_xgb
} else{
  best_model = best_drf
}

# find max mcc threshold using ground truth labels
mcc_th = get_maxmcc()[[1]]

pred_test = h2o.predict(best_model, as.h2o(test_norm_step2))
pred_test_tbl = cbind(test_norm_step2, pred_test %>% as.data.frame()) %>%
  select(-predict, -diabetes) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
  rename(GT = diabetes) %>%
  mutate(predict_label = if_else(p1 >= mcc_th, 1, 0))

test_metrics_supervised = get_metrics_df(pred_test_tbl) # models are predicting the same white patients to have diabetes
auc_supervised = get_auc_df(pred_test_tbl)
brier_supervised = get_brier(pred_test_tbl %>% mutate(GT = as.numeric(as.character(GT))))

bw_diff_supervised = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "black")
hw_diff_supervised = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "hispanic")