library(purrr)
pacman::p_load(tidyverse, gtsummary, PRROC, caret, pROC, Metrics, yardstick)
library(splitTools)
library(h2o)
h2o.init()

lp_perc = 0.25
load(file = paste0("/Users/ttran/Desktop/courses/STATS C263/clean_data/train_allfeats_", lp_perc, ".rda"))
load(file = paste0("/Users/ttran/Desktop/courses/STATS C263/clean_data/val_allfeats_", lp_perc, ".rda"))
load(file = paste0("/Users/ttran/Desktop/courses/STATS C263/clean_data/test_allfeats_", lp_perc, ".rda"))
load(file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/final_df.rda")


source('/Users/ttran/Desktop/courses/STATS C263/functions.R')

# continuous features mix max scaling
continuous_feats = c()
for(i in colnames(train)){
  if(!i %in% c("SEQN_new", "SDDSRVYR") & class(train[[i]]) != "factor"){
    continuous_feats = c(continuous_feats, i)
  }
}

continuous_feats_min = list()
continuous_feats_max = list()
for(i in continuous_feats){
  continuous_feats_min[[i]] = min(train[[i]], na.rm = T)
  continuous_feats_max[[i]] = max(train[[i]], na.rm = T)
}

# Function to perform min-max scaling with predefined min and max values
min_max_scale = function(x, col_name, min_list, max_list) {
  (x - min_list[[col_name]]) / (max_list[[col_name]] - min_list[[col_name]])
}

# Apply the function to each column in continuous_feats using across
train_norm = train %>%
  mutate(across(all_of(continuous_feats), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max)))
discrete_vars = c()
for(i in colnames(train_norm)){
  if((train_norm[[i]] %>% unique() %>% length()) < 10){
    discrete_vars = c(discrete_vars, i)
  }
}
discrete_vars_cov = discrete_vars[1:10]
# validation patients
val_norm = val %>% 
  mutate(across(all_of(continuous_feats), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max)))

# test patients
test_norm = test %>% 
  mutate(across(all_of(continuous_feats), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max)))

### 1. AutoML - step 1
seed_num = 20224766
train_step1 = train_norm %>% rename(labels = diabetes)
train_step1$labels = as.factor(train_step1$labels)
train_step1_h2o = as.h2o(train_step1)
predictors = colnames(train)[-c(1:3)]
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

# Benchmark (majority) train
pred_major_tbl = train_step1 %>% select(SEQN_new, labels) %>% cbind(., rep(0, nrow(train_step1)))
names(pred_major_tbl) = c("SEQN_new,", "train_label", "p1")
pred_major_tbl = pred_major_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label])
pr_major = pr.curve(scores.class0 = pred_major_tbl$p1, weights.class0 = pred_major_tbl$train_label)
# AUCPR
pr_major$auc.integral
roc_major = roc(pred_major_tbl$train_label, pred_major_tbl$p1)
pROC::auc(roc_major)

# Benchmark (majority) val
pred_major_tbl = val_h2o %>% select(SEQN_new, labels) %>% cbind(., rep(0, nrow(val_h2o)))
names(pred_major_tbl) = c("PatientID", "val_label", "p1")
pred_major_tbl = pred_major_tbl %>% 
  mutate(val_label = as.numeric(levels(val_label))[val_label])
pr_major = pr.curve(scores.class0 = pred_major_tbl$p1, weights.class0 = pred_major_tbl$val_label)
# AUCPR
pr_major$auc.integral
roc_major = roc(pred_major_tbl$val_label, pred_major_tbl$p1)
pROC::auc(roc_major)

step1_models_auc = get_auc_models(val_h2o)[[2]]
step1_models_aucpr = get_auc_models(val_h2o)[[1]]

### 2. Best step 1 model is GBM (both on train and validation sets), but GLM works best for identifying reliable negatives
if(which.max(unlist(step1_models_aucpr))==1){
  best_model_val = best_glm
} else if(which.max(unlist(step1_models_aucpr))==2){
  best_model_val = best_gbm
} else if(which.max(unlist(step1_models_aucpr))==2){
  best_model_val = best_xgb
} else{
  best_model_val = best_drf
}

best_model = best_glm
h2o.saveModel(best_model_val, path = '/Users/ttran/Desktop/courses/STATS C263/models/supervised/')
h2o.saveModel(best_model, path = '/Users/ttran/Desktop/courses/STATS C263/models/supervised/')
# Load model
best_model = h2o.loadModel(path = "/Users/ttran/Desktop/courses/STATS C263/models/supervised/GLM_1_AutoML_1_20240608_195816")
best_model_val = h2o.loadModel(path = "/Users/ttran/Desktop/courses/STATS C263/models/supervised/GBM_1_AutoML_1_20240608_195816")

# identify reliable negatives
#### 3. Get predicted probability
pred_glm = h2o.predict(best_model, train_step1_h2o, standardize = T)
pred_glm_tbl = cbind(train_step1, pred_glm %>% as.data.frame()) %>%
  mutate(delta_p = p1 - p0)

# get probabilistic gap
prob_gap = pred_glm_tbl %>% filter(labels == 1) %>% pull(delta_p) 
# Sample the vector 1000 times, calculate the minimum of each sample, and then calculate the mean of the minimums
set.seed(1347)  # for reproducibility
mean_min = mean(replicate(1000, min(sample(prob_gap, size = 500, replace = TRUE))))
print(mean_min) # -0.9908203

reliable_negatives_id = pred_glm_tbl %>% filter(labels == 0) %>% 
  filter(delta_p < mean_min) %>% 
  pull(SEQN_new)
length(reliable_negatives_id) # N = 1214
save(reliable_negatives_id, file = '/Users/ttran/Desktop/courses/STATS C263/reliable_negatives_id.rda')
load('/Users/ttran/Desktop/courses/STATS C263/reliable_negatives_id.rda')
writeLines(reliable_negatives_id, "/Users/ttran/Desktop/courses/STATS C263/reliable_negatives_id.txt")

# find max mcc threshold using ground truth labels
best_model = best_model_val
pred_val = h2o.predict(best_model , as.h2o(val_h2o))
mcc_th = get_maxmcc()[[1]]

# evaluate naive supervised model using mcc threshold
pred_test = h2o.predict(best_model_val, as.h2o(test_norm))
pred_test_tbl = cbind(test_norm, pred_test %>% as.data.frame()) %>%
  select(-predict, -diabetes) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
  rename(GT = diabetes) %>%
  mutate(predict_label = if_else(p1 >= mcc_th, 1, 0))

test_metrics_supervised = get_metrics_df(pred_test_tbl)
auc_supervised = get_auc_df(pred_test_tbl)
brier_supervised = get_brier(pred_test_tbl %>% mutate(GT = as.numeric(as.character(GT))))

# fairness
bw_diff_supervised = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "black")
hw_diff_supervised = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "hispanic")

#### step 2
train_step2_raw = train %>%
  filter(diabetes==1 | SEQN_new %in% reliable_negatives_id) 

# feature selection prior to step 2
p = list()

# feature selection
for (i in colnames(train_step2_raw)[14:length(colnames(train_step2_raw))]) {
  f1 = as.formula(paste('diabetes ~ ', i,  
                        ' + age + female + race_ethnicity + education + us_born + pir + 
                        smoke + alcohol_consumption + cholesterol_total + high_bp'))
  logi_fit = glm(f1, data = train_step2_raw, family = binomial())
  p[[i]] = coef(summary(logi_fit))[2,4]
}

sig_feats_step2 = names(Filter(function(x) x <= 0.05/197, p))

continuous_feats_step2 = intersect(sig_feats_step2, continuous_feats)
discrete_vars_step2 = intersect(sig_feats_step2, discrete_vars)

save(sig_feats_step2, continuous_feats, continuous_feats_step2, discrete_vars, discrete_vars_cov, discrete_vars_step2, 
     file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/feats.RData")
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/feats.RData")

train_step2 = train_step2_raw %>%
  select(c(1:13), all_of(sig_feats_step2)) %>%
  mutate(across(all_of(continuous_feats_step2), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars("SDDSRVYR", "diabetes", all_of(discrete_vars_step2)), as.factor) %>%
  rename(labels = diabetes)

seed_num = 20224766
train_step2$labels = as.factor(train_step2$labels)
predictors = colnames(train_step2)[-c(1:3)]
response = 'labels'
aml.balance = h2o.automl(x = predictors, y = response, training_frame = as.h2o(train_step2), 
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

# Benchmark (majority) train
pred_major_tbl = train_step2 %>% select(SEQN_new, labels) %>% cbind(., rep(0, nrow(train_step2)))
names(pred_major_tbl) = c("SEQN_new,", "train_label", "p1")
pred_major_tbl = pred_major_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label])
pr_major = pr.curve(scores.class0 = pred_major_tbl$p1, weights.class0 = pred_major_tbl$train_label)
# AUCPR
pr_major$auc.integral
roc_major = roc(pred_major_tbl$train_label, pred_major_tbl$p1)
pROC::auc(roc_major)

val_h2o = val_norm %>% 
  select(c(1:13), all_of(sig_feats_step2)) %>%
  rename(labels=diabetes)
val_h2o$labels = as.factor(val_h2o$labels)

step2_models_auc = get_auc_models(val_h2o)[[2]]
step2_models_aucpr = get_auc_models(val_h2o)[[1]]

if(which.max(unlist(step2_models_aucpr))==1){
  best_model = best_glm
} else if(which.max(unlist(step2_models_aucpr))==2){
  best_model = best_gbm
} else if(which.max(unlist(step2_models_aucpr))==3){
  best_model = best_xgb
} else{
  best_model = best_drf
}

### 4. Selected model - GBM (best aucpr on val set)
best_gbm@model$variable_importances$variable[1:20]
h2o.saveModel(best_gbm, path = '/Users/ttran/Desktop/courses/STATS C263/models/PUL/step2/')
# Load model
best_model = h2o.loadModel(path = "/Users/ttran/Desktop/courses/STATS C263/models/PUL/step2/GBM_1_AutoML_2_20240607_171539")

# find max mcc threshold using ground truth labels
pred_val = h2o.predict(best_model , as.h2o(val_h2o))
mcc_th = get_maxmcc()[[1]]

test_norm_step2 = test_norm %>%
  select(c(1:13), all_of(sig_feats_step2))

pred_test = h2o.predict(best_model, as.h2o(test_norm_step2))
pred_test_tbl = cbind(test_norm_step2, pred_test %>% as.data.frame()) %>%
  select(-predict, -diabetes) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
  rename(GT = diabetes) %>%
  mutate(predict_label = if_else(p1 >= mcc_th, 1, 0))

test_metrics_pul = get_metrics_df(pred_test_tbl) # models are predicting the same white patients to have diabetes
auc_pul = get_auc_df(pred_test_tbl)
brier_pul = get_brier(pred_test_tbl %>% mutate(GT = as.numeric(as.character(GT))))

# fairness
bw_diff_pul = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "black")
hw_diff_pul = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "hispanic")

save(train_norm, val_norm, val_h2o, test_norm, train_step1, train_step2, test_norm_step2,
     file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/baseline_data.Rdata")
