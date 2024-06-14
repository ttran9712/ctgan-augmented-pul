library(purrr)
pacman::p_load(tidyverse, gtsummary, PRROC, caret, pROC, Metrics, yardstick)
library(splitTools)
library(h2o)
h2o.init()

source('/Users/ttran/Desktop/courses/STATS C263/functions.R')
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/feats.RData")
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/baseline_data.Rdata")
load(file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/final_df.rda")
load('/Users/ttran/Desktop/courses/STATS C263/reliable_negatives_id.rda')

smote_white_pos = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/one_plus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 1) %>%
  mutate(diabetes = 1) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))
smote_white_neg = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/one_minus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 1) %>%
  mutate(diabetes = 0) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))
smote_black_pos = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/two_plus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 2) %>%
  mutate(diabetes = 1) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))
smote_black_neg = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/two_minus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 2) %>%
  mutate(diabetes = 0) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))
smote_hispanic_pos = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/three_plus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 3) %>%
  mutate(diabetes = 1) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))
smote_hispanic_neg = read.csv('/Users/ttran/Desktop/courses/STATS C263/SMOTE race balanced/three_minus.csv') %>%
  select(-X.1, -X) %>%
  mutate(race_ethnicity = 3) %>%
  mutate(diabetes = 0) %>%
  mutate(education = round(education)) %>%
  mutate(pir = round(pir)) %>%
  mutate(smoke = round(smoke)) %>%
  mutate(alcohol_consumption = round(alcohol_consumption)) %>%
  mutate(MCQ160M = round(MCQ160M)) %>%
  mutate(MCQ300C = round(MCQ300C))

train_step2 = train_step2 %>%
  select(-high_bp) %>%
  mutate(across(all_of(c(continuous_feats_cov)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  rename(diabetes=labels)

library(stringr)


synthetic_lp_rn = rbind(smote_white_pos, smote_white_neg, smote_black_pos,
                        smote_black_neg, smote_hispanic_pos, smote_hispanic_neg) %>%
  mutate(SEQN_new = c(1:nrow(.))) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor) %>%
  rbind(., train_step2) %>%
  mutate(class = case_when(SEQN_new %in% (train_step2 %>% filter(diabetes==1) %>% pull(SEQN_new)) ~ "Labeled positive",
                           SEQN_new %in% reliable_negatives_id ~ "Reliable negative",
                           !str_detect(SEQN_new, "^C")  & diabetes==1 ~ "Synthetic positive",
                           TRUE ~ "Synthetic negative"))

synthetic_lp_rn_famd = FAMD(synthetic_lp_rn %>%
                              column_to_rownames(var = "SEQN_new") %>%
                              select(-SDDSRVYR, -diabetes, -class), 
                            graph = FALSE, ncp = 50)

synthetic_lp_rn_famd_df = data.frame(SEQN_new = synthetic_lp_rn_famd$ind$coord %>% row.names(.),
                                     class = synthetic_lp_rn %>% 
                                       filter(SEQN_new %in% (synthetic_lp_rn_famd$ind$coord %>% row.names(.))) %>%
                                       pull(class),
                                     PC1 = synthetic_lp_rn_famd$ind$coord[,1],
                                     PC2 = synthetic_lp_rn_famd$ind$coord[,2])

p = ggplot(synthetic_lp_rn_famd_df, aes(PC1, PC2, color = class)) + 
  geom_point(size=0.5, alpha=1) +
  scale_color_manual(values = c("red", "blue", "skyblue", "orange")) +
  labs(title = "Synthetic training samples generated using SMOTE") + 
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.title = element_text(size = 14),  # Increase axis title font size
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 14),  # Increase legend title font size
        legend.text = element_text(size = 12))

legend <- get_legend(p)
# Remove the legend from the base plot
p <- p + theme(legend.position = "none")
p_with_density = ggMarginal(p, type = "density", groupColour = TRUE, groupFill = TRUE)
final_plot <- plot_grid(p_with_density, legend, ncol = 2, rel_widths = c(0.8, 0.2))
# Print the final combined plot
print(final_plot)

# train step 2 w/ synthetic data
train_step2 = synthetic_lp_rn %>%
  select(-class) %>%
  #mutate_at(vars(all_of(c(discrete_vars_step2, discrete_vars_cov))), as.factor) %>%
  rename(labels = diabetes)

seed_num = 20224766
train_step2$labels = as.factor(train_step2$labels)
predictors = colnames(train_step2)[-c(1:3)]
response = 'labels'
aml.balance = h2o.automl(x = predictors, y = response, training_frame = as.h2o(train_step2), 
                         balance_classes = FALSE, max_models = 10, nfolds = 5, 
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

val_h2o = val_h2o %>%
  select(-high_bp)
step2_models_auc = get_auc_models(val_h2o)[[2]]
step2_models_aucpr = get_auc_models(val_h2o)[[1]]

if(which.max(unlist(step2_models_aucpr))==1){
  best_model = best_glm
} else if(which.max(unlist(step2_models_aucpr))==2){
  best_model = best_gbm
} else if(which.max(unlist(step2_models_aucpr))==2){
  best_model = best_xgb
} else{
  best_model = best_drf
}


# find max mcc threshold using ground truth labels
mcc_th = get_maxmcc()[[1]]

test_norm_step2 = test_norm_step2 %>% select(-high_bp)
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
