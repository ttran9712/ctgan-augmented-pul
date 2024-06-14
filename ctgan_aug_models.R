library(purrr)
pacman::p_load(tidyverse, gtsummary, PRROC, caret, pROC, Metrics, yardstick)
library(splitTools)
library(h2o)
h2o.init()

# visualize top 2 PCs of reliable negatives vs. labeled positives
library(FactoMineR)
library(factoextra)
library(gridExtra)
library(grid)

source('/Users/ttran/Desktop/courses/STATS C263/functions.R')
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/feats.RData")
load("/Users/ttran/Desktop/courses/STATS C263/clean_data/baseline_data.Rdata")
load('/Users/ttran/Desktop/courses/STATS C263/reliable_negatives_id.rda')
# read in synthetic data
synthetic_data = read.csv('/Users/ttran/Desktop/courses/STATS C263/CTGAN_synthetic_data_updatedfeats_lprn.csv')
# synthetic data obtained via conditional sampling
# synthetic_data = read.csv('/Users/ttran/Desktop/courses/STATS C263/CTGAN_synthetic_train_sigfeats_corrected.csv')
synthetic_data_additional = read.csv('/Users/ttran/Desktop/courses/STATS C263/CTGAN_oversample.csv')
# get up to white number of samples for negative samples
# .
# 1    2    3 
# 2667  711 1146 

num_wp_sample = 1000 - (train_norm %>% filter(race_ethnicity==1 & diabetes==1) %>% nrow())
num_bp_sample = 1000 - (train_norm %>% filter(race_ethnicity==2 & diabetes==1) %>% nrow())
num_hp_sample = 1000 - (train_norm %>% filter(race_ethnicity==3 & diabetes==1) %>% nrow())
num_wn_sample = 1000 - (train_norm %>% filter(SEQN_new %in% reliable_negatives_id) %>%
                          filter(race_ethnicity==1) %>% nrow())
num_bn_sample = 1000 - (train_norm %>% filter(SEQN_new %in% reliable_negatives_id) %>%
                          filter(race_ethnicity==2) %>% nrow())
num_hn_sample = 1000 - (train_norm %>% filter(SEQN_new %in% reliable_negatives_id) %>%
                          filter(race_ethnicity==3) %>% nrow())
set.seed(1)
wp_synthetic = synthetic_data %>%
  filter(race_ethnicity==1) %>%
  filter(diabetes==1) %>%
  sample_n(num_wp_sample)
set.seed(1)
bp_synthetic = synthetic_data %>%
  filter(race_ethnicity==2) %>%
  filter(diabetes==1) %>%
  sample_n(num_bp_sample)
set.seed(1)
hp_synthetic = synthetic_data %>%
  filter(race_ethnicity==3) %>%
  filter(diabetes==1) %>%
  sample_n(num_hp_sample)
set.seed(1)
wn_synthetic = synthetic_data %>%
  filter(race_ethnicity==1 & diabetes==0) %>%
  sample_n(num_wn_sample)
set.seed(1)
bn_synthetic = synthetic_data %>%
  filter(race_ethnicity==2 & diabetes==0) %>%
  sample_n(num_bn_sample)
set.seed(1)
hn_synthetic = synthetic_data %>%
  filter(race_ethnicity==3 & diabetes==0) %>%
  sample_n(num_hn_sample)

set.seed(1)
wp_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==1) %>%
  filter(diabetes==1) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)
set.seed(1)
bp_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==2) %>%
  filter(diabetes==1) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)
set.seed(1)
hp_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==3) %>%
  filter(diabetes==1) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)
set.seed(1)
wn_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==1 & diabetes==0) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)
set.seed(1)
bn_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==2 & diabetes==0) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)
set.seed(1)
hn_synthetic_add = synthetic_data_additional %>%
  filter(race_ethnicity==3 & diabetes==0) %>%
  sample_n(10000) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)


synthetic_combined = rbind(wp_synthetic, bp_synthetic, hp_synthetic,
                           wn_synthetic, bn_synthetic, hn_synthetic) %>%
  mutate(across(all_of(c(continuous_feats_cov, continuous_feats_step2)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max))) %>%
  mutate_at(vars(all_of(c(discrete_vars_cov, discrete_vars_step2))), as.factor)

train_step2 = train_step2 %>%
  mutate(across(all_of(c(continuous_feats_cov)), ~ min_max_scale(.x, cur_column(), continuous_feats_min, continuous_feats_max)))

synthetic_lp_rn = rbind(synthetic_combined, train_step2 %>% rename(diabetes=labels)) %>%
  mutate(class = case_when(SEQN_new %in% c(wp_synthetic$SEQN_new, bp_synthetic$SEQN_new, hp_synthetic$SEQN_new) ~ "Synthetic positive",
                           SEQN_new %in% c(wn_synthetic$SEQN_new, bn_synthetic$SEQN_new, hn_synthetic$SEQN_new) ~ "Synthetic negative",
                           SEQN_new %in% reliable_negatives_id ~ "Reliable negative",
                           TRUE ~ "Labeled positive"))

synthetic_lp_rn_famd = FAMD(synthetic_lp_rn %>%
                              column_to_rownames(var = "SEQN_new") %>%
                              select(-SDDSRVYR, -diabetes, -class), 
                            graph = FALSE, ncp = 50)

perc_var = synthetic_lp_rn_famd$eig %>% as.data.frame() %>%
  select("percentage of variance") %>%
  rename("perc_var" = "percentage of variance") %>%
  mutate(PC = c(1:46))
ggplot(perc_var, aes(x = PC, y = perc_var)) +
  geom_line() +
  geom_point() +
  labs(title = "Scree plot", x = "PC", y = "Perc. var. explained") +
  theme_minimal()  
  
synthetic_lp_rn_famd_df = data.frame(SEQN_new = synthetic_lp_rn_famd$ind$coord %>% row.names(.),
                                     class = synthetic_lp_rn %>% 
                                       filter(SEQN_new %in% (synthetic_lp_rn_famd$ind$coord %>% row.names(.))) %>%
                                       pull(class),
                                     PC1 = synthetic_lp_rn_famd$ind$coord[,1],
                                     PC2 = synthetic_lp_rn_famd$ind$coord[,2])

p = ggplot(synthetic_lp_rn_famd_df, aes(PC1, PC2, color = class)) + 
  geom_point(size=0.5, alpha=1) +
  scale_color_manual(values = c("red", "blue", "skyblue", "orange")) +
  labs(title = "Before Adding Filtered Additional Samples (N = 6,000)") + 
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

# add additional filtered synthetic samples
lp_underrep = synthetic_lp_rn_famd_df %>%
  filter(class=="Labeled positive") %>%
  filter(PC1 > -0.3) %>%
  pull(SEQN_new)
rn_underrep = synthetic_lp_rn_famd_df %>%
  filter(class=="Reliable negative") %>%
  filter(PC2 > 2.5) %>%
  pull(SEQN_new)

get_similar_underrep_points = function(synthetic_df, underrep_class){
  if(underrep_class=="lp"){
    nrow = 30
    underrep_ids = lp_underrep
  } else{ # RN
    nrow = 138 
    underrep_ids = rn_underrep
  }

  combined_df = rbind(train_step2 %>% filter(SEQN_new %in% underrep_ids) %>%
                        select(-c(1:3, 6)), synthetic_df %>% select(-c(1:3, 6)))
  combined_famd = FAMD(combined_df, 
                       graph = FALSE, ncp = 50)
  combined_df = combined_famd$ind$coord[,c(1:20)]
  dist_mat = dist(combined_df) %>% as.matrix()
  nn = apply(dist_mat[1:nrow, (nrow+1):nrow(combined_df)], 1, which.min)
  nn2 <- (nrow+1):nrow(combined_df)
  nn_idx <- nn2[nn]
  dist_th = max(sapply(1:nrow, function(i) dist_mat[i, nn_idx[i]]))
  similar_points_list <- lapply(1:nrow, function(i) {
    which(dist_mat[i, ] <= dist_th)})
  similar_points_indices <- unique(unlist(similar_points_list))
  similar_points <- synthetic_df[similar_points_indices, ]
  return(similar_points)
}

set.seed(1)
wp_add_underrep = get_similar_underrep_points(wp_synthetic_add, "lp") %>%
  na.omit() %>%
  sample_n(1000)
set.seed(1)
bp_add_underrep = get_similar_underrep_points(bp_synthetic_add, "lp") %>%
  na.omit() %>%
  sample_n(1000)
set.seed(1)
hp_add_underrep = get_similar_underrep_points(hp_synthetic_add, "lp") %>%
  na.omit() %>%
  sample_n(1000)
set.seed(1)
wn_add_underrep = get_similar_underrep_points(wn_synthetic_add, "rn") %>%
  na.omit() %>%
  sample_n(1000)
set.seed(1)
bn_add_underrep = get_similar_underrep_points(bn_synthetic_add, "rn") %>%
  na.omit() %>%
  sample_n(1000)
set.seed(1)
hn_add_underrep = get_similar_underrep_points(hn_synthetic_add, "rn") %>%
  na.omit() %>%
  sample_n(1000)

# resample
synthetic_add_combined = rbind(wp_add_underrep,
                               bp_add_underrep,
                               hp_add_underrep,
                               wn_add_underrep,
                               bn_add_underrep,
                               hn_add_underrep) %>%
  mutate(class = if_else(diabetes==0, "Synthetic negative", "Synthetic positive"))

synthetic_lp_rn = rbind(synthetic_lp_rn, synthetic_add_combined)
row.names(synthetic_lp_rn) = NULL
counter <- 0
replace_with_unique <- function(x) {
  if (grepl("^sdv", x)) {
    counter <<- counter + 1
    paste0("sdv_", counter)
  } else {
    x
  }
}

synthetic_lp_rn = synthetic_lp_rn %>%
  rowwise() %>%
  mutate(SEQN_new = replace_with_unique(SEQN_new)) %>%
  ungroup()

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
library(ggExtra)
library(cowplot)

p = ggplot(synthetic_lp_rn_famd_df, aes(PC1, PC2, color = class)) + 
  geom_point(size=0.5, alpha=0.5) +
  scale_color_manual(values = c("red", "blue", "skyblue", "orange")) +
  labs(title = "After Adding Filtered Additional Samples (N = 12,000)") + 
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

# compare feature space with unlabeled positives
train_up = train_step1 %>%
  filter(labels==0) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes) %>% rename(GT=diabetes)) %>%
  filter(!SEQN_new %in% reliable_negatives_id) %>%
  filter(GT==1) %>%
  select(-GT) %>%
  mutate(class = "Unlabeled positive") %>%
  rename(diabetes=labels)

synthetic_pos_famd = FAMD(rbind(train_up, synthetic_lp_rn %>%
                                     filter(diabetes==1)) %>%
                               column_to_rownames(var = "SEQN_new") %>%
                               select(-SDDSRVYR, -diabetes, -class), 
                             graph = FALSE, ncp = 50)

synthetic_pos_famd_df = data.frame(SEQN_new = synthetic_pos_famd$ind$coord %>% row.names(.),
                                     class = rbind(train_up, synthetic_lp_rn %>%
                                                     filter(diabetes==1)) %>% 
                                       filter(SEQN_new %in% (synthetic_pos_famd$ind$coord %>% row.names(.))) %>%
                                       pull(class),
                                     PC1 = synthetic_pos_famd$ind$coord[,1],
                                     PC2 = synthetic_pos_famd$ind$coord[,2])

p = ggplot(synthetic_pos_famd_df, aes(PC1, PC2, color = class)) + 
  geom_point(size=0.5, alpha=0.5) +
  scale_color_manual(values = c("red", "orange", "darkgreen")) +
  labs(title = "Feature space of all real positive\nand synthetic positive samples") + 
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
# step 3
# evaluate on train set
# GLM
train_step1 = train_step1 %>%
  rename(diabetes=labels) %>%
  select(c(1:13), all_of(sig_feats_step2)) %>%
  rename(labels = diabetes)

pred_glm = h2o.predict(best_glm, as.h2o(train_step1), standardize = T)
pred_glm_tbl = cbind(train_step1$SEQN_new, pred_glm %>% as.data.frame(), train_step1$labels) %>%
  rename('SEQN_new' = 'train_step1$SEQN_new', 'train_label' = 'train_step1$labels')
pred_glm_tbl = pred_glm_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label]) %>%
  inner_join(., train_norm %>% select(SEQN_new, race_ethnicity))
pr_glm = pr.curve(scores.class0 = pred_glm_tbl$p1, weights.class0 = pred_glm_tbl$train_label)
# AUCPR
pr_glm$auc.integral
roc_glm = roc(pred_glm_tbl$train_label, pred_glm_tbl$p1)
pROC::auc(roc_glm)

# GBM
pred_gbm = h2o.predict(best_gbm, as.h2o(train_step1), standardize = T)
pred_gbm_tbl = cbind(train_step1$SEQN_new, pred_gbm %>% as.data.frame(), train_step1$labels) %>%
  rename('SEQN_new' = 'train_step1$SEQN_new', 'train_label' = 'train_step1$labels')
pred_gbm_tbl = pred_gbm_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label]) %>%
  inner_join(., train_norm %>% select(SEQN_new, race_ethnicity))
pr_gbm = pr.curve(scores.class0 = pred_gbm_tbl$p1, weights.class0 = pred_gbm_tbl$train_label)
# AUCPR
pr_gbm$auc.integral
roc_gbm = roc(pred_gbm_tbl$train_label, pred_gbm_tbl$p1)
pROC::auc(roc_gbm)

# xgb
pred_xgb = h2o.predict(best_xgb, as.h2o(train_step1), standardize = T)
pred_xgb_tbl = cbind(train_step1$SEQN_new, pred_xgb %>% as.data.frame(), train_step1$labels) %>%
  rename('SEQN_new' = 'train_step1$SEQN_new', 'train_label' = 'train_step1$labels')
pred_xgb_tbl = pred_xgb_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label]) %>%
  inner_join(., train_norm %>% select(SEQN_new, race_ethnicity))
pr_xgb = pr.curve(scores.class0 = pred_xgb_tbl$p1, weights.class0 = pred_xgb_tbl$train_label)
# AUCPR
pr_xgb$auc.integral
roc_xgb = roc(pred_xgb_tbl$train_label, pred_xgb_tbl$p1)
pROC::auc(roc_xgb)

# drf
pred_drf = h2o.predict(best_drf, as.h2o(train_step1), standardize = T)
pred_drf_tbl = cbind(train_step1$SEQN_new, pred_drf %>% as.data.frame(), train_step1$labels) %>%
  rename('SEQN_new' = 'train_step1$SEQN_new', 'train_label' = 'train_step1$labels')
pred_drf_tbl = pred_drf_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label]) %>%
  inner_join(., train_norm %>% select(SEQN_new, race_ethnicity))
pr_drf = pr.curve(scores.class0 = pred_drf_tbl$p1, weights.class0 = pred_drf_tbl$train_label)
# AUCPR
pr_drf$auc.integral
roc_drf = roc(pred_drf_tbl$train_label, pred_drf_tbl$p1)
pROC::auc(roc_drf)

min_p1_lp_white = min(pred_drf_tbl %>% filter(race_ethnicity==1) %>% filter(train_label==1) %>% pull(p1))
min_p1_lp_black = min(pred_drf_tbl %>% filter(race_ethnicity==2) %>% filter(train_label==1) %>% pull(p1))
min_p1_lp_hispanic = min(pred_drf_tbl %>% filter(race_ethnicity==3) %>% filter(train_label==1) %>% pull(p1))

min_p1_lp_white = min(pred_gbm_tbl %>% filter(race_ethnicity==1) %>% filter(train_label==1) %>% pull(p1))
min_p1_lp_black = min(pred_gbm_tbl %>% filter(race_ethnicity==2) %>% filter(train_label==1) %>% pull(p1))
min_p1_lp_hispanic = min(pred_gbm_tbl %>% filter(race_ethnicity==3) %>% filter(train_label==1) %>% pull(p1))

max_p1_rn_white = max(pred_drf_tbl %>% filter(race_ethnicity==1) %>% filter(SEQN_new %in% reliable_negatives_id) %>% pull(p1))
max_p1_rn_black = max(pred_drf_tbl %>% filter(race_ethnicity==2) %>% filter(SEQN_new %in% reliable_negatives_id) %>% pull(p1))
max_p1_rn_hispanic = max(pred_drf_tbl %>% filter(race_ethnicity==3) %>% filter(SEQN_new %in% reliable_negatives_id) %>% pull(p1))

# ap = pred_drf_tbl %>% filter(train_label==0) %>% filter(p1 >= min_p1_lp) %>% arrange(desc(p1)) %>% 
#   slice_head(n=668) %>% pull(SEQN_new)
ap_white = pred_drf_tbl %>% filter(race_ethnicity==1) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>% filter(p1 >= min_p1_lp_white) %>% arrange(desc(p1)) %>% 
  slice_head(n=290) %>% pull(SEQN_new) # add 290 to get to overall white class prior (8.5%) 
ap_black = pred_drf_tbl %>% filter(race_ethnicity==2) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>% filter(p1 >= min_p1_lp_black) %>% arrange(desc(p1)) %>% 
  slice_head(n=181) %>% pull(SEQN_new) # add 181 to get to overall black class prior (13.3%) 
ap_hispanic = pred_drf_tbl %>% filter(race_ethnicity==3) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>% filter(p1 >= min_p1_lp_hispanic) %>% arrange(desc(p1)) %>% 
  slice_head(n=197) %>% pull(SEQN_new) # add 197 to get to overall hispanic class prior (11.6%) 
ap_combined = c(ap_white, ap_black, ap_hispanic)

# an = pred_drf_tbl %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>%
#   filter(p1 < max_p1_rn) %>% pull(SEQN_new) 
# length(an) # 2799
an_white = pred_drf_tbl %>% filter(race_ethnicity==1) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>%
  filter(p1 < max_p1_rn_white) %>% pull(SEQN_new) 
an_black = pred_drf_tbl %>% filter(race_ethnicity==2) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>%
  filter(p1 < max_p1_rn_black) %>% pull(SEQN_new) 
an_hispanic = pred_drf_tbl %>% filter(race_ethnicity==3) %>% filter(train_label==0 & !SEQN_new %in% reliable_negatives_id) %>%
  filter(p1 < max_p1_rn_hispanic) %>% pull(SEQN_new) 
an_combined = c(an_white, an_black, an_hispanic)
an_rn_combined = c(an_white, an_black, an_hispanic, reliable_negatives_id)
writeLines(an_rn_combined, "/Users/ttran/Desktop/courses/STATS C263/an_rn_id.txt")

writeLines(sig_feats_step2, "/Users/ttran/Desktop/courses/STATS C263/sig_feats_lprn.txt")

# AP FPs are distinct from RNs, but very similar to LPs feature-wise. different feats?
final_df %>% filter(SEQN_new %in% ap_white) %>% pull(diabetes) %>% table() # only getting ~half of AP correct
final_df %>% filter(SEQN_new %in% ap_black) %>% pull(diabetes) %>% table() # only getting ~half of AP correct
final_df %>% filter(SEQN_new %in% ap_hispanic) %>% pull(diabetes) %>% table() # only getting ~half of AP correct

final_df %>% filter(SEQN_new %in% an_white) %>% pull(diabetes) %>% table() 
final_df %>% filter(SEQN_new %in% an_black) %>% pull(diabetes) %>% table() 
final_df %>% filter(SEQN_new %in% an_hispanic) %>% pull(diabetes) %>% table() 

train_step3 = train_norm %>%
  filter(diabetes==1 | SEQN_new %in% reliable_negatives_id |
           SEQN_new %in% ap_combined | SEQN_new %in% an_combined) %>%
  mutate(diabetes = if_else(SEQN_new %in% an_combined | SEQN_new %in% reliable_negatives_id, 0, 1)) %>%
  select(c(1:13), all_of(sig_feats_step2)) %>%
  rename(labels = diabetes)
seed_num = 20224766
train_step3$labels = as.factor(train_step3$labels)
predictors = colnames(train_step3)[-c(1:3)]
response = 'labels'
aml.balance = h2o.automl(x = predictors, y = response, training_frame = as.h2o(train_step3), 
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
pred_major_tbl = train_step3 %>% select(SEQN_new, labels) %>% cbind(., rep(0, nrow(train_step3)))
names(pred_major_tbl) = c("SEQN_new,", "train_label", "p1")
pred_major_tbl = pred_major_tbl %>% 
  mutate(train_label = as.numeric(levels(train_label))[train_label])
pr_major = pr.curve(scores.class0 = pred_major_tbl$p1, weights.class0 = pred_major_tbl$train_label)
# AUCPR
pr_major$auc.integral
roc_major = roc(pred_major_tbl$train_label, pred_major_tbl$p1)
pROC::auc(roc_major)

# GLM
pred_glm = h2o.predict(best_glm, as.h2o(val_h2o), standardize = T)
pred_glm_tbl = cbind(val_h2o$SEQN_new, pred_glm %>% as.data.frame(), val_h2o$labels) %>%
  rename('SEQN_new' = 'val_h2o$SEQN_new', 'val_label' = 'val_h2o$labels')
pred_glm_tbl = pred_glm_tbl %>% 
  mutate(val_label = as.numeric(levels(val_label))[val_label])
pr_glm = pr.curve(scores.class0 = pred_glm_tbl$p1, weights.class0 = pred_glm_tbl$val_label)
# AUCPR
pr_glm$auc.integral
roc_glm = roc(pred_glm_tbl$val_label, pred_glm_tbl$p1)
pROC::auc(roc_glm)

# GBM
pred_gbm = h2o.predict(best_gbm, as.h2o(val_h2o), standardize = T)
pred_gbm_tbl = cbind(val_h2o$SEQN_new, pred_gbm %>% as.data.frame(), val_h2o$labels) %>%
  rename('SEQN_new' = 'val_h2o$SEQN_new', 'val_label' = 'val_h2o$labels')
pred_gbm_tbl = pred_gbm_tbl %>% 
  mutate(val_label = as.numeric(levels(val_label))[val_label])
pr_gbm = pr.curve(scores.class0 = pred_gbm_tbl$p1, weights.class0 = pred_gbm_tbl$val_label)
# AUCPR
pr_gbm$auc.integral
roc_gbm = roc(pred_gbm_tbl$val_label, pred_gbm_tbl$p1)
pROC::auc(roc_gbm)

# xgb
pred_xgb = h2o.predict(best_xgb, as.h2o(val_h2o), standardize = T)
pred_xgb_tbl = cbind(val_h2o$SEQN_new, pred_xgb %>% as.data.frame(), val_h2o$labels) %>%
  rename('SEQN_new' = 'val_h2o$SEQN_new', 'val_label' = 'val_h2o$labels')
pred_xgb_tbl = pred_xgb_tbl %>% 
  mutate(val_label = as.numeric(levels(val_label))[val_label])
pr_xgb = pr.curve(scores.class0 = pred_xgb_tbl$p1, weights.class0 = pred_xgb_tbl$val_label)
# AUCPR
pr_xgb$auc.integral
roc_xgb = roc(pred_xgb_tbl$val_label, pred_xgb_tbl$p1)
pROC::auc(roc_xgb)

# drf
pred_drf = h2o.predict(best_drf, as.h2o(val_h2o), standardize = T)
pred_drf_tbl = cbind(val_h2o$SEQN_new, pred_drf %>% as.data.frame(), val_h2o$labels) %>%
  rename('SEQN_new' = 'val_h2o$SEQN_new', 'val_label' = 'val_h2o$labels')
pred_drf_tbl = pred_drf_tbl %>% 
  mutate(val_label = as.numeric(levels(val_label))[val_label])
pr_drf = pr.curve(scores.class0 = pred_drf_tbl$p1, weights.class0 = pred_drf_tbl$val_label)
# AUCPR
pr_drf$auc.integral
roc_drf = roc(pred_drf_tbl$val_label, pred_drf_tbl$p1)
pROC::auc(roc_drf)

### 2. Selected model - GLM
best_glm@model$variable_importances$variable[1:20]
h2o.saveModel(best_glm, path = '/Users/ttran/Desktop/courses/STATS C263/models/PUL/step3/')
# Load model
best_glm = h2o.loadModel(path = "/Users/ttran/Desktop/courses/STATS C263/models/supervised/step1/GLM_1_AutoML_9_20240604_25544")

# find max mcc threshold using ground truth labels
pred_val = h2o.predict(best_glm, as.h2o(val_h2o))
pred_val_tbl = cbind(val_h2o, pred_val %>% as.data.frame()) %>%
  select(-predict) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
  select(-labels) %>%
  rename(labels = diabetes)
val_perf = h2o.performance(best_glm, as.h2o(pred_val_tbl))
mcc_th = h2o.mcc(val_perf)$threshold[which.max(h2o.mcc(val_perf)$absolute_mcc)] # mcc_th = 0.9984515
mcc = h2o.mcc(val_perf)$absolute_mcc[which.max(h2o.mcc(val_perf)$absolute_mcc)] # mcc = 0.6595753

# evaluate naive supervised model using mcc threshold
pred_test = h2o.predict(best_glm, as.h2o(test_norm_step2))
pred_test_tbl = cbind(test_norm_step2, pred_test %>% as.data.frame()) %>%
  select(-predict, -diabetes) %>%
  inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
  rename(GT = diabetes) %>%
  mutate(predict_label = if_else(p1 >= mcc_th, 1, 0))

test_metrics_step3 = get_metrics_df(pred_test_tbl)
auc_step3 = get_auc_df(pred_test_tbl)
brier_step3 = get_brier(pred_test_tbl %>% mutate(GT = as.numeric(as.character(GT))))

# fairness
bw_diff_step3 = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "black")
hw_diff_step3 = get_fairness_metrics(pred_test_tbl, p_group = "white", up_group = "hispanic")

# get final predictions info
pred_test_tbl %>% filter(race_ethnicity==1) %>% pull(predict_label) %>% table()
pred_test_tbl %>% filter(race_ethnicity==2) %>% pull(predict_label) %>% table()
pred_test_tbl %>% filter(race_ethnicity==3) %>% pull(predict_label) %>% table()
pred_test_tbl %>% filter(female==0) %>% pull(predict_label) %>% table()
pred_test_tbl %>% filter(female==1) %>% pull(predict_label) %>% table()

# vimp
vimp = best_glm@model$variable_importances$variable
top20_vimp = data.frame(vimp = best_glm@model$variable_importances$variable[1:20],
                        scaled_imp = best_glm@model$variable_importances$scaled_importance[1:20])
top20_vimp$vimp = factor(top20_vimp$vimp, levels = rev(top20_vimp$vimp))
p = ggplot(top20_vimp, aes(vimp, scaled_imp)) +
  geom_bar(stat = "identity") +
  labs(x = "Feature", y = "Scaled Importance") +
  ggtitle("Variables of Importance of Final Classifier") +
  theme(plot.title = element_text(hjust = 0.5, size=16),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 14))
p + coord_flip()

dic = read.csv('/Users/ttran/Desktop/courses/STATS C263/nhanes_data/dictionary_nhanes.csv')
top20_vimp = top20_vimp %>% 
  left_join(., dic %>% select(variable_codename_use, variable_description_use, in_dataset) %>%
              rename(vimp=variable_codename_use))

write.csv(top20_vimp, file = '/Users/ttran/Desktop/courses/STATS C263/vimp.csv')


p = ggplot(synthetic_lp_rn_famd_df, aes(PC1, PC2, color = class)) + 
  geom_point(size=0.5, alpha=0.5) +
  scale_color_manual(values = c("red", "blue", "skyblue", "orange")) +
  labs(title = "After Adding Filtered Additional Samples (N = 12,000)") + 
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
