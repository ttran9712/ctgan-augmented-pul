min_max_scale = function(x, col_name, min_list, max_list) {
  (x - min_list[[col_name]]) / (max_list[[col_name]] - min_list[[col_name]])
}

get_auc_models = function(df){
  model_aucpr = list()
  model_auc = list()
  
  # GLM
  pred_glm = h2o.predict(best_glm, as.h2o(df), standardize = T)
  pred_glm_tbl = cbind(df$SEQN_new, pred_glm %>% as.data.frame(), df$label)
  names(pred_glm_tbl) = c("SEQN_new", "predict", "p0", "p1", "label")
  pred_glm_tbl = pred_glm_tbl %>% 
    mutate(label = as.numeric(levels(label))[label]) %>%
    inner_join(., df %>% select(SEQN_new, race_ethnicity))
  pr_glm = pr.curve(scores.class0 = pred_glm_tbl$p1, weights.class0 = pred_glm_tbl$label)
  # AUCPR
  model_aucpr[["glm"]] = pr_glm$auc.integral %>% as.numeric()
  roc_glm = roc(pred_glm_tbl$label, pred_glm_tbl$p1)
  model_auc[["glm"]] = pROC::auc(roc_glm) %>% as.numeric()
  
  # GBM
  pred_gbm = h2o.predict(best_gbm, as.h2o(df), standardize = T)
  pred_gbm_tbl = cbind(df$SEQN_new, pred_gbm %>% as.data.frame(), df$label)
  names(pred_gbm_tbl) = c("SEQN_new", "predict", "p0", "p1", "label")
  pred_gbm_tbl = pred_gbm_tbl %>% 
    mutate(label = as.numeric(levels(label))[label]) %>%
    inner_join(., df %>% select(SEQN_new, race_ethnicity))
  pr_gbm = pr.curve(scores.class0 = pred_gbm_tbl$p1, weights.class0 = pred_gbm_tbl$label)
  # AUCPR
  model_aucpr[["gbm"]] = pr_gbm$auc.integral %>% as.numeric()
  roc_gbm = roc(pred_gbm_tbl$label, pred_gbm_tbl$p1)
  model_auc[["gbm"]] = pROC::auc(roc_gbm) %>% as.numeric()
  
  # XGB
  pred_xgb = h2o.predict(best_xgb, as.h2o(df), standardize = T)
  pred_xgb_tbl = cbind(df$SEQN_new, pred_xgb %>% as.data.frame(), df$label)
  names(pred_xgb_tbl) = c("SEQN_new", "predict", "p0", "p1", "label")
  pred_xgb_tbl = pred_xgb_tbl %>% 
    mutate(label = as.numeric(levels(label))[label]) %>%
    inner_join(., df %>% select(SEQN_new, race_ethnicity))
  pr_xgb = pr.curve(scores.class0 = pred_xgb_tbl$p1, weights.class0 = pred_xgb_tbl$label)
  # AUCPR
  model_aucpr[["xgb"]] = pr_xgb$auc.integral %>% as.numeric()
  roc_xgb = roc(pred_xgb_tbl$label, pred_xgb_tbl$p1)
  model_auc[["xgb"]] = pROC::auc(roc_xgb) %>% as.numeric()
  
  # DRF
  pred_drf = h2o.predict(best_drf, as.h2o(df), standardize = T)
  pred_drf_tbl = cbind(df$SEQN_new, pred_drf %>% as.data.frame(), df$label)
  names(pred_drf_tbl) = c("SEQN_new", "predict", "p0", "p1", "label")
  pred_drf_tbl = pred_drf_tbl %>% 
    mutate(label = as.numeric(levels(label))[label]) %>%
    inner_join(., df %>% select(SEQN_new, race_ethnicity))
  pr_drf = pr.curve(scores.class0 = pred_drf_tbl$p1, weights.class0 = pred_drf_tbl$label)
  # AUCPR
  model_aucpr[["drf"]] = pr_drf$auc.integral %>% as.numeric()
  roc_drf = roc(pred_drf_tbl$label, pred_drf_tbl$p1)
  model_auc[["drf"]] = pROC::auc(roc_drf) %>% as.numeric()
  
  return(list(model_aucpr, model_auc))
}

get_maxmcc = function(){
  pred_val = h2o.predict(best_model , as.h2o(val_h2o))
  pred_val_tbl = cbind(val_h2o, pred_val %>% as.data.frame()) %>%
    select(-predict) %>%
    inner_join(., final_df %>% select(SEQN_new, diabetes)) %>%
    select(-labels) %>%
    rename(labels = diabetes)
  val_perf = h2o.performance(best_model, as.h2o(pred_val_tbl))
  mcc_th = h2o.mcc(val_perf)$threshold[which.max(h2o.mcc(val_perf)$absolute_mcc)] 
  mcc = h2o.mcc(val_perf)$absolute_mcc[which.max(h2o.mcc(val_perf)$absolute_mcc)] 
  return(list(mcc_th, mcc))
}

get_metrics <- function(df){
  TN = dim(df[df[['GT']]==0 & df[['predict_label']]==0,])[1]
  FN = dim(df[df[['GT']]==1 & df[['predict_label']]==0,])[1]
  TP = dim(df[df[['GT']]==1 & df[['predict_label']]==1,])[1]
  FP = dim(df[df[['GT']]==0 & df[['predict_label']]==1,])[1]
  
  sensitivity = TP/(TP+FN)
  specificity = TN/(TN+FP)
  precision = TP/(TP+FP)
  b.acc = (sensitivity + specificity)/2
  F1 = (2 * precision * sensitivity)/(precision + sensitivity)
  positive_rate = (df %>% filter(predict_label==1) %>% nrow())/(df %>% nrow())
  if(all(df[['race_ethnicity']]==1)){
    white_val_pos = final_df %>% filter(diabetes==1) %>% filter(race_ethnicity==1) %>% nrow()
    white_total = final_df %>% filter(race_ethnicity==1) %>% nrow()
    ehr_prevalence = white_val_pos/white_total
    gbe_ehr = positive_rate/ehr_prevalence
  } else if(all(df[['race_ethnicity']]==2)){
    black_val_pos = final_df %>% filter(diabetes==1) %>% filter(race_ethnicity==2) %>% nrow()
    black_total = final_df %>% filter(race_ethnicity==2) %>% nrow()
    ehr_prevalence = black_val_pos/black_total
    gbe_ehr = positive_rate/ehr_prevalence
  } else if(all(df[['race_ethnicity']]==3)){
    hispanic_val_pos = final_df %>% filter(diabetes==1) %>% filter(race_ethnicity==3) %>% nrow()
    hispanic_total = final_df %>% filter(race_ethnicity==3) %>% nrow()
    ehr_prevalence = hispanic_val_pos/hispanic_total
    gbe_ehr = positive_rate/ehr_prevalence
  } else {
    gbe_ehr = NA
  }
  
  return(c(sensitivity, specificity, precision, b.acc, F1, gbe_ehr))
}

get_metrics_df = function(dataset, race){
  white_df <- dataset %>% filter(race_ethnicity==1)
  black_df <- dataset %>% filter(race_ethnicity==2)
  hisp_df <- dataset %>% filter(race_ethnicity==3)
  
  white_metrics = get_metrics(white_df)
  
  black_metrics = get_metrics(black_df)
  
  hisp_metrics = get_metrics(hisp_df)
  
  df_metrics = data.frame(white_sensitivity=white_metrics[1], white_specificity=white_metrics[2], white_precision=white_metrics[3], 
                          white_b.accuracy=white_metrics[4], white_F1=white_metrics[5], white_gbeehr=white_metrics[6],
                          black_sensitivity=black_metrics[1], black_specificity=black_metrics[2], black_precision=black_metrics[3],
                          black_b.accuracy=black_metrics[4], black_F1=black_metrics[5], black_gbeehr=black_metrics[6],
                          hispanic_sensitivity=hisp_metrics[1], hispanic_specificity=hisp_metrics[2], hispanic_precision=hisp_metrics[3],
                          hispanic_b.accuracy=hisp_metrics[4], hispanic_F1=hisp_metrics[5], hispanic_gbeehr=hisp_metrics[6])
  
  # overall
  sensitivity = get_metrics(dataset)[1]
  specificity = get_metrics(dataset)[2]
  precision = get_metrics(dataset)[3]
  b.acc = get_metrics(dataset)[4]
  F1 = get_metrics(dataset)[5]
  
  metrics_overall = data.frame(overall_sensitivity=sensitivity, overall_specificity=specificity, overall_precision=precision, 
                               overall_b.accuracy=b.acc, overall_F1=F1, overall_gbeehr = NA)
  
  combined = cbind(df_metrics, metrics_overall)
  return(combined)
}

get_auc_df = function(df){
  fg_white =  (df %>% filter(race_ethnicity==1) %>% pull(p1))[(df %>% filter(race_ethnicity==1))$GT == 1]
  bg_white = (df %>% filter(race_ethnicity==1) %>% pull(p1))[(df %>% filter(race_ethnicity==1))$GT == 0]
  fg_black =  (df %>% filter(race_ethnicity==2) %>% pull(p1))[(df %>% filter(race_ethnicity==2))$GT == 1]
  bg_black = (df %>% filter(race_ethnicity==2) %>% pull(p1))[(df %>% filter(race_ethnicity==2))$GT == 0]
  fg_hispanic =  (df %>% filter(race_ethnicity==3) %>% pull(p1))[(df %>% filter(race_ethnicity==3))$GT == 1]
  bg_hispanic = (df %>% filter(race_ethnicity==3) %>% pull(p1))[(df %>% filter(race_ethnicity==3))$GT == 0]
  fg = df$p1[df$GT == 1]
  bg = df$p1[df$GT == 0]
  
  auc_overall = roc.curve(scores.class0 = fg, scores.class1 = bg)$auc
  auc_white = roc.curve(scores.class0 = fg_white, scores.class1 = bg_white)$auc
  auc_black = roc.curve(scores.class0 = fg_black, scores.class1 = bg_black)$auc
  auc_hispanic = roc.curve(scores.class0 = fg_hispanic, scores.class1 = bg_hispanic)$auc
  aucpr_overall = pr.curve(scores.class0 = fg, scores.class1 = bg)$auc.integral
  aucpr_white = pr.curve(scores.class0 = fg_white, scores.class1 = bg_white)$auc.integral
  aucpr_black = pr.curve(scores.class0 = fg_black, scores.class1 = bg_black)$auc.integral
  aucpr_hispanic = pr.curve(scores.class0 = fg_hispanic, scores.class1 = bg_hispanic)$auc.integral
  
  auc_df = data.frame(auc_white = auc_white,
                      auc_black = auc_black,
                      auc_hispanic = auc_hispanic,
                      auc_overall = auc_overall,
                      aucpr_white = aucpr_white,
                      aucpr_black = aucpr_black,
                      aucpr_hispanic = aucpr_hispanic,
                      aucpr_overall = aucpr_overall)
  return(auc_df)
}

get_brier = function(dataset){
  overall_brier_positive = sum(((dataset %>% filter(GT==1) %>% pull(p1)) - (dataset %>% filter(GT==1) %>% pull(GT)))^2) / dim(dataset %>% filter(GT==1))[1]
  overall_brier_negative = sum(((dataset %>% filter(GT==0) %>% pull(p1)) - (dataset %>% filter(GT==0) %>% pull(GT)))^2) / dim(dataset %>% filter(GT==0))[1]
  overall_brier_balanced = overall_brier_positive + overall_brier_negative
  
  # white
  white_brier_positive = sum(((dataset %>% filter(race_ethnicity==1) %>% filter(GT==1) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==1) %>% filter(GT==1) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==1) %>% filter(GT==1))[1]
  white_brier_negative = sum(((dataset %>% filter(race_ethnicity==1) %>% filter(GT==0) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==1) %>% filter(GT==0) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==1) %>% filter(GT==0))[1]
  white_brier_balanced = white_brier_positive + white_brier_negative
  # black
  black_brier_positive = sum(((dataset %>% filter(race_ethnicity==2) %>% filter(GT==1) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==2) %>% filter(GT==1) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==2) %>% filter(GT==1))[1]
  black_brier_negative = sum(((dataset %>% filter(race_ethnicity==2) %>% filter(GT==0) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==2) %>% filter(GT==0) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==2) %>% filter(GT==0))[1]
  black_brier_balanced = black_brier_positive + black_brier_negative
  # hispanic
  hispanic_brier_positive = sum(((dataset %>% filter(race_ethnicity==3) %>% filter(GT==1) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==3) %>% filter(GT==1) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==3) %>% filter(GT==1))[1]
  hispanic_brier_negative = sum(((dataset %>% filter(race_ethnicity==3) %>% filter(GT==0) %>% pull(p1)) - (dataset %>% filter(race_ethnicity==3) %>% filter(GT==0) %>% pull(GT)))^2) / dim(dataset %>% filter(race_ethnicity==3) %>% filter(GT==0))[1]
  hispanic_brier_balanced = hispanic_brier_positive + hispanic_brier_negative
  
  brier_df = data.frame(white_brier_negative=white_brier_negative, white_brier_positive=white_brier_positive, white_brier_balanced=white_brier_balanced,
                        black_brier_negative=black_brier_negative, black_brier_positive=black_brier_positive, black_brier_balanced=black_brier_balanced,
                        hispanic_brier_negative=hispanic_brier_negative, hispanic_brier_positive=hispanic_brier_positive, hispanic_brier_balanced=hispanic_brier_balanced,
                        overall_brier_negative=overall_brier_negative, overall_brier_positive=overall_brier_positive, overall_brier_balanced=overall_brier_balanced)
  return(brier_df)
}

get_fairness_metrics <- function(df, p_group, up_group){
  df = df %>%
    mutate(white = if_else(race_ethnicity==1, 1, 0)) %>%
    mutate(black = if_else(race_ethnicity==2, 1, 0)) %>%
    mutate(hispanic = if_else(race_ethnicity==3, 1, 0))
  # selection rate for privileged group
  sr_p = (df %>% filter(!!sym(p_group)==1) %>% filter(predict_label==1) %>% nrow())/(df %>% filter(!!sym(p_group)==1) %>% nrow())
  # selection rate for unprivileged group 
  sr_up = (df %>% filter(!!sym(up_group)==1) %>% filter(predict_label==1) %>% nrow())/(df %>% filter(!!sym(up_group)==1) %>% nrow())
  # statistical parity difference
  sp_diff = sr_up - sr_p
  
  # average absolute odds difference
  # true favorable rate = sensitivity b/c favorable label = AD
  tp_p = df %>% filter(!!sym(p_group)==1) %>% filter(predict_label==1) %>% filter(GT==1) %>% nrow()
  fn_p = df %>% filter(!!sym(p_group)==1) %>% filter(predict_label==0) %>% filter(GT==1) %>% nrow()
  sens_p = tp_p/(tp_p + fn_p)
  tn_p = df %>% filter(!!sym(p_group)==1) %>% filter(predict_label==0) %>% filter(GT==0) %>% nrow()
  fp_p = df %>% filter(!!sym(p_group)==1) %>% filter(predict_label==1) %>% filter(GT==0) %>% nrow()
  fpr_p = fp_p/(fp_p + tn_p)
  tp_up = df %>% filter(!!sym(up_group)==1) %>% filter(predict_label==1) %>% filter(GT==1) %>% nrow()
  fn_up = df %>% filter(!!sym(up_group)==1) %>% filter(predict_label==0) %>% filter(GT==1) %>% nrow()
  sens_up = tp_up/(tp_up + fn_up)
  tn_up = df %>% filter(!!sym(up_group)==1) %>% filter(predict_label==0) %>% filter(GT==0) %>% nrow()
  fp_up = df %>% filter(!!sym(up_group)==1) %>% filter(predict_label==1) %>% filter(GT==0) %>% nrow()
  fpr_up = fp_up/(fp_up + tn_up)
  eqop_diff = sens_up - sens_p # equal opportunity difference - same as true favorable rate difference
  ffr_diff = fpr_up - fpr_p # false favorable/positive rate difference 
  avg_odds_diff = (abs(eqop_diff) + abs(ffr_diff))/2
  
  # average absolute predictive GTue difference
  precision_up = tp_up/(tp_up + fp_up)
  for_up = fn_up/(fn_up + tn_up)
  precision_p = tp_p/(tp_p + fp_p)
  for_p = fn_p/(fn_p + tn_p)
  precision_diff = precision_up - precision_p
  for_diff = for_up - for_p
  apv_diff = (abs(precision_diff) + abs(for_diff))/2
  
  specificity_p = tn_p/(tn_p + fp_p) # specificity
  specificity_up = tn_up/(tn_up + fp_up)
  npv_p = tn_p/(tn_p + fn_p)
  npv_up = tn_up/(tn_up + fn_up)
  fnr_p = fn_p/(fn_p + tp_p)
  fnr_up = fn_up/(fn_up + tp_up)
  fdr_p = fp_p/(fp_p + tp_p)
  fdr_up = fp_up/(fp_up + tp_up)
  f1_p = (2 * precision_p * sens_p)/(precision_p + sens_p)
  f1_up = (2 * precision_up * sens_up)/(precision_up + sens_up)
  bacc_p = (sens_p + specificity_p)/2
  bacc_up = (sens_up + specificity_up)/2
  
  # other differences
  specificity_diff = specificity_up - specificity_p
  npv_diff = npv_up - npv_p
  fnr_diff = fnr_up - fnr_p
  fdr_diff = fdr_up - fdr_p
  f1_diff = f1_up - f1_p
  bacc_diff = bacc_up - bacc_p
  
  return(list(sp_diff=sp_diff, eqop_diff=eqop_diff, 
              ffr_diff=ffr_diff, avg_odds_diff=avg_odds_diff, 
              precision_diff=precision_diff, for_diff=for_diff, 
              apv_diff=apv_diff, specificity_diff=specificity_diff,
              npv_diff=npv_diff, fnr_diff=fnr_diff,
              fdr_diff=fdr_diff, f1_diff=f1_diff,
              bacc_diff=bacc_diff))
}
