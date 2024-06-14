library(tidyverse)
library(purrr)
pacman::p_load(lubridate, caret, gtsummary, MatchIt)
library(splitTools)
library(parglm)

data_dir = "/Users/ttran/Desktop/courses/STATS C263/nhanes_data/"
qdata = read.csv(file = paste0(data_dir,"questionnaire_clean.csv"))
response = read.csv(file = paste0(data_dir, "response_clean.csv"))
demo = read.csv(file = paste0(data_dir, "demographics_clean.csv"))
chem = read.csv(file = paste0(data_dir, "chemicals_clean.csv"))
occ = read.csv(file = paste0(data_dir, "occupation_clean.csv"))
diet = read.csv(file = paste0(data_dir, "dietary_clean.csv"))
comments = read.csv(file = paste0(data_dir, "comments_clean.csv"))
dic = read.csv(file = paste0(data_dir, "dictionary_nhanes.csv"))
dic_cat = read.csv(file = paste0(data_dir, "dictionary_harmonized_categories.csv"))
dic_chem = dic %>% filter(in_dataset=="Chemicals")
dic_demo = dic %>% filter(in_dataset=="Demographics")
dic_diet = dic %>% filter(in_dataset=="Dietary")
dic_occ = dic %>% filter(in_dataset=="Occupation")
dic_q = dic %>% filter(in_dataset=="Questionnaire")
dic_response = dic %>% filter(in_dataset=="Response")

# subset dics for cycles of interest
dic_diet_subset = dic_diet %>%
  filter(grepl('1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018', unique_cycles))
dic_chem_subset = dic_chem %>%
  filter(grepl('1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018', unique_cycles)) %>%
  filter(!variable_codename_use %in% c('SEQN', 'SEQN_new', 'SDDSRVYR', 'LBXCOT'))
dic_q_subset = dic_q %>%
  filter(grepl('1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018', unique_cycles)) %>%
  filter(!variable_codename_use %in% c('SEQN', 'SEQN_new', 'SDDSRVYR', 'DIQ010', 'ALQ151'))
dic_response_subset = dic_response %>%
  filter(grepl('1999-2000, 2001-2002, 2003-2004, 2005-2006, 2007-2008, 2009-2010, 2011-2012, 2013-2014, 2015-2016, 2017-2018', unique_cycles))

# plasma fasting glucose - LBXGLU; repsonse data
# doctor told you you have diabetes? - DIQ010; questionnaire

#### clean data based on criteria
# criteria:
# 1) >20 yrs of age
# RIDAGEYR - Age in years of the participant at the time of screening; demo
# 2) non-pregnant - RHQ131; questionnaire

demo_clean = demo %>% 
  filter(RIDAGEYR>=20)
# filter pregnancy status based on questionnaire
# RHQ131 - Ever been pregnant?
# MAPF12R - Pregnancy status recode
# RHD143 - Are you pregnant now?
non_pregnant1 = qdata %>% # Ever been pregnant?
  filter(RHQ131==2 | MAPF12R==2 | RHD143==2) %>%
  pull(SEQN_new)

# filter pregnancy status based on questionnaire
# RIDEXPRG - Pregnancy status at exam
# RIDPREG - Pregnancy status variable based on all source data
non_pregnant2 = demo %>% # Pregnancy status at exam
  filter(RIDEXPRG==2 | RIDPREG==2) %>%
  pull(SEQN_new)

# filter pregnancy status based on response
# PERPREG - Patient Pregnant--cannot obtain BIA
# PEPPACE2 - Examinee pregnant--cannot obtain BIA replicate 2
non_pregnant3 = response %>%
  filter(PEPPACE2==2 | PERPREG==2) %>%
  pull(SEQN_new)

non_pregnant = c(non_pregnant1, non_pregnant2, non_pregnant3) %>% unique()
nonpreg_female = demo_clean %>%
  filter(RIAGENDR==2) %>%
  filter(SEQN_new %in% non_pregnant) %>%
  pull(SEQN_new)
demo_clean = demo_clean %>%
  filter(RIAGENDR==1 | SEQN_new %in% nonpreg_female)

#### clean data based on missing values
# yes/no diabetes
diabetes_status_qdata = qdata %>% filter(DIQ010 %in% c(1,2,3)) %>% pull(SEQN_new)
diabetes1_qdata = qdata %>% filter(DIQ010==1) %>% pull(SEQN_new)
diabetes_status_response = response %>% filter(!is.na(LBXGLU)) %>% pull(SEQN_new)
diabetes1_response = response %>% filter(LBXGLU >=126) %>% pull(SEQN_new)

# combine demo variables and covariates for patients w/ diabetes indicator
# NOTE: physical activity not included due to lack of evidence supporting hours spent watching tv as good proxy for sedentary behavior
cov = chem %>% filter(SDDSRVYR %in% c(1,2,3,4,5,6,7,8,9,10)) %>% select(SEQN_new, LBXCOT) %>%
  full_join(., qdata %>% filter(SDDSRVYR %in% c(1,2,3,4,5,6,7,8,9,10)) %>% filter(ALQ151 %in% c(1,2)) %>% select(SEQN_new, ALQ151, BPQ020)) %>%
  full_join(., response %>% filter(SDDSRVYR %in% c(1,2,3,4,5,6,7,8,9,10)) %>% select(SEQN_new, LBDTCSI)) %>%
  na.omit()

demo_cov_clean = demo_clean %>%
  filter(RIDRETH1 %in% c(1,2,3,4)) %>% # race/ethnicity
  filter(!DMDEDUC2 %in% c(7,9)) %>% # education
  filter(SEQN_new %in% diabetes_status_qdata | SEQN_new %in% diabetes_status_response) %>% # diabetes status
  filter(DMDBORN4 %in% c(1,2)) %>% # country of birth
  filter(!is.na(INDFMPIR)) %>% # SES
  filter(SEQN_new %in% (response %>% filter(!is.na(BMXBMI)) %>% pull(SEQN_new))) %>% # BMI
  filter(SDDSRVYR %in% c(1,2,3,4,5,6,7,8,9,10)) %>% # cycle number
  select(SEQN_new, SDDSRVYR, RIDAGEYR, RIAGENDR, RIDRETH1, DMDEDUC2, DMDBORN4, INDFMPIR) %>%
  filter(complete.cases(.)) %>%
  mutate(diabetes = case_when(SEQN_new %in% diabetes1_qdata | SEQN_new %in% diabetes1_response ~ 1,
                              TRUE ~ 0)) %>%
  mutate(race_ethnicity = case_when(RIDRETH1==3 ~ 1, # NH-white
                                    RIDRETH1==4 ~ 2, # NH-AfAm
                                    TRUE ~ 3)) %>% # HL
  rename(education = DMDEDUC2) %>%
  rename(us_born = DMDBORN4) %>%
  mutate(us_born = if_else(us_born==1, 1, 0)) %>%
  mutate(pir = case_when(INDFMPIR <= 1 ~ 1, # low
                         INDFMPIR > 1 & INDFMPIR < 4 ~ 2, # middle
                         INDFMPIR >=4 ~ 3)) %>% # high
  rename(age = RIDAGEYR) %>%
  rename(female = RIAGENDR) %>%
  mutate(female = if_else(female==1, 0, 1)) %>%
  inner_join(., cov) %>%
  na.omit() %>%
  rename(smoke = LBXCOT) %>%
  mutate(smoke = if_else(smoke < 10, 0, 1)) %>%
  rename(alcohol_consumption = ALQ151) %>%
  mutate(alcohol_consumption = if_else(alcohol_consumption == 1, 1, 0)) %>%
  rename(cholesterol_total = LBDTCSI) %>%
  rename(high_bp = BPQ020) %>%
  mutate(high_bp = if_else(high_bp == 1, 1, 0)) %>%
  select(SEQN_new, SDDSRVYR, diabetes, age, female, race_ethnicity, education, us_born, pir,
         smoke, alcohol_consumption, cholesterol_total, high_bp) %>%
  mutate_at(vars(diabetes, female, race_ethnicity, education, us_born, pir, alcohol_consumption,
                 smoke, high_bp), factor)

# check how many chem features per unique set of cycles
# chem_cohort = list()
# chem_cohort[order(sapply(chem_cohort, `[`, 1))]
# for(i in unique(dic_chem$unique_cycles)){
#   chem_cohort[[i]] = dic_chem %>%
#     filter(!variable_codename_use %in% c("SEQN", "SEQN_new", "SDDSRVYR")) %>%
#     filter(unique_cycles==i) %>%
#     pull(variable_description_use) %>%
#     unique() %>%
#     length()
# }

vals79 = c(7, 77, 777, 7777, 77777, 9, 99, 999, 9999, 99999)
rm_79 = function(df){
  for(i in colnames(df)){
    if((df[[i]] %>% unique() %>% length()) < 10){
      df[[i]][df[[i]] %in% vals79] = NA
    }
  }
  return(df)
}

# diet
# 16 is SEQN_new
dic_diet_subset = dic_diet_subset %>%
  filter(!X %in% c(17, 18, 19, 20, 21, 22, 24, 28, 29, 30, 31, 111, 241, 31, 22, 24, 288))

# labs
dic_response_subset = dic_response_subset %>%
  filter(!X %in% c(729, 730, 731, 739, 745, 788, 915, 921:924, 934, 940, 946, 
                  952, 953, 959, 964, 967, 970, 973, 974, 993, 1018, 1019, 
                  1023, 1024, 1025, 1029, 1030, 1084, 1120, 1196, 1202, 1208, 
                  1214, 1217, 1220, 1223, 1226, 1230, 1428, 1434, 1454, 1457, 
                  1511, 1582, 1591, 1601, 1606, 1612, 1745, 1752))

# questionnaire
dic_q_subset = dic_q_subset %>%
  filter(X %in% c(1920, 1921, 1923, 2740, 2741, 2742, 2743, 2791, 2797, 2808, 
                   2822:2828, 2831:2833, 2837, 2838, 2846, 2960, 2961, 2962, 3009))

combined_data = diet %>%
  filter(SDDSRVYR %in% c(1:10)) %>%
  filter(survey_day==1) %>%
  filter(DRXDRSTZ==1) %>%
  select(SEQN_new, all_of(dic_diet_subset$variable_codename_use)) %>%
  full_join(., qdata %>% filter(SDDSRVYR %in% c(1:10)) %>% 
              select(SEQN_new, all_of(dic_q_subset$variable_codename_use)), by = "SEQN_new") %>%
  full_join(., response %>% filter(SDDSRVYR %in% c(1:10)) %>%
              select(SEQN_new, all_of(dic_response_subset$variable_codename_use)), by = "SEQN_new") %>%
  full_join(., chem %>% filter(SDDSRVYR %in% c(1:10)) %>%
              select(SEQN_new, all_of(dic_chem_subset$variable_codename_use)), by = "SEQN_new") %>%
  rm_79(.)

num_samples = list()
for(i in colnames(combined_data)[-1]){
  col = combined_data %>%
    select(SEQN_new, i)
  num_samples[[i]] = demo_cov_clean %>%
    inner_join(., col) %>%
    na.omit() %>%
    nrow()
}
num_samples_filtered = keep(num_samples, ~ any(.x > 17000))

combined_data_filtered = combined_data %>%
  select(SEQN_new, all_of(names(num_samples_filtered)))
final_df = demo_cov_clean %>%
  inner_join(., combined_data_filtered) %>%
  na.omit()

# for sanity check
for(i in colnames(final_df)[15:length(colnames(final_df))]){
  if(class(final_df[[i]]) != "character"){
    print(i)
    print(final_df[[i]] %>% max())
  }
}
dic_final_subet = dic %>% filter(variable_codename_use %in% colnames(final_df))

# make discrete variables factors
discrete_vars = c()
for(i in colnames(final_df)[14:length(colnames(final_df))]){
  if((final_df[[i]] %>% unique() %>% length()) < 10){
    if((final_df[[i]] %>% unique() %>% length()) == 2){ # binary
      final_df = final_df %>%
        mutate(!!sym(i) := if_else(!!sym(i) == 1, 1, 0))
    }
    discrete_vars = c(discrete_vars, i)
  }
}
final_df = final_df %>%
  mutate_at(vars(discrete_vars), factor)

save(final_df, file = paste0(data_dir, "final_df.rda"))
load(file = "/Users/ttran/Desktop/courses/STATS C263/clean_data/final_df.rda")

# stratified train-test split by diabetes status
set.seed(1) # original

inds = partition(final_df$diabetes, p = c(train=0.9, test=0.1))
train_prep = final_df[inds$train,]
test = final_df[inds$test,]

# split train into train/val 90:10 by diabetes status
set.seed(1) 
inds = partition(train_prep$diabetes, p = c(train=0.9, val=0.1))
train = train_prep[inds$train,]
val = train_prep[inds$val,]

# Compare sample distribution at patient-level
combined_sample = rbind(train, val, test) %>% as.data.frame() %>% 
  mutate(group = c(rep("train", nrow(train)),
                   rep("validation", nrow(val)),
                   rep("test", nrow(test))))

combined_sample %>% select(diabetes, age, female, race_ethnicity, education, us_born, pir,
                           smoke, alcohol_consumption, cholesterol_total, high_bp, group) %>% 
  tbl_summary(by = "group") %>% add_overall()

load('/Users/ttran/Desktop/courses/STATS C263/train_step3.rda')
# Check the distribution of the target variable in both sets
prop.table(table(train$diabetes))
prop.table(table(test$diabetes))

# randomly assign 75% of positive examples to unlabeled (0) set
diabetes_rows = which(train$diabetes == 1)
set.seed(123) 
shuffled_indices = sample(diabetes_rows)
lp_perc = 0.25
num_samples_2change = floor(lp_perc * length(diabetes_rows))
train[shuffled_indices[1:num_samples_2change], "diabetes"] = 0

# check number of cases per race/ethnicity
train %>% filter(race_ethnicity==1) %>% filter(diabetes==1) %>% nrow()
train %>% filter(race_ethnicity==2) %>% filter(diabetes==1) %>% nrow()
train %>% filter(race_ethnicity==3) %>% filter(diabetes==1) %>% nrow()

# rda format
save(train, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/train_allfeats_', lp_perc, '.rda'))
save(val, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/val_allfeats_', lp_perc, '.rda'))
save(test, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/test_allfeats_', lp_perc, '.rda'))

# csv format
write.csv(train, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/csv/train_allfeats_', lp_perc, '.csv'))
write.csv(val, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/csv/val_allfeats_', lp_perc, '.csv'))
write.csv(test, file=paste0('/Users/ttran/Desktop/courses/STATS C263/clean_data/csv/test_allfeats_', lp_perc, '.csv'))