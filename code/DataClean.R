library(here)
library(tidyverse)
library(rio)
library(recipes)


scale_vars <- import(here("data", "ScaleVars.xlsx"))
OPP_vars <- import(here("data","OPPvars.xlsx"))
PRO_vars <- import(here("data", "ProParvars.xlsx"))
var_names1 <- readxl::read_excel(here("data", "Variables.xlsx"), sheet = "KEEP2")
var_names2 <- readxl::read_excel(here("data", "Variables.xlsx"), sheet = "KEEP")

scale_filter_vars <- var_names1 %>% select(var) %>% t() %>% as.character()
rename_vars1 <- var_names1 %>% select(var_name) %>% t() %>% as.character()

rename_vars2 <- var_names2 %>% select(var_name) %>% t() %>% as.character()
OPP_filter_vars <- var_names2 %>% select(OPP) %>% t() %>% as.character()
PRO_filter_vars <- var_names2 %>% select(PRO) %>% t() %>% as.character()


scale_data <- scale_vars %>% select(all_of(scale_filter_vars))
colnames(scale_data) <- rename_vars1

OPP_data <- OPP_vars %>% select(all_of(OPP_filter_vars))
colnames(OPP_data) <- rename_vars2

PRO_data <- PRO_vars %>% select(all_of(PRO_filter_vars))
colnames(PRO_data) <- rename_vars2

all_data <- rbind(OPP_data, PRO_data) %>% full_join(scale_data, by = "study_id")

PCBOS_data <- all_data %>% select(matches("study_ID|PCBOS")) %>% 
  mutate_all(~str_replace(., "Checked", "1")) %>% 
  pivot_longer(cols = matches("PCBOS"), names_to = c("Measure", "Var", "Task", "Interval"), names_sep = "_") %>%
  mutate_at(vars(matches("value")), as.numeric) %>%
  group_by(study_id, Var, Task, Measure) %>%
  summarise(ct = sum(value, na.rm = TRUE)) %>% 
  pivot_wider(id_cols = study_id, names_from = c("Measure", "Var", "Task"), values_from = ct)

all_data <- all_data %>% mutate(x_PCBOS_yes = (PCBOS_Praise_Play_1*0)+1) %>%
  select(!starts_with("PCBOS")) %>% full_join(PCBOS_data, by = "study_id") %>%
  mutate(across(starts_with("PCBOS"), ~ case_when(x_PCBOS_yes == 1 ~ .)))

TC_sib_data <- all_data %>% select(matches("study_ID|sib")) %>% 
  mutate_all(~str_replace(., "Yes", "1")) %>%
  mutate_all(~str_replace(., "No", "0")) %>%
  pivot_longer(cols = matches("TC_sib"), names_to = c("Measure", "sib", "Var", "n"), names_sep = "_") %>%
  mutate_at(vars(matches("value")), as.numeric) %>%
  group_by(study_id, Var, sib, Measure) %>%
  summarise(ct = sum(value, na.rm = TRUE), m = mean(value, na.rm = TRUE)) %>% 
  mutate(count = "ct", mean = "mean") 

TC_sib_bx <- TC_sib_data %>% 
  pivot_wider(id_cols = study_id, names_from = c("Measure", "sib", "Var", "count"), values_from = ct) %>%
  mutate(across(starts_with("TC_sib"), ~case_when(TC_sib_yes_ct == 1 ~ .))) %>%
  select(-TC_sib_age_ct)

TC_vars <- TC_sib_data %>% filter(str_detect(Var, "age")) %>% 
  pivot_wider(id_cols = study_id, names_from = c("Measure", "sib", "Var", "mean"), values_from = m) %>%
  full_join(TC_sib_bx) %>%
  mutate(across(contains("mean"), ~case_when(TC_sib_yes_ct == 1 ~ .)))

  #making things look not ugly
all_data <- all_data %>% select(-contains("TC_sib")) %>% full_join(TC_vars) %>%
  mutate(x_demo_na = case_when(!is.na(TC_gender) ~ 1)) %>%
  mutate(across(starts_with("TC_race"), ~case_when(is.na(.) == FALSE ~ 1))) %>%
  mutate(across(starts_with("TC_race"), ~if_else(is.na(.) == FALSE, ., 
                                                 if_else(is.na(x_demo_na), NA_real_, 0)))) %>%
  mutate(across(everything(.), ~str_replace(., "Checked", "1"))) %>%
  mutate(gov_assist = as.numeric(gov_assist)) %>%
  mutate(gov_assist = if_else(x_demo_na == 1, gov_assist*1, NA_real_)) %>%
  mutate(TC_sib_yes_ct = as.numeric(TC_sib_yes_ct)) %>%
  mutate(TC_sib_yes_ct = if_else(is.na(TC_sib_yes_ct) == FALSE, TC_sib_yes_ct*1, 
                                 if_else(is.na(x_demo_na), NA_real_, 0))) %>%
  mutate(across(matches("yrs_ed|annual_income"), ~str_replace(., "\\w{1}\\. ", ""))) %>%
  mutate(TC_diagnosis = str_replace_all(TC_diagnosis, 
                                        c("Autism.*" = "ASD", 
                                          "Genetic.*" = "Genetic Disorder", 
                                          "Learning.*" = "Learning Disorder", 
                                          "Motor.*" = "Motor Delay", 
                                          "Social.*" = "Social-Emotional Disorder", 
                                          "Deaf.*" = "Deaf/Hearing Impaired", 
                                          "Sensory.*" = "Sensory Disorder"))) %>%
  mutate(across(matches("PC_fam"), ~str_replace(., ", who & what:", ""))) %>%
  mutate(across(matches("employment"), ~str_replace(., "Self-employed", "Self employed"))) %>%
  mutate(TC_SPED = str_replace(TC_SPED, "Don't Know", "DK")) %>%
  mutate(PC_employ_hours = as.numeric(PC_employ_hours)) %>%
  mutate(PC_employ_hours = if_else(is.na(PC_employ_hours) == FALSE, PC_employ_hours*1, 
                                   if_else(is.na(PC_employment), NA_real_, 
                                                 0)))
  
  export(all_data, here("data", "OPP_PRO_Clean.csv"))
  