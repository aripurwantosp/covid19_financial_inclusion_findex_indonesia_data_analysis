# R file for:
# Running data analysis
# 
# For paper:
# Determinants of Financial Inclusion in Indonesia Before and During COVID-19:
# Evidence from Global Findex Data
# 
# Authors of the paper:
# Prasetyoputra et al.
# 
# Code by:
# Ari Purwanto Sarwo Prasojo
# 
# Date of this version:
# 2025/10/14


gc()
rm(list=ls())


# Library & source ----
library(dplyr)
library(magrittr)
library(stringr)
library(purrr)
library(haven)
library(labelled)
library(writexl)
library(mice)
# library(mitools)
library(survey)
library(marginaleffects)
library(ggeffects)
library(broom)
library(broom.helpers)
library(gtsummary)
library(flextable)
library(ggplot2)
library(patchwork)
library(scales)
library(ggtext)
library(sdcLog)
library(here)


# Path directory ----
dta_path <- here("data")
syn_path <- here("syntax")
log_path <- here("log")
mdl_path <- here("models")
mdl_ame_path <- here(mdl_path, "ame")
out_path <- here("output")

# source ----
source(here(syn_path, "utils_fun.R"))


# Run ----

## 1. Preparing dataset for analysis ----
if(0){
  sdc_log(
    here(syn_path, "1_preparing_dataset_for_analysis.R"),
    here(log_path, "log_1_preparing_dataset_for_analysis.txt"),
    replace = TRUE
  )
}

## 2. Reporting descriptive statistics ----
if(0){
  sdc_log(
    here(syn_path, "2_reporting_descriptive_statistics.R"),
    here(log_path, "log_2_reporting_descriptive_statistics.txt"),
    replace = TRUE
  )
}

## 3. Assessing and imputing mobintpay variable ----
if(1){
  sdc_log(
    here(syn_path, "3_assessing_imputing_mobintpay.R"),
    here(log_path, "log_3_assessing_imputing_mobintpay.txt"),
    replace = TRUE
  )
}

## 4. Fitting models ----
if(0){
  sdc_log(
    here(syn_path, "4_fitting_models.R"),
    here(log_path, "log_4_fitting_models.txt"),
    replace = TRUE
  )
}

## 5. Reporting VIF ----
if(0){
  sdc_log(
    here(syn_path, "5_reporting_vif.R"),
    here(log_path, "log_5_reporting_vif.txt"),
    replace = TRUE
  )
}

## 6. Reporting AME, main analysis ----
if(0){
  sdc_log(
    here(syn_path, "6_reporting_ame_main_analysis.R"),
    here(log_path, "log_6_reporting_ame_main_analysis.txt"),
    replace = TRUE
  )
}

## 7. Sensitivity analysis ----

### 7a. Sensitivity 1: weighted glm + vocvHC(type = "HC1") ----
if(0){
  sdc_log(
    here(syn_path, "7a_reporting_sensitivity_analysis1.R"),
    here(log_path, "log_7a_reporting_sensitivity_analysis1.txt"),
    replace = TRUE
  )
}

### 7b. Sensitivity 2: svyglm adjusted vcov by reported Deff ----
if(0){
  sdc_log(
    here(syn_path, "7b_reporting_sensitivity_analysis2.R"),
    here(log_path, "log_7b_reporting_sensitivity_analysis2.txt"),
    replace = TRUE
  )
}

### 7c. Sensitivity 2: svyglm probit adjusted vcov by reported Deff ----
if(0){
  sdc_log(
    here(syn_path, "7c_reporting_sensitivity_analysis3.R"),
    here(log_path, "log_7b_reporting_sensitivity_analysis3.txt"),
    replace = TRUE
  )
}

### 7d. Forest plot for main vs sensitivity ----
if(0){
  sdc_log(
    here(syn_path, "7d_forest_plot_for_sensitivity.R"),
    here(log_path, "log_7d_forest_plot_for_sensitivity.txt"),
    replace = TRUE
  )
}

### 8. Extracting main models' coefficients ----
if(0){
  sdc_log(
    here(syn_path, "8_extract_coefficients_main_analysis.R"),
    here(log_path, "log_8_extract_coefficients_main_analysis.txt"),
    replace = TRUE
  )
}

## 9. Predicted probability plot ----
if(0){
  sdc_log(
    here(syn_path, "9_predicted_probability_plot.R"),
    here(log_path, "log_9_predicted_probability_plot.txt"),
    replace = TRUE
  )
}



