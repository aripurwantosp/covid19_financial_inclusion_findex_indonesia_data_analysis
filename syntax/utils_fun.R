# R file for:
# Auxiliarry or utils functions
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


# Function for extracting average marginal effect
# note:
# mdl  = model (glm or svyglm object)
# vcov = variance-covariance type see ?marginaleffects::avg_slopes
ame <- function(mdl, vcov = TRUE){
  if("svyglm" %in% class(mdl)){
    # marginaleffects::avg_slopes
    dydx <- avg_slopes(mdl, vcov = vcov, wts = weights(mdl$survey.design))
  }else{
    dydx <- avg_slopes(mdl, vcov = vcov)
  }
  return(dydx)
}

# ame for multiple imputation model (list)
ame_mi <- function(mdl, vcov = TRUE){
  dydx <- map(mdl, ~ame(.))
  ests <- map(dydx, ~coef(.))
  vcovs <- map(dydx, ~vcov(.))
  infer <- mitools::MIcombine(results = ests, variances = vcovs)
  return(infer)
}

# custom vcov
# adjusted by reported design effect
# mdl = svyglm object
# DE: 2017 = 1.38 & 2021 = 1.42
# n: 2017 = 1000 & 2021 = 1062
# pooled DE = (n1 * DE1 + n2 * DE2)/(n1 + n2)
#           = (1.38 * 1000 + 1.42 * 1062)/(1000 + 1062)
#           = 1.400601
DEvcov <- function(mdl, DE = 1.400601){
  # vc <- sandwich::vcovHC(mdl, type = type)
  vc <- vcov(mdl)
  vc <- DE * vc
  return(vc)
}

# Function for tidying average marginal effect
# note:
# dydx  = an object from ame function
tidy_ame <- function(dydx){
  out <- tidy(dydx)
  # rearrange for income var
  out <- out[c(5, 1:3, 8, 9, 6, 7, 4, 10:11), ]
  return(out)
}

# tidying ame for multiple imputation model
# dydx = an object from ame_mi function
tidy_ame_mi <- function(dydx){
  est <- dydx$coefficients
  se <- sqrt(diag(dydx$variance))
  tval <- est/se
  df <- dydx$df
  pval <- 2 * (1 - pt(abs(tval), df))
  lower <- est - qt(0.975, df) * se
  upper <- est + qt(0.975, df) * se
  out <- tibble(
    term_contrast = names(est),
    estimate = est,
    `std.error` = se,
    statistic = tval,
    `p.value` = pval,
    `conf.low` = lower,
    `conf.high` = upper,
    missinfo = dydx$missinfo
  ) %>% 
    tidyr::separate(term_contrast, into = c("term", "contrast"), sep = " ", extra = "merge")
  # rearrange for income var
  out <- out[c(5, 1:3, 8, 9, 6, 7, 4, 10:11), ]
  return(out)
}

# Function for modifying tidy output
# note:
# mod_tidy_num: keep in numeric, input
#   df_tidy = object from tidy_ame or tidy_ame_mi
# mod_tidy_char: make into character, input
#   mdf_tidy = object from mod_tidy_num
mod_tidy_num <- function(df_tidy){
  mdf_tidy <- df_tidy %>% 
    mutate(
      level = str_trim(str_extract(contrast, "^[^-]+")),
      level = ifelse(level == "dY/dX", "", level)
    ) %>% 
    dplyr::select(
      term, level, 
      ame = estimate, se = `std.error`,
      pvalue = `p.value`, low = `conf.low`, high = `conf.high`
    )
  
  return(mdf_tidy) 
}

mod_tidy_char <- function(mdf_tidy){
  mdf_tidy <- mdf_tidy %>% 
        mutate(
          sig = stars(pvalue),
          ame = fmt_number(ame),
          ame = str_c(ame, sig),
          se = fmt_number(se, width = 5)
        ) %>% 
        dplyr::select(-c(pvalue, sig, low, high))
  
  return(mdf_tidy)
}

# Function for reshaping AME of multiple models from long to wide
ame_long_to_wide <- function(df){
  df_wide <- df %>% 
    tidyr::pivot_wider(
      # id_cols = c("term","level"), 
      names_from = indicator,
      values_from = c(ame, se),
      names_glue = "{indicator}_{.value}"
    )
  
  df_wide_h <- df_wide %>% 
    filter(level != "") %>% 
    # pecah per var_lab, lalu untuk tiap grup buat 1 baris heading + baris aslinya
    group_split(term) %>%
    map_dfr(function(df) {
      df <- df %>% mutate(level = paste0("   ", level))
      heading_row <- df[1, ]            # salin struktur kolom dari baris pertama
      ncols <- ncol(heading_row)
      heading_row$level <- as.character(df$variable[1])  # isi kolom 'level' dengan nama var_lab
      heading_row[, 3:ncols] <- NA
      bind_rows(heading_row, df)           # gabungkan heading di atas baris asli
    })
  
  df_wide_h <- bind_rows(
    df_wide_h[1:2, ],
    df_wide[2, ],
    df_wide_h[-c(1,2), ]
  ) %>% 
    mutate(level = ifelse(level == "", variable, level)) %>% 
    dplyr::select(-variable)
  
  return(df_wide_h)
}

# Function for add vars heading AME df (single) model
ame_add_var_head <- function(df_ame, ref_vars){
  df_ame <- df_ame %>% 
    mutate(term = factor(term, levels = ref_vars$term)) %>%
    left_join(ref_vars, by = "term")

  df_ame_h <- df_ame %>% 
    filter(level != "") %>% 
    group_split(term) %>%
    map_dfr(function(df) {
      df <- df %>% mutate(level = paste0("   ", level))
      heading_row <- df[1, ]            # salin struktur kolom dari baris pertama
      heading_row$level <- as.character(df$variable[1])  # isi kolom 'level' dengan nama var_lab
      heading_row$ame  <- NA  
      heading_row$se  <- NA
      heading_row$pvalue <- NA
      heading_row$low <- NA
      heading_row$high <- NA
      bind_rows(heading_row, df)           # gabungkan heading di atas baris asli
    })
  
  df_ame_h <- bind_rows(
    df_ame_h[1:2, ],
    df_ame[2, ],
    df_ame_h[-c(1,2), ]
  )

  df_ame_h <- df_ame_h %>% 
    mutate(
      level = ifelse(level == "", variable, level),
      level_idx = n():1
    ) %>% 
    dplyr::select(-variable)

  return(df_ame_h)

}

# # Function for calculating area under curve
# # note:
# # mdl = model (glm or svyglm object)
# auc <- function(mdl){
#   # performance::performance_roc
#   x <- performance_roc(mdl)
#   auc <- bayestestR::area_under_curve(x$Specificity, x$Sensitivity)
#   return(auc)
# }

# Function for calculating area under ROC curve (AUC)
# note:
# mdl = model (glm or svyglm object)
auc <- function(mdl){
  df <- model.frame(mdl)
  if("svyglm" %in% class(mdl)){
    # df <- mdl$survey.design$variables
    df <- df %>% rename(weights = `(weights)`)
  }else{
    df$weights <- mdl$prior.weights
  }
  # predict response
  df$pred <- predict(mdl, type = "response")
  # from WeightedROC::
  roc_obj <- WeightedROC::WeightedROC(df$pred, df[,1], weight = df$weights)
  auc <- WeightedROC::WeightedAUC(roc_obj)
  return(auc)
}

# auc for multiple imputation model (list)
auc_mi <- function(mdl){
  auc <- map_dbl(mdl, ~auc(.))
  mean_auc <- mean(auc)
  sd_auc <- sd(auc)
  out <- c(mean_auc, sd_auc)
  names(out) <- c("mean", "sd")
  return(out)
}

# Function for calcualting Tjur's R2
# note:
# mdl = model (glm or svyglm object)
tjur_r2 <- function(mdl){
  df <- model.frame(mdl)
  if("svyglm" %in% class(mdl)){
    # df <- mdl$survey.design$variables
    df <- df %>% rename(weights = `(weights)`)
  }else{
    df$weights <- mdl$prior.weights
  }
  # predict response
  df$pred <- predict(mdl, type = "response")
  lev <- levels(df[,1])
  idx_1 <- which(df[,1] == lev[2])
  idx_0 <- which(df[,1] == lev[1])
  r2 <- (weighted.mean(df$pred[idx_1], df$weights[idx_1]) -
    weighted.mean(df$pred[idx_0], df$weights[idx_0]))
  return(r2)
}

# Tjur's R2 for multiple imputation model (list)
tjur_r2_mi <- function(mdl){
  r2 <- map_dbl(mdl, ~tjur_r2(.))
  mean_r2 <- mean(r2)
  sd_r2 <- sd(r2)
  out <- c(mean_r2, sd_r2)
  names(out) <- c("mean", "sd")
  return(out)
}

# Function for calculate optimum point of age's quadratic term
opt_age <- function(mdl){
  coefs <- coef(mdl)
  coef_names <- names(coefs)
  idx_age <- which(coef_names %in% c("age", "I(age^2)"))
  coefs_age <- coefs[idx_age]
  opt_age <- -coefs_age[1]/(2*coefs_age[2])
  return(opt_age)
}

# function for format vector number
fmt_number <- function(vc, digits = 3, width = 7){
  # sprintf("%.3f", vc) #set digit in here
  formatC(vc, format = "f", digits = digits, width = width)
}

# stars sign function
stars <- function(p) {
  ifelse(p < 0.001, "***",
  ifelse(p < 0.01,  "**",
  ifelse(p < 0.05,  "*",
  ifelse(p < 0.1,   "+",""))))
}