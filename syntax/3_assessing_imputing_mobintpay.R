# R file for:
# Assessing missing data and imputation for mobintpay
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
# 2025/11/06


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(dta_path, "gcfin1721_final_dataset_for_analysis_imp.rds")
sv_name2 <- here(out_path, "mobintpay_missingness_check_logit.docx")


# Read data ----
df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
df <- readRDS(df_path)
df_wave <- df %>% 
  filter(!is.na(mphone)) %>% 
  group_split(year2c)
names(df_wave) <- c("2017","2021")


# mobintpay: has used mobile phone/internet to make payments ----
# check if missing is not MCAR using logit regression
# notes:
# MCAR - missing completely at random, not MCAR if any significant independent variable
# is missing probability depend on individual characteristics?
mdl_wave <- map(
  df_wave,
  ~{
    glm(
      mobintpay_mi ~ female + age + agesq + educ3c + income5c + employed + mphone,
      data = filter(., acdebit == 1),
      family = binomial(link="logit")
    )
  }
)
map(mdl_wave, ~summary(.))

# report missingnes diagnostic (logistic regression)
tbl_reg <- map(mdl_wave,
  ~{
    tbl_regression(.,
      exponentiate = FALSE,
      estimate_fun = ~ style_number(., digits = 3)
    ) %>% 
      add_significance_stars(hide_ci = TRUE, hide_p = TRUE) %>%
      modify_header(label = "**Variable**") %>% 
      add_glance_table(include = c(nobs))
  } 
) 

tbl_reg_m <- tbl_merge(
  tbl_reg,
  tab_spanner = c("**2017**", "**2021**")
) %>% 
  remove_abbreviation("CI = Confidence Interval") %>%
  modify_table_styling(
    columns = label,
    rows = row_type == "label",
    text_format = "bold"
  )

tbl_reg_m

tbl_reg_m %>%
  as_flex_table() %>%
  font(fontname = "Times New Roman", part = "all") %>%
  save_as_docx(path = sv_name2)


# impute mobintpay ----
# index or location of missing obervation
idx <- map(df_wave,
  ~{mutate(., row = row_number()) %>% 
    filter(acdebit == 1 & mobintpay_mi == 1) %>% 
    pull(row)
  }
)

# select eligible respondent only
df_wave_elig <- map(df_wave, ~filter(., acdebit == 1))
# index or location of missing obervation in eligible dataset
idx_elig <- map(df_wave_elig,
  ~{mutate(., row = row_number()) %>% 
    filter(mobintpay_mi == 1) %>% 
    pull(row)
  }
)

# select relevant variables
df_wave_elig <- map(
  df_wave_elig, 
  ~{
    # log wieght
    mutate(., logwgt = log(wgt)) %>% 
    dplyr::select(., 
      mobintpay, female, age, agesq, educ3c, 
      income5c, employed, mphone, logwgt
    )
  }
)

# make predictor matrix
pred_mat <- map(df_wave_elig, ~mice::make.predictorMatrix(.))
pred_mat[[1]][2:9,] <- 0
pred_mat[[2]][2:9,] <- 0

# multiple imputation
# notes:
# if calculation is not prohibitive, we may set m to the average percentage of
# missing data. The substantive conclusions are unlikely to change as a result
# of raising m (van Buuren, 2018)
m <- 30
mi_mobintpay <- map2(
  df_wave_elig,
  pred_mat,
  ~mice::mice(
    .x,
    m = m,
    method = "logreg",
    predictorMatrix = .y,
    maxit = 10,
    seed = 1234
  )
)

# complete data (subset only)
df_imp <- map(
  mi_mobintpay,
  ~mice::complete(., action = "all")
)

# complete wave data (append to full data)
df_wave_imp <- list(
  `2017` = rep(list(df_wave[[1]]), m),
  `2021` = rep(list(df_wave[[2]]), m)
)
# replace missing with imputed
for(i in 1:m){
  df_wave_imp[[1]][[i]]$mobintpay[idx[[1]]] <- df_imp[[1]][[i]]$mobintpay[idx_elig[[1]]]
  df_wave_imp[[2]][[i]]$mobintpay[idx[[2]]] <- df_imp[[2]][[i]]$mobintpay[idx_elig[[2]]]
}

# pooled
df_pooled_imp <- list()
for(i in 1:m){
  df_pooled_imp[[i]] <- bind_rows(
    # 2017
    df_wave_imp[[1]][[i]],
    # 2021
    df_wave_imp[[2]][[i]]
  )
}

# save
saveRDS(df_pooled_imp, file = sv_name)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))