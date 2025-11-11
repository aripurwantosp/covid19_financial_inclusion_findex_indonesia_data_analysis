# R file for:
# Fitting regression models
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


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Read data ----
# non-imputed dataset
df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
df_full <- readRDS(df_path)
apply(df_full, 2, anyNA)
df <- df_full %>% filter(!is.na(mphone))

# imputed dataset (for mobintpay)
df_path_imp <- here(dta_path, "gcfin1721_final_dataset_for_analysis_imp.rds")
df_imp <- readRDS(df_path_imp)


# Using survey design ----
# set design
full_design <- svydesign(ids = ~1, weights = ~wgt, data = df_full)
design <- subset(full_design, !is.na(mphone))

## Owns a financial account or mobile money ----
svyglm_account <- svyglm(
  account ~ female + age + I(age^2) + educ3c + income5c + employed + 
    mphone + year2c,
  design = design,
  family = binomial(link="logit")
)
saveRDS(svyglm_account, file = here(mdl_path, "svyglm_account.rds"))

## Saved money in the past year ----
svyglm_saving <- update(svyglm_account, saving ~ .)
saveRDS(svyglm_saving, file = here(mdl_path, "svyglm_saving.rds"))

## Borrowed money in the past year ----
svyglm_credit <- update(svyglm_account, credit ~ .)
saveRDS(svyglm_credit, file = here(mdl_path, "svyglm_credit.rds"))

## Has used mobile phone/internet to make payments ----
# note: mobintpay - using imputed dataset
# set design
design_imp <- map(
  df_imp,
  ~svydesign(ids = ~1, weights = ~wgt, data = .)
)
# fit svyglm
svyglm_mobintpay <- map(
  design_imp,
  ~svyglm(
    mobintpay ~ female + age + I(age^2) + educ3c + income5c + employed + 
      mphone + year2c,
    design = subset(., !is.na(mphone) & acdebit == 1),
    family = binomial(link="logit")
  )
)
saveRDS(svyglm_mobintpay, file = here(mdl_path, "svyglm_mobintpay.rds"))

## Has used mobile money services ----
svyglm_mmoney <- update(svyglm_account, mmoney ~ .)
saveRDS(svyglm_mmoney, file = here(mdl_path, "svyglm_mmoney.rds"))


# Non survey design + weights (for sensitivity analysis) ----

## Owns a financial account or mobile money ----
glm_account <- glm(
  account ~ female + age + I(age^2) + educ3c + income5c + employed + 
    mphone + year2c,
  data = df,
  family = binomial(link ="logit"),
  weights = wgt
)
saveRDS(glm_account, file = here(mdl_path, "glm_account.rds"))

## Saved money in the past year ----
glm_saving <- update(glm_account, saving ~ .)
saveRDS(glm_saving, file = here(mdl_path, "glm_saving.rds"))

## Borrowed money in the past year ----
glm_credit <- update(glm_account, credit ~ .)
saveRDS(glm_credit, file = here(mdl_path, "glm_credit.rds"))

## Has used mobile phone/internet to make payments ----
# note: mobintpay - using imputed dataset
glm_mobintpay <- map(
  df_imp,
  ~glm(
    mobintpay ~ female + age + I(age^2) + educ3c + income5c + employed + 
      mphone + year2c,
    data = filter(., !is.na(mphone) & acdebit == 1),
    family = binomial(link ="logit"),
    weights = wgt
  )
)
saveRDS(glm_mobintpay, file = here(mdl_path, "glm_mobintpay.rds"))

## Has used mobile money services ----
glm_mmoney <- update(glm_account, mmoney ~ .)
saveRDS(glm_mmoney, file = here(mdl_path, "glm_mmoney.rds"))


# Probit: Using survey design ----

## Owns a financial account or mobile money ----
svyglm_account <- svyglm(
  account ~ female + age + I(age^2) + educ3c + income5c + employed + 
    mphone + year2c,
  design = design,
  family = binomial(link="probit")
)
saveRDS(svyglm_account, file = here(mdl_path, "svyglm_probit_account.rds"))

## Saved money in the past year ----
svyglm_saving <- update(svyglm_account, saving ~ .)
saveRDS(svyglm_saving, file = here(mdl_path, "svyglm_probit_saving.rds"))

## Borrowed money in the past year ----
svyglm_credit <- update(svyglm_account, credit ~ .)
saveRDS(svyglm_credit, file = here(mdl_path, "svyglm_probit_credit.rds"))

## Has used mobile phone/internet to make payments ----
# note: mobintpay - using imputed dataset
# fit svyglm
svyglm_mobintpay <- map(
  design_imp,
  ~svyglm(
    mobintpay ~ female + age + I(age^2) + educ3c + income5c + employed + 
      mphone + year2c,
    design = subset(., !is.na(mphone) & acdebit == 1),
    family = binomial(link="probit")
  )
)
saveRDS(svyglm_mobintpay, file = here(mdl_path, "svyglm_probit_mobintpay.rds"))

## Has used mobile money services ----
svyglm_mmoney <- update(svyglm_account, mmoney ~ .)
saveRDS(svyglm_mmoney, file = here(mdl_path, "svyglm_probit_mmoney.rds"))


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))