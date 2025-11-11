# R file for:
# Coefficients of models for main analysis
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


# Saved name ----
sv_name <- here(out_path, "coefs_main_analysis.xlsx")


# # Read data ----
# # non-imputed dataset
# df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
# df_full <- readRDS(df_path)
# df <- df_full %>% filter(!is.na(mphone))


# Read models ----
mdl_account <- readRDS(here(mdl_path, "svyglm_account.rds"))
mdl_saving <- readRDS(here(mdl_path, "svyglm_saving.rds"))
mdl_credit <- readRDS(here(mdl_path, "svyglm_credit.rds"))
mdl_mobintpay <- readRDS(here(mdl_path, "svyglm_mobintpay.rds"))
mdl_mmoney <- readRDS(here(mdl_path, "svyglm_mmoney.rds"))


# Extract coefs ----

## main indicators ----
main_coefs <- list(
  `Formal account` = tidy(mdl_account),
  `Formal saving` = tidy(mdl_saving),
  `Formal credit` = tidy(mdl_credit)
) %>% 
  bind_rows(.id = "indicators") %>% 
  mutate(
    sig = stars(`p.value`),
    coef = fmt_number(estimate),
    coef = str_c(coef, sig),
    se = fmt_number(std.error, width = 5)
  ) %>% 
  dplyr::select(indicators, term, coef, se)

## fintech ----
# mobintpay
ests <- map(mdl_mobintpay, ~coef(.))
vcovs <- map(mdl_mobintpay, ~vcov(.))
infer <- mitools::MIcombine(results = ests, variances = vcovs)
coef_mobintpay = tibble(
  term = names(infer$coefficients),
  coef = infer$coefficients,
  se = sqrt(diag(infer$variance)),
  df = infer$df
) %>% 
  mutate(
    tvalue = coef/se,
    pvalue = 2 * (1 - pt(abs(tvalue), df)),
    sig = stars(pvalue),
    coef = fmt_number(coef),
    coef = str_c(coef, sig),
    se = fmt_number(se, width = 5)
  ) %>% 
  dplyr::select(term, coef, se)

fintech_coefs <- list(
  `Used mobile phone or internet to make payments` = coef_mobintpay,
  `Used mobile money services in the last 12 months` = tidy(mdl_mmoney) %>% 
    mutate(
    sig = stars(`p.value`),
    coef = fmt_number(estimate),
    coef = str_c(coef, sig),
    se = fmt_number(std.error, width = 5)
  ) %>% 
    dplyr::select(term, coef, se)
) %>% 
  bind_rows(.id = "indicator")
  
# save to excel
write_xlsx(
  x = list(coef_main = main_coefs, coef_fintech = fintech_coefs),
  path = sv_name
)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))