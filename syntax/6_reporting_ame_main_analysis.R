# R file for:
# Reporting models for main analysis
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
# 2025/10/10


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(out_path, "ame_main_analysis.xlsx")
sv_name2 <- here(mdl_ame_path, "ame_main_main.rds")
sv_name3 <- here(mdl_ame_path, "ame_fintech_main.rds")
sv_name4 <- here(mdl_ame_path, "ref_vars.rds")


# Read data ----
# non-imputed dataset
df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
df_full <- readRDS(df_path)
df <- df_full %>% filter(!is.na(mphone))

# # imputed dataset (for mobintpay)
# df_path_imp <- here(dta_path, "gcfin1721_final_dataset_for_analysis_imp.rds")
# df_imp <- readRDS(df_path_imp)


# # Set svy design
# design <- svydesign(ids = ~1, weights = ~wgt, data = df)
# design_imp <- map(
#   df_imp,
#   ~svydesign(ids = ~1, weights = ~wgt, data = .)
# )


# Read models ----
mdl_account <- readRDS(here(mdl_path, "svyglm_account.rds"))
mdl_saving <- readRDS(here(mdl_path, "svyglm_saving.rds"))
mdl_credit <- readRDS(here(mdl_path, "svyglm_credit.rds"))
mdl_mobintpay <- readRDS(here(mdl_path, "svyglm_mobintpay.rds"))
mdl_mmoney <- readRDS(here(mdl_path, "svyglm_mmoney.rds"))


# Reference for variable label ----
vars <- colnames(model.frame(mdl_account))[2:9]
vars <- vars[-3] #remove "I(age^2)"
vars_ref <- map_chr(vars, 
  ~{
    ref <- levels(df[[.]])
    ifelse(is.null(ref), "", ref[1])
  }
)
vars_lab <- map_chr(
  vars,
  ~attr(df[[.]], "label")
)
vars_lab[1] <- "Female"
vars_lab[2] <- "Age"
vars_lab[4] <- "Income quintile"
vars_lab <- map2_chr(vars_lab, vars_ref,
  ~ifelse(.y != "", paste0(.x, " (ref: ", .y, ")"), .x)
)
ref_vars <- tibble(term = vars, variable = vars_lab) %>% 
  mutate(term = factor(term, levels = .$term))
saveRDS(ref_vars, file = sv_name4)


# AME ----
## Main financial inclusion indicators ----
# note: utilizing custom functions in utils_fun.R to extract and reshaping results data
ame_main <- bind_rows(
  mod_tidy_num(tidy_ame(ame(mdl_account))) %>% 
    mutate(indicator = "Formal account"),
  mod_tidy_num(tidy_ame(ame(mdl_saving))) %>% 
    mutate(indicator = "Formal saving"),
  mod_tidy_num(tidy_ame(ame(mdl_credit))) %>% 
    mutate(indicator = "Formal credit")
)

# save
saveRDS(ame_main, file = sv_name2)

# to char
ame_main <- mod_tidy_char(ame_main)

# reshaping to wide
ame_main_new <- ame_main %>% 
  mutate(term = factor(term, levels = ref_vars$term)) %>%
  left_join(ref_vars, by = "term")
ame_main_wide <- ame_long_to_wide(ame_main_new)


## Use of financial technologies ----
ame_fintech <- bind_rows(
  mod_tidy_num(tidy_ame_mi(ame_mi(mdl_mobintpay))) %>% 
    mutate(indicator = "Payment using Mobile"),
  mod_tidy_num(tidy_ame(ame(mdl_mmoney))) %>% 
    mutate(indicator = "Mobile Money Services")
)

# save
saveRDS(ame_fintech, file = sv_name3)

# to char
ame_fintech <- mod_tidy_char(ame_fintech)

# reshaping to wide
ame_fintech_new <- ame_fintech %>% 
  mutate(term = factor(term, levels = ref_vars$term)) %>%
  left_join(ref_vars, by = "term")
ame_fintech_wide <- ame_long_to_wide(ame_fintech_new)


# Model statistics ----
## Main financial inclusion indicators ----
formal_account <- c(
  n = length(mdl_account$fitted.values),
  r2_tjur = tjur_r2(mdl_account),
  au <- auc(mdl_account)
)

formal_saving <- c(
  n = length(mdl_saving$fitted.values),
  r2_tjur = tjur_r2(mdl_saving),
  au <- auc(mdl_saving)
)

formal_credit <- c(
  n = length(mdl_credit$fitted.values),
  r2_tjur = tjur_r2(mdl_credit),
  au <- auc(mdl_credit)
)

mdl_stat_main <- tibble(
  stat = c("Observations", "Tjur's R2", "AUC"),
  formal_account = formal_account,
  formal_saving = formal_saving,
  formal_credit = formal_credit
)

## Use of financial technologies ----
mob_payment <- c(
  n = length(mdl_mobintpay[[1]]$fitted.values),
  r2_tjur = tjur_r2_mi(mdl_mobintpay)[1], #mean
  au = auc_mi(mdl_mobintpay)[1] #mean
)

mmoney_services <- c(
  n = length(mdl_mmoney$fitted.values),
  r2_tjur = tjur_r2(mdl_mmoney),
  au <- auc(mdl_mmoney)
)

mdl_stat_fintech <- tibble(
  stat = c("Observations", "Tjur's R2", "AUC"),
  mob_payment = mob_payment,
  mmoney_services = mmoney_services,
)

# Save to excel ----
write_xlsx(
  x = list(
    ame_main = ame_main_wide, 
    mdl_stat_main = mdl_stat_main,
    ame_fintech = ame_fintech_wide,
    mdl_stat_fintech = mdl_stat_fintech
  ),
  path = sv_name
)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))