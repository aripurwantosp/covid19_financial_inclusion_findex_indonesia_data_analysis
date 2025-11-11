# R file for:
# Reporting VIF (generalized)
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
sv_name <- here(out_path, "gvif.xlsx")


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
vars[3] <- "agesq"
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
vars_lab[2:3] <- c("Age", "Age squared")
vars_lab[5] <- "Income quintile"
ref_vars <- tibble(term = vars, variable = vars_lab) %>% 
  mutate(term = factor(term, levels = .$term))


# Calculating vif ----
vif_account <- car::vif(mdl_account) %>% as.data.frame()
vif_account
vif_saving <- car::vif(mdl_saving) %>% as.data.frame()
vif_saving
vif_credit <- car::vif(mdl_credit) %>% as.data.frame()
vif_credit

vif_mobintpay <- map(
  mdl_mobintpay,
  ~car::vif(.) %>% as.data.frame(.) %>% 
    tibble::rownames_to_column(.)
)
names(vif_mobintpay) <- paste0("m",1:length(vif_mobintpay))
vif_mobintpay <- vif_mobintpay %>% 
  bind_rows(.id = "m") %>% 
  rename(term = rowname) %>% 
  group_by(term) %>% 
  summarise_if(is.double, ~max(.)) %>% 
  ungroup() %>% 
  mutate(
    term = ifelse(term == "I(age^2)", "agesq", term),
    term = factor(term, levels = vars)
  ) %>% 
  arrange(term)
vif_mobintpay

vif_mmoney <- car::vif(mdl_mmoney) %>% as.data.frame()
vif_mmoney

# vif all (adjusted)
vif_all <- tibble(
  term = ref_vars$term,
  formal_account = vif_account[,3],
  formal_saving = vif_saving[,3],
  formal_credit = vif_credit[,3],
  mob_payment = pull(vif_mobintpay[,4]),
  mmoney_services = vif_mmoney[,3],
) %>% 
  right_join(ref_vars, by = "term") %>% 
  relocate(variable, .after = "term")
vif_all


# Save to excel ----
write_xlsx(vif_all, path = sv_name)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))