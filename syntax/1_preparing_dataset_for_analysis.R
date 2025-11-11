# R file for:
# Preparing dataset for analysis
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
# 2025/10/06


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")


# Read data ----
df_path <- here(dta_path, "gcfin1721-final-dataset.dta")
df <- read_dta(df_path)
df <- df %>% relocate(year2c, .before = "wpid_random") %>% 
  relocate(wgt, .after = "wpid_random")
glimpse(df)


# Scaling weight ----
df <- df %>% 
  group_by(year2c)

# check weight type: normalized or expanded to population
# the weight is proportionally per wave sample size
df %>% summarize(
  n_wave = n(), 
  sum_wgt = sum(wgt),
  ratio = sum_wgt / n_wave
)

# # scaling weight
# df <- df %>% 
#   mutate(wgt_scl = wgt * (n()/sum(wgt))) %>% 
#   ungroup() %>% 
#   relocate(wgt_scl, .after = "wgt")

# # check scaled weight
# df %>% group_by(year2c) %>% 
#   summarize(n = n(), sum_scaled_wgt = sum(wgt_scl))


# Renaming and relabel variables ----
# Rename some variables
df <- df %>% 
  rename(
    account   = acc_fin, 
    credit    = borrowed, 
    saving    = saved,
    mobintpay = mob_int_pay,
    mmoney    = mo_money,
    debitcard = debit_card
  ) 

# Remove the value labels for age & age2
attributes(df$age)  <- NULL
attributes(df$agesq) <- NULL

# as_factor
df <- as_factor(df)

# Rename income labels
df$income5c <- forcats::fct_relabel(
  df$income5c, ~stringr::str_remove(.,"Income-") %>% stringr::str_to_title(.)
)

# Rename year2c labels
df$year2c <- forcats::fct_relabel(
  df$year2c, ~stringr::str_extract(., "^[^,]+")
)

# Fix variable label

attr(df$account, "label")  <- "Formal account"
attr(df$credit, "label")   <- "Formal credit"
attr(df$saving, "label")   <- "Formal saving"
attr(df$age, "label")      <- "Age (years)"
attr(df$agesq, "label")    <- "Age (years), squared"
attr(df$employed, "label") <- "Currently employed"

# Select all variable used in analysis only
df <- df %>% 
  dplyr::select(
    year2c,
    wpid_random,
    wgt,
    account, debitcard, saving, credit,
    mobintpay, mmoney,
    female, age, agesq, educ3c, income5c, employed, mphone
  ) %>% 
  ungroup()

# Generate variable for assessing missing values:
# mphone_mi, acdebit, mobintpay_mi for frequency check
df <- df %>% 
  mutate(
      # missing of mphone
      mphone_mi = ifelse(is.na(mphone), 1, 0),
      # missing of debit card
      debitcard_mi = ifelse(is.na(debitcard), 1, 0),
      # has account or debit card
      acdebit = ifelse(account == "Yes" | debitcard == "Yes", 1 ,0),
      # missing of mobintpay
      mobintpay_mi = ifelse(is.na(mobintpay), 1, 0)
    )


# Save
saveRDS(df, file = sv_name)

# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))
