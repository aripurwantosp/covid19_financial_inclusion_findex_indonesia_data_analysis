# R file for:
# Reporting descriptive statistics
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
# 2025/11/05


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(out_path, "table_variable_summary_statistics.docx")
sv_name2 <- here(out_path, "table_variable_summary_mobintpay.docx")


# Read data ----
df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
df <- readRDS(df_path)

# # modify mobintpay (add has not account or debit) showing contditional on acdebit
df <- df %>% 
  mutate(
    mobintpay_new = case_when(
      acdebit == 0 ~ 0,
      is.na(acdebit) ~ 1,
      mobintpay == "No" ~ 2,
      mobintpay == "Yes" ~ 3,
    ),
    mobintpay_new = factor(
      mobintpay_new,
      levels = 0:3,
      labels = c("Has account or debit: No",
                 "Has account or debit: Missing",
                 "No",
                 "Yes"
                )
    )
  )
attr(df$mobintpay_new, "label") <- attr(df$mobintpay, "label")


# Missing data ----
# note:
# -variables for all respondents
# -NA: "don't know" or "refused"
df %>% 
  dplyr::select(-mobintpay) %>% 
  funModeling::df_status(.)

# # mphone_mi, acdebit, mobintpay_mi for frequency check
# df <- df %>% 
#   mutate(
#       mphone_mi = ifelse(is.na(mphone), 1, 0),
#       # has account or debit card
#       acdebit = ifelse(account == "Yes" | debitcard == "Yes", 1 ,0),
#       mobintpay_mi = ifelse(is.na(mobintpay), 1, 0)
#     )

# frequency of mphone's missing by wave
# unweighted
df %>% group_by(year2c, mphone) %>% summarise(n = n())
# weighted
design <- svydesign(
  ids = ~1, 
  weights = ~wgt, 
  data = df
)
prop.table(svytable(~year2c + mphone_mi, design = design), 1)

# frequency of debitcard's missing by wave
# unweighted
df %>% group_by(year2c, debitcard) %>% summarise(n = n())
# weighted
prop.table(svytable(~year2c + debitcard_mi, design = design), 1)

# frequency of mobintpay's msising by wave
# mobintpay -> if has account or has a debit card
# unweighted
df %>% group_by(year2c, acdebit) %>% summarise(n = n())
df %>% filter(acdebit == 1) %>% 
  group_by(year2c, mobintpay) %>% 
  summarise(n=n())
# weighted
prop.table(svytable(~year2c + mobintpay_mi, design = subset(design, acdebit == 1)), 1)


# Variable's summary statistics ----
# unweighted n
tbl_uw <- df %>% 
  tbl_summary(
    type = list(all_dichotomous() ~ "categorical"),
    by = year2c,
    include = c(account, saving, credit, mobintpay_new, mmoney,
                female, age, educ3c, employed, income5c, mphone),
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ 2),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
  ) %>% 
  add_overall(
    col_label = "**Total**  \nN = {style_number(N)}"
  )

# weighted %
tbl_w <- design %>% 
  tbl_svysummary(
    type = list(all_dichotomous() ~ "categorical"),
    by = year2c,
    include = c(account, saving, credit, mobintpay_new, mmoney,
                female, age, educ3c, employed, income5c, mphone),
    statistic = list(all_categorical() ~ "({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ 2,
                  all_continuous() ~ 2),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "({p_miss}%)",
  ) %>% 
   add_overall(
    col_label = "**Total**  \nN = {style_number(N)}"
  )
  
# merge table
tbl_m <- tbl_merge(
  tbls = list(tbl_uw, tbl_w),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>% 
  modify_header(label ~ "**Variables**") %>%
  add_variable_group_header(
    header = "Main indicators of financial inclusion",
    variables = c(account, saving, credit)
  ) %>%
  add_variable_group_header(
    header = "Financial technology",
    variables = c(mobintpay_new, mmoney)
  ) %>% 
  add_variable_group_header(
    header = "Explanatory variables",
    variables = c(female, age, educ3c, employed, income5c, mphone)
  ) %>% 
  modify_table_styling(
    columns = label,
    rows = row_type %in% c("variable_group", "label"),
    text_format = "bold"
  )
tbl_m

idx_pad <- which(tbl_m$table_body$row_type %in% c("level", "missing"))
idx_grp <- which(tbl_m$table_body$row_type == "variable_group")

# save to docx
# document settings
sect_prop <- officer::prop_section(
  page_size = officer::page_size(orient = "landscape")
)

tbl_m %>%
  as_flex_table() %>%
  padding(i = idx_pad, j = 1, padding.left = 20) %>% 
  align(j = -1, align = "right", part = "body") %>% 
  hline(i = c(idx_grp, idx_grp[-1]-1)) %>% 
  italic(i = idx_grp, italic = TRUE, part = "body") %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  save_as_docx(path = sv_name, pr_section = sect_prop)


# Mobintpay: subset !is.na(mphone) and acdebit == 1 ----
# unweighted n
tbl_uw <- df %>%
  filter(!is.na(mphone), acdebit == 1) %>% 
  tbl_summary(
    type = list(all_dichotomous() ~ "categorical"),
    by = year2c,
    include = mobintpay,
    statistic = list(all_categorical() ~ "{n} ({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ c(0,2),
                  all_continuous() ~ 2),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "{N_miss} ({p_miss}%)",
  ) %>% 
  add_overall(
    col_label = "**Total**  \nN = {style_number(N)}"
  )

sub_design <- subset(design, !is.na(mphone) & acdebit == 1)
attr(sub_design$variables$mobintpay, "label") <- attr(df$mobintpay, "label")

# weighted %
tbl_w <- sub_design %>% 
  tbl_svysummary(
    type = list(all_dichotomous() ~ "categorical"),
    by = year2c,
    include = mobintpay,
    statistic = list(all_categorical() ~ "({p}%)",
                     all_continuous() ~ "{mean} ({sd})"),
    digits = list(all_categorical() ~ 2,
                  all_continuous() ~ 2),
    missing = "ifany",
    missing_text = "Missing",
    missing_stat = "({p_miss}%)",
  ) %>% 
   add_overall(
    col_label = "**Total**  \nN = {style_number(N)}"
  )

# merge table
tbl_m <- tbl_merge(
  tbls = list(tbl_uw, tbl_w),
  tab_spanner = c("**Unweighted**", "**Weighted**")
) %>% 
  modify_header(label ~ "**Variable**") %>%
  modify_table_styling(
    columns = label,
    rows = row_type == "label",
    text_format = "bold"
  )
tbl_m

# save to docx
tbl_m %>% 
  as_flex_table() %>%
  align(j = -1, align = "right", part = "body") %>% 
  font(fontname = "Times New Roman", part = "all") %>%
  save_as_docx(path = sv_name2, pr_section = sect_prop)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))