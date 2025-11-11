# R file for:
# Forest plot for sensitivity analysis
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
# 2025/10/13


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(out_path, "ame_main_sensitivity.png")
sv_name2 <- here(out_path, "ame_fintech_sensitivity.png")


# Read data ----
# reference for variable label
# note: ref_vars.rds from 6_reporting_ame_main_analysis.R
ref_vars <- readRDS(here(mdl_ame_path, "ref_vars.rds"))


# Read and modified AME data ----
# Main analysis
ame_main_main <- readRDS(here(mdl_ame_path, "ame_main_main.rds"))
ame_main_main <- ame_main_main %>% 
  mutate(model = "Main") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

ame_fintech_main <- readRDS(here(mdl_ame_path, "ame_fintech_main.rds"))
ame_fintech_main <- ame_fintech_main %>% 
  mutate(model = "Main") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

# Sensitivity analysis 1
ame_main_sens1 <- readRDS(here(mdl_ame_path, "ame_main_sensitivity1.rds"))
ame_main_sens1 <- ame_main_sens1 %>% 
  mutate(model = "Sensitivity 1") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

ame_fintech_sens1 <- readRDS(here(mdl_ame_path, "ame_fintech_sensitivity1.rds"))
ame_fintech_sens1 <- ame_fintech_sens1 %>% 
  mutate(model = "Sensitivity 1") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

# Sensitivity analysis 2
ame_main_sens2 <- readRDS(here(mdl_ame_path, "ame_main_sensitivity2.rds"))
ame_main_sens2 <- ame_main_sens2 %>% 
  mutate(model = "Sensitivity 2") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

ame_fintech_sens2 <- readRDS(here(mdl_ame_path, "ame_fintech_sensitivity2.rds"))
ame_fintech_sens2 <- ame_fintech_sens2 %>% 
  mutate(model = "Sensitivity 2") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

# Sensitivity analysis 3
ame_main_sens3 <- readRDS(here(mdl_ame_path, "ame_main_sensitivity3.rds"))
ame_main_sens3 <- ame_main_sens3 %>% 
  mutate(model = "Sensitivity 3") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()

ame_fintech_sens3 <- readRDS(here(mdl_ame_path, "ame_fintech_sensitivity3.rds"))
ame_fintech_sens3 <- ame_fintech_sens3 %>% 
  mutate(model = "Sensitivity 3") %>% 
  group_split(indicator) %>% 
  map(., ~ame_add_var_head(., ref_vars)) %>% 
  bind_rows()


# Pooled AME data ----
ame_main_pooled <- bind_rows(
  ame_main_main,
  ame_main_sens1,
  ame_main_sens2,
  ame_main_sens3
) %>% 
  mutate(
    level = ifelse(!str_detect(level, "^\\s+"), paste0("**", level, "**"), level),
    model = factor(model, levels = c("Main", paste("Sensitivity", 1:3))),
    indicator = factor(indicator, levels = c("Formal account", "Formal saving", "Formal credit"))
  )

ame_fintech_pooled <- bind_rows(
  ame_fintech_main,
  ame_fintech_sens1,
  ame_fintech_sens2,
  ame_fintech_sens3
) %>% 
  mutate(
    level = ifelse(!str_detect(level, "^\\s+"), paste0("**", level, "**"), level),
    model = factor(model, levels = c("Main", paste("Sensitivity", 1:3))),
    indicator = factor(indicator, levels = c("Payment using Mobile", "Mobile Money Services"))
  )


# Make forest plot ----

## plot setting ----
# x label
x_lab <- "Average marginal effect (AME) and 95% CI"

# position dodge (cross model)
pd <- ggstance::position_dodgev(height = 0.65)

# theme
theme_opt <- theme_minimal() +
  theme(
    strip.background = element_rect(fill = "black"),
    strip.text = element_text(size = 10, color = "white", face = "bold"),
    axis.title = element_text(size = 12, color = "black"),
    axis.text = element_markdown(size = 10, color = "black"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom"
    # panel.grid.major.y = element_blank() # hilangkan grid default
  )

## main indicator ----
plt_main <- ggplot(ame_main_pooled, aes(x = ame, y = level_idx, group = model)) +

  # tambahkan garis horizontal hanya di baris heading
  geom_hline(
    data = ame_main_pooled %>% 
      filter(!str_detect(level, "^\\s+")), # heading
    aes(yintercept = level_idx),
    linewidth = .5, color = "grey", alpha = .4
  ) +
  geom_vline(xintercept = 0, linewidth = .25) +
  
  # main element: ame, 95%ci
  geom_point(
    aes(color = model),
    position = pd,
    size = 1.25,
    na.rm = TRUE
  ) +
  geom_errorbarh(
    aes(xmin = low, xmax = high, color = model),
    position = pd,
    linewidth = .25, 
    height = 0,
    na.rm = TRUE
  ) +
  
  # axis label
  labs(x = x_lab, y = NULL, color = NULL) +
  
  # scale color
  ggthemes::scale_color_stata() +

  # axis scale
  scale_y_continuous(breaks = ame_main_pooled$level_idx[1:18], labels = ame_main_pooled$level[1:18]) +
  scale_x_continuous(breaks = function(x) {pretty_breaks()(x) %>% union(1)}) +

  # facet
  facet_grid(. ~ indicator) +
  # theme
  theme_opt
plt_main

# save
ggsave(filename = sv_name, width = 7.5, height = 10, units = "in", dpi = 300)

## fintech ----
plt_fintech <- ggplot(ame_fintech_pooled, aes(x = ame, y = level_idx, group = model)) +

  # tambahkan garis horizontal hanya di baris heading
  geom_hline(
    data = ame_fintech_pooled %>% 
      filter(!str_detect(level, "^\\s+")), # heading
    aes(yintercept = level_idx),
    linewidth = .5, color = "grey", alpha = .4
  ) +
  geom_vline(xintercept = 0, linewidth = .25) +
  
  # main element: ame, 95%ci
  geom_point(
    aes(color = model),
    position = pd,
    size = 1.25,
    na.rm = TRUE
  ) +
  geom_errorbarh(
    aes(xmin = low, xmax = high, color = model),
    position = pd,
    linewidth = .25, 
    height = 0,
    na.rm = TRUE
  ) +
  
  # axis label
  labs(x = x_lab, y = NULL, color = NULL) +
  
  # scale color
  ggthemes::scale_color_stata() +

  # axis scale
  scale_y_continuous(breaks = ame_fintech_pooled$level_idx[1:18], labels = ame_fintech_pooled$level[1:18]) +
  scale_x_continuous(breaks = function(x) {pretty_breaks()(x) %>% union(1)}) +

  # facet
  facet_grid(. ~ indicator) +
  # theme
  theme_opt
plt_fintech

# save
ggsave(filename = sv_name2, width = 7, height = 9, units = "in", dpi = 300)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))