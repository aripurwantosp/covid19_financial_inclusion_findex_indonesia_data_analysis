# R file for:
# Average marginal predicted probabilities for age
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
# 2026/01/15


# initial objects
init_obj <- ls()


# time start, system info
time_start <- Sys.time()
time_start
benchmarkme::get_cpu()
benchmarkme::get_ram()
devtools::session_info()


# Saved name ----
sv_name <- here(out_path, "predicted_main.png")
sv_name2 <- here(out_path, "predicted_fintech.png")


# Read data ----
# non-imputed dataset
df_path <- here(dta_path, "gcfin1721_final_dataset_for_analysis.rds")
df_full <- readRDS(df_path)
df <- df_full %>% filter(!is.na(mphone))


# Read models ----
mdl_account <- readRDS(here(mdl_path, "svyglm_account.rds"))
mdl_saving <- readRDS(here(mdl_path, "svyglm_saving.rds"))
mdl_credit <- readRDS(here(mdl_path, "svyglm_credit.rds"))
mdl_mobintpay <- readRDS(here(mdl_path, "svyglm_mobintpay.rds"))
mdl_mmoney <- readRDS(here(mdl_path, "svyglm_mmoney.rds"))


# Make average predicted probability and optimum age plot ----

# plot settings
theme_opt <- theme(
    axis.title = element_text(size = 8)
  )

## Main financial indicators ----
# make list plot
plt_main <- list(
  mdl_account, mdl_saving, mdl_credit
) %>% 
  map(
    ~{
      optage <- opt_age(.)
      dep <- model.frame(.) %>%
        colnames()
      dep <- dep[1]
      ylab <- tolower(attr(df[[dep]], "label"))
      pred <- predict_response(
        .,
        terms = "age",
        margin = "empirical",
        weights = weights(.$survey.design)
      )
      plt <-  plot(pred)

      # draw optimum age if not formal saving, because quadratic coef very small
      if(ylab != "formal saving"){
        plt <- plt +
          geom_vline(
          xintercept = optage, 
          color = "red", 
          linetype = "dashed"
        ) +
          annotate(
          "text",
          x = optage,
          y = max(pred$predicted),
          label = paste("Age = ", round(optage, 2)),
          vjust = -0.5,
          hjust = -.1,
          color = "red"
        )
      }
      
      plt <- plt +
        labs(
          x = "Age",
          y = paste0("Average predicted probabilities of\n", ylab),
          title = NULL
        ) +
        theme_opt
        
      plt
    }
  )

# wrap plots
plt_main_wrap <- wrap_plots(
  plt_main, 
  ncol = 2
) +
  plot_annotation(tag_levels = "A")
plt_main_wrap

# save
ggsave(sv_name, plt_main_wrap, width = 7, height = 6, units = "in", dpi = 300)

## fintech ----

plt_fintech <- list()

### mobintpay ----
# note: quadratic term was non-statistically significant

# predicted probability
pred_mobintpay <- mdl_mobintpay %>% 
  map(
    ~predict_response(
        .,
        terms = "age",
        margin = "empirical",
        weights = weights(.$survey.design)
      )
  )
names(pred_mobintpay) <- paste0("m",1:length(pred_mobintpay))
pred_mobintpay_pooled <- pool_predictions(pred_mobintpay)

# plot
dep <- model.frame(mdl_mobintpay[[1]]) %>% colnames()
dep <- dep[1]
ylab <- tolower(attr(df[[dep]], "label"))

plt_fintech[[1]] <- plot(pred_mobintpay_pooled) +
  labs(
    x = "Age",
    y = paste0("Average predicted probabilities of\n", ylab),
    title = NULL
  ) +
  theme_opt

### mmoney ----
# note: quadratic term was non-statistically significant
# predicted probability
pred <- mdl_mmoney %>% 
  predict_response(
    terms = "age",
    margin = "empirical",
    weights = weights(.$survey.design)
  )

# plot
dep <- model.frame(mdl_mmoney) %>% colnames()
dep <- dep[1]
ylab <- tolower(attr(df[[dep]], "label"))

plt_fintech[[2]] <- plot(pred) +
  labs(
    x = "Age",
    y = paste0("Average predicted probabilities of\n", ylab),
    title = NULL
  ) +
  theme_opt

### wraps plot & save ----
plt_fintech_wrap <- wrap_plots(
  plt_fintech, 
  ncol = 2
) +
  plot_annotation(tag_levels = "A")
plt_fintech_wrap

# save
ggsave(sv_name2, plt_fintech_wrap, width = 7, height = 3, units = "in", dpi = 300)


# time end
time_end <- Sys.time()
time_end
time_exec <- time_end - time_start
time_exec


# remove all objects which are created in current r script file
rm(list = setdiff(ls(), init_obj))