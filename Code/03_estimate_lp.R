rm(list = ls())
gc()

library(readxl)
library(dplyr)
library(tidyr)
library(broom)

df <- read_excel("Data/Cleaned/macro_dataset.xlsx")

# Check structure and variable names
str(df)
names(df)

# Set max lag for controls 
max_lag <- 12

for (l in 1:max_lag) {
  df <- df %>%
    mutate(
      !!paste0("ipi_l", l) := dplyr::lag(ip_mom, l),
      !!paste0("kpif_l", l) := dplyr::lag(infl_mom, l),
      !!paste0("shock_l", l) := dplyr::lag(mp_shock, l)
    )
}

# Choose horizon
H <- 24  # 2 years ahead

# Run LP regressions
results <- list()

for (h in 0:H) {
  
  df <- df %>%
    mutate(y_lead = dplyr::lead(ip_mom, h))
  
  model <- lm(
    y_lead ~ mp_shock +
      shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
      ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 + 
      kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6,
    data = df
  )
  
  results[[h + 1]] <- tidy(model) %>%
    filter(term == "mp_shock") %>%
    mutate(horizon = h)
}

irf <- bind_rows(results)

library(ggplot2)

ggplot(irf, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Impulse Response of Industrial Production",
    x = "Months after shock",
    y = "Response (%)"
  )


max_lag <- 6



df %>%
  select(ip_mom, ipi_l1, ipi_l2) %>%
  head(10)

cor(df$ip_mom, df$ipi_l1, use = "complete.obs")


for (h in 0:H) {
  
  df_temp <- df %>%
    mutate(y_lead = dplyr::lead(ipi_mom, h))
  
  model <- lm(
    y_lead ~ mp_shock +
      shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
      shock_l7 + shock_l8 + shock_l9 + shock_l10 + shock_l11 + shock_l12 +
      ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
      ipi_l7 + ipi_l8 + ipi_l9 + ipi_l10 + ipi_l11 + ipi_l12 +
      kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6 +
      kpif_l7 + kpif_l8 + kpif_l9 + kpif_l10 + kpif_l11 + kpif_l12,
    data = df_temp
  )
  
  coef <- summary(model)$coefficients["mp_shock", ]
  
  results[[h + 1]] <- data.frame(
    estimate = coef[1],
    std.error = coef[2],
    horizon = h
  )
}


# Recommended lead
df_temp <- df %>%
  mutate(y_lead = dplyr::lead(ip_mom, h))

data = df_temp





# Alt.

results <- list()

for (h in 0:H) {
  
  df_temp <- df %>%
    mutate(y_lead = dplyr::lead(ip_mom, h))
  
  model <- lm(
    y_lead ~ mp_shock +
      shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
      shock_l7 + shock_l8 + shock_l9 + shock_l10 + shock_l11 + shock_l12 +
      ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
      ipi_l7 + ipi_l8 + ipi_l9 + ipi_l10 + ipi_l11 + ipi_l12 +
      kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6 +
      kpif_l7 + kpif_l8 + kpif_l9 + kpif_l10 + kpif_l11 + kpif_l12,
    data = df_temp
  )
  
  coef <- summary(model)$coefficients["mp_shock", ]
  
  results[[h + 1]] <- data.frame(
    estimate = coef[1],
    std.error = coef[2],
    horizon = h
  )
}


############################LOOP##############################################

run_lp <- function(df, response_var, H = 24) {
  
  results <- list()
  
  for (h in 0:H) {
    
    df_temp <- df %>%
      mutate(y_lead = dplyr::lead(.data[[response_var]], h))
    
    model <- lm(
      y_lead ~ mp_shock +
        shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
        ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
        kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6,
      data = df_temp
    )
    
    coef <- summary(model)$coefficients["mp_shock", ]
    
    results[[h + 1]] <- data.frame(
      estimate = coef[1],
      std.error = coef[2],
      horizon = h
    )
  }
  
  dplyr::bind_rows(results)
}

# IPI
irf_ipi <- run_lp(df, "ip_mom")

ggplot(irf_ipi, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Response of Industrial Production")

# KPIF
irf_kpif <- run_lp(df, "infl_mom")

ggplot(irf_kpif, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Response of Inflation (KPIF)")


##########################ADD CI##############################################

run_lp <- function(df, response_var, H = 24) {
  
  results <- list()
  
  for (h in 0:H) {
    
    df_temp <- df %>%
      mutate(y_lead = dplyr::lead(.data[[response_var]], h))
    
    model <- lm(
      y_lead ~ mp_shock +
        shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
        ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
        kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6,
      data = df_temp
    )
    
    coef <- summary(model)$coefficients["mp_shock", ]
    
    results[[h + 1]] <- data.frame(
      estimate = coef[1],
      std.error = coef[2],
      lower = coef[1] - 1.96 * coef[2],
      upper = coef[1] + 1.96 * coef[2],
      horizon = h
    )
  }
  
  dplyr::bind_rows(results)
}

# Plot with CI

# KPIF
irf_kpif <- run_lp(df, "infl_mom")

ggplot(irf_kpif, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Response of Inflation (KPIF)")



# IPI
irf_ipi <- run_lp(df, "ip_mom")

ggplot(irf_ipi, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Response of Industrial Production")


################NEWEY-WEST STANDARD ERRORS#####################################

library(sandwich)
library(lmtest)

coeftest(model, vcov = NeweyWest(model, lag = 12))

mutate(
  lower = estimate - 1.96 * std.error,
  upper = estimate + 1.96 * std.error
)

geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)




run_lp_nw <- function(df, response_var, H = 24, nw_lag = 6) {
  
  results <- list()
  
  for (h in 0:H) {
    
    df_temp <- df %>%
      mutate(y_lead = dplyr::lead(.data[[response_var]], h))
    
    model <- lm(
      y_lead ~ mp_shock +
        shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
        ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
        kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6,
      data = df_temp
    )
    
    # Newey-West variance-covariance matrix
    nw_vcov <- NeweyWest(model, lag = nw_lag, prewhite = FALSE)
    
    # Extract coefficient + robust SE
    coef <- coeftest(model, vcov = nw_vcov)["mp_shock", ]
    
    results[[h + 1]] <- data.frame(
      estimate = coef[1],
      std.error = coef[2],
      lower = coef[1] - 1.96 * coef[2],
      upper = coef[1] + 1.96 * coef[2],
      horizon = h
    )
  }
  
  dplyr::bind_rows(results)
}

irf_ipi  <- run_lp_nw(df, "ip_mom")

ggplot(irf_ipi, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Response of Industrial Production (Newey-West CI)",
    x = "Months",
    y = "%"
  )


irf_kpif <- run_lp_nw(df, "infl_mom")

ggplot(irf_kpif, aes(x = horizon, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Response of Inflation (Newey-West CI)",
    x = "Months",
    y = "%"
  )










###############################################################################

df <- readRDS("data/processed/df_variables.rds")

# vector of variables

y_vars <- c(
  "ip_growth",
  "inflation",
  "policy_rate",
  "credit",
  "exchange_rate"
)

# core LP function
run_lp <- function(data, y_var, shock_var, controls, H = 24){
  
  results <- data.frame()
  
  for(h in 0:H){
    
    data$y_lead <- dplyr::lead(data[[y_var]], h)
    
    formula_string <- paste0(
      "y_lead ~ ", shock_var, " + ",
      paste(controls, collapse = " + ")
    )
    
    model <- fixest::feols(
      as.formula(formula_string),
      data = data
    )
    
    beta <- coef(model)[shock_var]
    se <- sqrt(vcov(model)[shock_var, shock_var])
    
    results <- rbind(results,
                     data.frame(
                       horizon = h,
                       irf = beta,
                       se = se
                     )
    )
  }
  
  results$variable <- y_var
  
  return(results)
}


# run LP for all variables

controls <- c(
  "lag(ip_growth,1:12)",
  "lag(inflation,1:12)",
  "lag(policy_rate,1:12)"
)

irf_all <- purrr::map_dfr(
  y_vars,
  ~run_lp(df, .x, "shock", controls)
)

# now irf_all contains IRFs for all variables automatically.




# LP-IV with fixest

model <- feols(
  y_lead ~ controls | shock ~ instrument,
  data = df
)


# example

model <- feols(
  y_lead ~ lag(ip_growth,1:12) + lag(inflation,1:12) |
    shock ~ hf_shock,
  data = df
)


# state-dependent LP (liquidity surplus)

formula_string <- paste0(
  "y_lead ~ shock + shock:liquidity_state + ",
  paste(controls, collapse=" + ")
)

model <- feols(
  as.formula(formula_string),
  data = df
)


# smooth state-dependent LP

gamma <- 1.5

df$G <- 1 / (1 + exp(-gamma * df$liquidity_ratio))

# then

model <- feols(
  y_lead ~ shock*G + shock*(1-G) + controls,
  data=df
)


# newey-West Standard Errors (Important for LP)

model <- feols(
  y_lead ~ shock + controls,
  data = df,
  vcov = "NW"
)

# alt. horizon-adjusted
vcov = NW ~ horizon

# bootstrap confidence intervals

boot_irf <- replicate(1000, {
  
  sample_data <- df[sample(nrow(df), replace=TRUE), ]
  
  run_lp(sample_data, "ip_growth", "shock", controls)
  
})






###Older Code

H <- 24   # horizons (months)
irf <- numeric(H)

for(h in 0:H){
  
  df$y_lead <- dplyr::lead(df$ip_growth, h)
  
  model <- feols(
    y_lead ~ shock + lag(ip_growth,1:12) + lag(inflation,1:12) + lag(policy_rate,1:12),
    data = df
  )
  
  irf[h+1] <- coef(model)["shock"]
  
}

irf_df <- data.frame(
  horizon = 0:H,
  irf = irf
)

saveRDS(irf_df,"output/irf_results.rds")



for(h in 0:H){
  
  df$y_lead <- dplyr::lead(df$ip_growth, h)
  
  model <- feols(
    y_lead ~ shock + shock:liquidity_state +
      lag(ip_growth,1:12) + lag(inflation,1:12),
    data = df
  )
  
}



# Normal regime = shock coefficient
# Liquidity surplus = shock + interaction

# estimate LP-IV

model <- feols(
  y_lead ~ 1 | shock ~ instrument,
  data = df
)