# State-Space (final)

df <- df %>%
  mutate(
    mpliab = scale(mpliab)  # standardize (important)
  )

df <- df %>%
  mutate(
    shock_liq = mp_shock * mpliab
  )



# Modified LP model

run_lp_state <- function(df, response_var, H = 24, nw_lag = 6) {
  
  results <- list()
  
  for (h in 0:H) {
    
    df_temp <- df %>%
      mutate(y_lead = dplyr::lead(.data[[response_var]], h))
    
    model <- lm(
      y_lead ~ mp_shock + shock_liq +
        shock_l1 + shock_l2 + shock_l3 + shock_l4 + shock_l5 + shock_l6 +
        ipi_l1 + ipi_l2 + ipi_l3 + ipi_l4 + ipi_l5 + ipi_l6 +
        kpif_l1 + kpif_l2 + kpif_l3 + kpif_l4 + kpif_l5 + kpif_l6,
      data = df_temp
    )
    
    nw_vcov <- sandwich::NeweyWest(model, lag = nw_lag, prewhite = FALSE)
    coefs <- lmtest::coeftest(model, vcov = nw_vcov)
    
    results[[h + 1]] <- data.frame(
      beta_base = coefs["mp_shock", 1],
      beta_liq  = coefs["shock_liq", 1],
      horizon = h
    )
  }
  
  dplyr::bind_rows(results)
}


# Plot at different liquidity levels

# IPI response
irf_state <- run_lp_state(df, "ip_mom")

# Example: low vs high liquidity
irf_state <- irf_state %>%
  mutate(
    low_liq  = beta_base + beta_liq * (-1),
    high_liq = beta_base + beta_liq * (1)
  )

# Plot
ggplot(irf_state, aes(x = horizon)) +
  geom_line(aes(y = low_liq, color = "Low liquidity")) +
  geom_line(aes(y = high_liq, color = "High liquidity")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "State-Dependent IRF (Liquidity)")


# KPIF response
irf_state <- run_lp_state(df, "infl_mom")

# Example: low vs high liquidity
irf_state <- irf_state %>%
  mutate(
    low_liq  = beta_base + beta_liq * (-1), # 1 SD below mean
    high_liq = beta_base + beta_liq * (1) # 1 SD above mean
  )

# Plot
ggplot(irf_state, aes(x = horizon)) +
  geom_line(aes(y = low_liq, color = "Low liquidity")) +
  geom_line(aes(y = high_liq, color = "High liquidity")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "State-Dependent IRF (Liquidity)")




irf_df <- readRDS("output/irf_results.rds")

# confidence intervals

irf_all <- irf_all %>%
  mutate(
    upper = irf + 1.96*se,
    lower = irf - 1.96*se
  )

# plot all IRFs

ggplot(irf_all, aes(x=horizon, y=irf)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
  facet_wrap(~variable, scales="free_y") +
  theme_minimal


# always save output
saveRDS(irf_all, "output/irfs_main_model.rds")





#### Older code

ggplot(irf_df, aes(x=horizon, y=irf)) +
  geom_line(size=1) +
  geom_hline(yintercept=0, linetype="dashed") +
  theme_minimal() +
  labs(
    x="Months",
    y="Response",
    title="Impulse Response to Monetary Policy Shock"
  )