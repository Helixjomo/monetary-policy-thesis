df <- readRDS("data/processed/df_clean.rds")

df <- df %>%
  mutate(
    inflation = 100 * (log(cpi) - log(lag(cpi,1))),
    ip_growth = 100 * (log(ip) - log(lag(ip,1))),
    policy_rate = rate,
    liquidity_ratio = reserves / gdp
  )

# Example state variable (liquidity surplus)
df <- df %>%
  mutate(
    liquidity_state = ifelse(liquidity_ratio > median(liquidity_ratio, na.rm=TRUE),1,0)
  )

saveRDS(df, "data/processed/df_variables.rds")