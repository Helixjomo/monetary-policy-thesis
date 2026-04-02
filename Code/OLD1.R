# Core
library(tidyverse)
library(lubridate)
library(zoo)

# LP estimation
library(fixest)        # fast FE + clustering
library(lpirfs)        # optional (for single-country checks)

# Inference
library(sandwich)
library(modelsummary)
library(lmtest)

# LP helper (optional but useful)
library(fixest)   # for fast regressions with clustering

# .xlsx file reading
library(readxl)



# ---- Load Swedish MP shocks ----

# Check sheets
excel_sheets("Data/Raw/database-for-event-studies-of-swedish-monetary-policy.xlsx")

# Import
shocks <- read_excel(
  "Data/Raw/database-for-event-studies-of-swedish-monetary-policy.xlsx",
  sheet = "Database"
)


shocks <- read_csv(
  "Data/Raw/database-for-event-studies-of-swedish-monetary-policy.xlsx"
)

shocks <- shocks %>%
  mutate(
    date = as.Date(date),
    mp_shock      = mp,
    unconv_shock  = unconv,
    info_shock    = info
  ) %>%
  select(date, mp_shock, unconv_shock, info_shock)


macro <- read_csv("data_raw/macro_panel.csv")

macro <- macro %>%
  mutate(
    date = as.Date(date),
    country = as.factor(country)
  )

# Merge shocks (ECB shocks are common)
panel <- macro %>%
  left_join(shocks, by = "date")


panel <- panel %>%
  mutate(
    regime = case_when(
      date < as.Date("2008-09-01") ~ "pre_gfc",
      date >= as.Date("2008-09-01") & date < as.Date("2015-01-01") ~ "gfc_elb",
      date >= as.Date("2020-03-01") & date < as.Date("2022-12-01") ~ "covid",
      TRUE ~ "post_covid"
    ),
    regime = factor(
      regime,
      levels = c("pre_gfc", "gfc_elb", "covid", "post_covid")
    )
  )


H <- 20   # horizons

panel_lp <- panel %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(
    y = gdp_growth
  )

for (h in 0:H) {
  panel_lp <- panel_lp %>%
    mutate(!!paste0("y_lead_", h) := lead(y, h))
}


h <- 8

lp_h <- feols(
  y_lead_8 ~ 
    mp_shock:regime +
    unconv_shock:regime +
    info_shock +
    lag(y, 1:4) |
    country,
  data = panel_lp,
  cluster = ~ country
)

summary(lp_h)



lp_results <- map_df(0:H, function(h) {
  
  fml <- as.formula(
    paste0(
      "y_lead_", h, " ~ ",
      "mp_shock:regime + unconv_shock:regime + info_shock + lag(y, 1:4) | country"
    )
  )
  
  est <- feols(
    fml,
    data = panel_lp,
    cluster = ~ country
  )
  
  broom::tidy(est) %>%
    mutate(h = h)
})


lp_results %>%
  filter(str_detect(term, "mp_shock")) %>%
  ggplot(aes(h, estimate, color = term)) +
  geom_line() +
  geom_ribbon(
    aes(ymin = estimate - 1.96 * std.error,
        ymax = estimate + 1.96 * std.error),
    alpha = 0.2
  ) +
  theme_minimal() +
  labs(
    title = "GDP response to ECB monetary policy shocks",
    x = "Horizon",
    y = "Response"
  )



# Shadow rate incorporation

shadow <- read_csv("data_raw/wu_xia_ea_shadow_rate.csv") %>%
  mutate(date = as.Date(date))

panel <- panel %>%
  left_join(shadow, by = "date")


panel <- panel %>%
  mutate(
    sr_regime = case_when(
      shadow_rate > 0 ~ "normal",
      shadow_rate <= 0 & shadow_rate > -2 ~ "elb",
      shadow_rate <= -2 ~ "deep_elb"
    ),
    sr_regime = factor(sr_regime)
  )


# Main LP regression

feols(
  y_lead_h ~
    mp_shock:sr_regime +
    unconv_shock:sr_regime +
    info_shock +
    lag(y, 1:4) |
    country,
  data = panel,
  cluster = ~ country
)




panel <- panel %>%
  mutate(
    shadow_regime = case_when(
      shadow_rate > 0 ~ "normal",
      shadow_rate <= 0 & shadow_rate > -2 ~ "mild_ELB",
      shadow_rate <= -2 ~ "deep_ELB"
    )
  )


y_lead_h ~
  mp_shock:shadow_regime +
  unconv_shock:shadow_regime +
  controls | country



