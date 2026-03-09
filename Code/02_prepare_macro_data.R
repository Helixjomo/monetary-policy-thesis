library(AER)

max_h <- 24

irf <- numeric(max_h+1)

for (h in 0:max_h) {

  y_lead <- lead(IP, h) - lag(IP,1)

  model <- ivreg(
    y_lead ~ policy_rate + lags_IP + lags_CPI |
    shock + lags_IP + lags_CPI
  )

  irf[h+1] <- coef(model)["policy_rate"]
}
