# Load macro data
macro <- read_csv("data/raw/macro_data.csv")

# Load monetary policy shocks
shocks <- read_csv("data/raw/mp_shocks.csv")

# Convert to date
macro$date <- as.Date(macro$date)
shocks$date <- as.Date(shocks$date)

# Merge
df <- macro %>%
  left_join(shocks, by = "date") %>%
  arrange(date)

# Replace NA shocks with 0
df$shock[is.na(df$shock)] <- 0

saveRDS(df, "data/cleaned/df_clean.rds")


# First principal component aggregation of yield curve responses (for comparison with Jarociński)

database_for_event_studies_of_swedish_monetary_policy <- read_excel("Data/Raw/database-for-event-studies-of-swedish-monetary-policy.xlsx", sheet = "Database", range = "A1:M290")
View(database_for_event_studies_of_swedish_monetary_policy)

names(database_for_event_studies_of_swedish_monetary_policy)[names(database_for_event_studies_of_swedish_monetary_policy) %in% c("1m","3m","6m","1yr","2yr","5yr","10yr")] <-
  c("y1m","y3m","y6m","y1yr","y2yr","y5yr","y10yr")

database_for_event_studies_of_swedish_monetary_policy <- data.frame(
  y1m = database_for_event_studies_of_swedish_monetary_policy$y1m,
  y3m = database_for_event_studies_of_swedish_monetary_policy$y3m,
  y6m = database_for_event_studies_of_swedish_monetary_policy$y6m,
  y1y = database_for_event_studies_of_swedish_monetary_policy$y1y,
  y2y = database_for_event_studies_of_swedish_monetary_policy$y2y,
  y5y = database_for_event_studies_of_swedish_monetary_policy$y5y,
  y10y = database_for_event_studies_of_swedish_monetary_policy$y10y
)

yields <- data.frame(y1m, y3m, y6m, y1y, y2y, y5y, y10y)

pca <- prcomp(yields, scale = TRUE)

mp_shock <- pca$x[,1]   # first principal component


# Alt.

yields_scaled <- scale(yields)

# This provides pca$x → factor scores (time series), pca$rotation → loadings (weights)
pca <- prcomp(yields_scaled)

# Extract PC1
mp_shock <- pca$x[,1]

pca$rotation[,1]

if(cor(mp_shock, yields$`2y`) < 0){
  mp_shock <- -mp_shock
}

# normalize magnitude
mp_shock <- mp_shock / sd(mp_shock)

# diagnostics
summary(pca)
plot(pca$rotation[,1])
cor(mp_shock, yields$`2y`)


# alt. clean code (review final version of code)

# 1. Select yields
yields <- data.frame(y1m, y3m, y6m, y1y, y2y, y5y, y10y)

# 2. Standardize
yields_scaled <- scale(yields)

# 3. PCA
pca <- prcomp(yields_scaled)

# 4. Extract PC1
mp_shock <- pca$x[,1]

# 5. Fix sign
if(cor(mp_shock, yields$y2y) < 0){
  mp_shock <- -mp_shock
}

# 6. Normalize
mp_shock <- mp_shock / sd(mp_shock)


### Alt. Alt.

# Load Excel sheet
almerud_data <- read_excel(
  "Data/Raw/database-for-event-studies-of-swedish-monetary-policy.xlsx",
  sheet = "Database",
  range = "A1:M290"
)

# Rename columns
names(almerud_data)[
  names(almerud_data) %in% c("1m","3m","6m","1yr","2yr","5yr","10yr")
] <- c("y1m","y3m","y6m","y1y","y2y","y5y","y10y")

# Convert your date column to Date class
almerud_data$Date <- as.Date(
  almerud_data$Date
)

# Subset only the yield columns into a new data frame
yields <- data.frame(
  y1m = almerud_data$y1m,
  y3m = almerud_data$y3m,
  y6m = almerud_data$y6m,
  y1y = almerud_data$y1y,
  y2y = almerud_data$y2y,
  y5y = almerud_data$y5y,
  y10y = almerud_data$y10y
)


# Perform PCA on yields
yields <- almerud_data %>%
  select(any_of(c("y1m","y3m","y6m","y1y","y2y","y5y","y10y")))

pca <- prcomp(yields, scale. = TRUE)

# Extract first principal component
almerud_data$mp_shock <- pca$x[,1]


# Aggregate shocks into monthly variables by summation

monthly_shocks <- almerud_data %>%
  mutate(
    Date = as.Date(Date),                         # ensure proper date format
    month = floor_date(Date, "month")             # convert to monthly period
  ) %>%
  group_by(month) %>%
  summarise(
    mp_shock = sum(mp_shock, na.rm = TRUE)        # ADD shocks within month
  ) %>%
  ungroup()

# Add missing months

monthly_shocks <- monthly_shocks %>%
  complete(
    month = seq(min(month), max(month), by = "month"),
    fill = list(mp_shock = 0)
  )


# Inspect the results

head(monthly_shocks)
plot(monthly_shocks$month, monthly_shocks$mp_shock, type = "l")


# Save the shocks

df_out <- data.frame(
  date = monthly_shocks$month,
  mp_shock = monthly_shocks$mp_shock
)

write_xlsx(df_out, "Data/Cleaned/mp_shocks.xlsx")


# Alt. approach "target and path factors (long AND short maturities), robustness

###############################################################################




# Synchronize the dates
files <- list.files(path = "Data/Cleaned", pattern = "\\.xlsx$", full.names = TRUE)

clean_dates <- function(df) {
  df %>%
    mutate(date = case_when(
      
      # Format like "2001M1"
      grepl("^\\d{4}M\\d{1,2}$", date) ~ as.yearmon(date, format = "%YM%m"),
      
      # Excel numeric dates
      is.numeric(date) ~ as.yearmon(as.Date(date, origin = "1899-12-30")),
      
      # Standard strings (adjust dmy/ymd if needed)
      TRUE ~ as.yearmon(dmy(date))
      
    ))
}

list_data <- lapply(list_data, clean_dates)


#3##########33

clean_dates <- function(df) {
  
  df %>%
    mutate(date = as.character(date)) %>%  # ensure consistent type
    mutate(
      date = ifelse(
        grepl("^\\d{4}M\\d{1,2}$", date),
        as.character(as.yearmon(date, format = "%YM%m")),
        date
      )
    ) %>%
    mutate(
      date = suppressWarnings(
        as.yearmon(date, tryFormats = c(
          "%Y-%m-%d",
          "%d/%m/%Y",
          "%d.%m.%Y",
          "%Y-%m",
          "%m/%Y"
        ))
      )
    )
}

for (i in seq_along(list_data)) {
  cat("Checking file", i, "\n")
  print(head(list_data[[i]]$date))
}

test <- lapply(list_data, function(df) {
  try(clean_dates(df))
})

mutate(date = na_if(date, ""))



files <- list.files(
  path = "Data/Cleaned",
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Remove temp files starting with "~$"
files <- files[!grepl("^~\\$", basename(files))]

list_data <- lapply(files, read_excel)

list_data <- lapply(list_data, function(df) {
  df %>%
    mutate(date = as.Date(date))
})


# Load macro data
macro <- read_excel("Data/Cleaned/ipi.xlsx")
macro <- read_excel("Data/Cleaned/ir.xlsx")
macro <- read_excel("Data/Cleaned/facility.xlsx")
macro <- read_excel("Data/Cleaned/KPIF.xlsx")

# Load monetary policy shocks
shocks <- read_excel("Data/Cleaned/mp_shocks.xlsx")

# Convert to date
macro$date <- as.Date(macro$date)
shocks$date <- as.Date(shocks$date)

# Merge
df <- macro %>%
  left_join(shocks, by = "date") %>%
  arrange(date)

# Replace NA shocks with 0
df$shock[is.na(df$shock)] <- 0

saveRDS(df, "data/processed/df_clean.rds")

################################################################################

# Convert Excel dates into R dates and transform to monthly data

convert_excel_date <- function(x) {
  x_num <- suppressWarnings(as.numeric(x))
  as.Date(x_num, origin = "1899-12-30")
}

# Convert the monetary policy interest rates

ir <- read_excel("Data/Cleaned/ir.xlsx")
ir <- ir %>%
  mutate(date = ymd(date))

# Check results
str(ir$date)

# Aggregate monthly (There is one conflict in 2008-10 where two interest rates are announced in the same month)
ir_monthly <- ir %>%
  mutate(month = floor_date(date, "month")) %>%
  arrange(date) %>%
  group_by(month) %>%
  summarise(ir = last(ir))

head(ir$date, 10)
str(ir$date)

# Complete the missing months with the existing policy rates
monthly_index <- data.frame(
  month = seq(min(ir_monthly$month), max(ir_monthly$month), by = "month")
)

mpir_monthly <- monthly_index %>%
  left_join(ir_monthly, by = "month") %>%
  arrange(month) %>%
  fill(ir, .direction = "down")

View(mpir_monthly)


# Convert Riksbanken MP liabilities (6 dates are in the wrong format)

mpliab <- read_excel("Data/Cleaned/mpliab.xlsx") %>%
  mutate(date_raw = str_trim(as.character(date))) %>%
  mutate(
    date = case_when(
      # Case 1: Excel numeric dates
      grepl("^[0-9]+(\\.[0-9]+)?$", date_raw) ~ 
        as.Date(as.numeric(date_raw), origin = "1899-12-30"),
      
      # Case 2: ISO-like strings → extract first 10 chars
      grepl("^\\d{4}-\\d{2}-\\d{2}", date_raw) ~ 
        as.Date(substr(date_raw, 1, 10), format = "%Y-%m-%d"),
      
      TRUE ~ NA_Date_
    )
  )

# Check that it is solved
sum(is.na(mpliab$date))

# Aggregate monthly
ml_monthly <- mpliab %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(dfacility = mean(mpliab, na.rm = TRUE)) %>%
  arrange(month) %>%
  filter(month < max(month)) # drops the last month as we do not have full data on it


ml_monthly <- mpliab %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(mpliab = mean(mpliab, na.rm = TRUE))

final_df <- left_join(main_data, df_monthly, by = "month")


# KPIF cleaning
kpif <- read_excel("Data/Cleaned/KPIF.xlsx")
kpif <- kpif %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))

kpif <- kpif %>%
  arrange(date)

kpif_monthly <- kpif %>%
  mutate(month = floor_date(date, "month"))

infl_yoy <- kpif_monthly %>%
  mutate(
    infl_yoy = 100 * (log(kpif) - log(dplyr::lag(kpif, 12)))
  )

infl_mom <- kpif_monthly %>%
  mutate(
    infl_mom = 100 * (log(kpif) - log(dplyr::lag(kpif, 1)))
  )

# Drop date and kpif
infl_yoy <- infl_yoy[, !(names(infl_yoy) %in% c("date", "kpif"))]
infl_mom <- infl_mom[, !(names(infl_mom) %in% c("date", "kpif"))]


#kpif <- kpif %>%
#  mutate(month = floor_date(date, "month")) %>%
#  group_by(month) %>%
#  summarise(kpif = mean(kpif, na.rm = TRUE))


# Industrial Production
ipi <- read_excel("Data/Cleaned/ipi.xlsx")

ipi <- ipi %>%
  mutate(
    year  = substr(date, 1, 4),
    month = substr(date, 6, 7),
    date  = as.Date(paste0(year, "-", month, "-01"))
  ) %>%
  select(date, ipi)

monthly_ipi <- ipi %>%
  rename(month = date)

#ip_growth <- monthly_ipi %>%
#  arrange(month) %>%
#  mutate(ip = 100 * (ipi / dplyr::lag(ipi) - 1))

ip_yoy <- monthly_ipi %>%
  mutate(
    ipi_yoy = 100 * (log(ipi) - log(dplyr::lag(ipi, 12)))
  )

ip_mom <- monthly_ipi %>%
  mutate(
    ipi_mom = 100 * (log(ipi) - log(dplyr::lag(ipi, 1)))
  )

# Drop ipi
ip_yoy <- ip_yoy[, !(names(ip_yoy) %in% c("ipi"))]
ip_mom <- ip_mom[, !(names(ip_mom) %in% c("ipi"))]


# Merging all variables
df_main <- monthly_shocks %>%
  left_join(infl_yoy, by = "month") %>%
  left_join(ip_yoy, by = "month") %>%
  left_join(ml_monthly, by = "month") %>%
  left_join(infl_mom, by = "month") %>%
  left_join(ip_mom, by = "month") %>%
  arrange(month)

# Ensure full monthly sequence
all_months <- data.frame(
  month = seq(min(df_main$month), max(df_main$month), by = "month")
)

df_main <- all_months %>%
  left_join(df_main, by = "month")

# 5. Save
write_xlsx(df_main, "Data/Cleaned/macro_dataset.xlsx")



###############################################################################


deposit %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    n_obs = n(),
    mean_val = mean(dfacility, na.rm = TRUE)
  ) %>%
  tail()


deposit <- deposit %>%
  mutate(date = as.numeric(date),
         date = as.Date(date, origin = "1899-12-30"))

head(deposit$date)
str(deposit$date)

deposit %>%
  filter(is.na(as.numeric(date))) %>%
  distinct(date)

deposit <- deposit %>%
  mutate(date = dmy(date))

df_monthly <- deposit %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(deposit_facility = mean(dfacility, na.rm = TRUE))

################################################################################

# Clear all before analysis
rm(list = ls(all.names = TRUE))
gc()
cat("\014")  # clears console in some environments



# Trim estimation window (perhaps later)
df_monthly <- df_monthly %>%
  filter(month <= as.Date("2025-11-01"))


# e.g. for data frames
max_shock_month <- max(shocks_df$month)

df_monthly <- df_monthly %>%
  filter(month <= max_shock_month)


plot(ml_monthly$month, ml_monthly$mpliab, type = "l")

# Optional removing outliers and smoothing
df_monthly <- df_monthly %>%
  mutate(
    change = abs(dfacility - lag(dfacility))
  ) %>%
  filter(change < 200000 | is.na(change))

# Smoothing
df_monthly$dfacility_smooth <- zoo::rollmean(df_monthly$dfacility, 3, fill = NA)

# Compare with total liabilities
summary(mpliab$mpliab)
summary(mpliab$tliab)

# Ratio
plot(mpliab$date, mpliab$mpliab / mpliab$tliab, type = "l")

# Log scale
plot(mpliab$date, log(mpliab$mpliab), type = "l", col = "blue")
lines(mpliab$date, log(mpliab$tliab), col = "red")

# Normal scale
plot(mpliab$date, mpliab$mpliab, type = "l", col = "blue")
lines(mpliab$date, mpliab$tliab, col = "red")

# Realign and skip until 2009

mpliab_plot <- mpliab %>%
  filter(date >= as.Date("2009-01-01"))

mpliab_plot <- mpliab %>%
  filter(date >= as.Date("2009-01-01")) %>%
  arrange(date) %>%
  mutate(
    mp_shifted = mpliab + 
      (first(tliab) - first(mpliab))
  )

plot(mpliab_plot$date, mpliab_plot$mp_shifted, type = "l", col = "blue")
lines(mpliab_plot$date, mpliab_plot$tliab, col = "red")



# GGplot version
mpliab_plot <- mpliab %>%
  filter(date >= as.Date("2009-01-01"))

ggplot(mpliab_plot, aes(x = date)) +
  geom_line(aes(y = tliab, color = "Total liabilities"), linewidth = 1) +
  geom_line(aes(y = mpliab, color = "MP liabilities"), linewidth = 1) +
  labs(
    title = "Liquidity in the Swedish Banking System",
    x = "Date",
    y = "SEK (millions)",
    color = ""
  ) +
  theme_minimal()


# Better colours
ggplot(mpliab_plot, aes(x = date)) +
  geom_line(aes(y = tliab, color = "Total liabilities"), linewidth = 1) +
  geom_line(aes(y = mpliab, color = "MP liabilities"), linewidth = 1) +
  scale_color_manual(values = c(
    "Total liabilities" = "red",
    "MP liabilities" = "blue"
  )) +
  labs(
    title = "Liquidity in the Swedish Banking System",
    subtitle = "Riksbank balance sheet components",
    x = "Date",
    y = "SEK (millions)",
    color = NULL
  ) +
  theme_minimal(base_size = 14)


# Even better
ggplot(mpliab_plot, aes(x = date)) +
  geom_line(aes(y = tliab, color = "Total liabilities"), linewidth = 1) +
  geom_line(aes(y = mpliab, color = "MP liabilities"), linewidth = 1) +
  scale_color_manual(values = c(
    "Total liabilities" = "#D55E00",
    "MP liabilities" = "#0072B2"
  )) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Liquidity in the Swedish Banking System",
#    subtitle = "Monetary policy liabilities & total liabilities",
    x = NULL,
    y = "SEK (millions)",
    color = NULL,
    caption = "Source: Sveriges Riksbank"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray30")
  )


# Highlight QE
ggplot(mpliab_plot, aes(x = date)) +
  geom_rect(aes(xmin = as.Date("2015-01-01"),
                xmax = as.Date("2021-12-31"),
                ymin = -Inf, ymax = Inf),
            fill = "grey90", alpha = 0.3) +
  geom_line(aes(y = tliab, color = "Total liabilities"), linewidth = 1) +
  geom_line(aes(y = mpliab, color = "MP liabilities"), linewidth = 1) +
  scale_color_manual(values = c(
    "Total liabilities" = "#D55E00",
    "MP liabilities" = "#0072B2"
  )) +
  labs(
    title = "Liquidity in the Swedish Banking System",
#    subtitle = "Shaded area indicates QE period",
    x = NULL,
    y = "SEK (millions)",
    color = NULL
  ) +
  theme_minimal(base_size = 14)


# Shifted
mp_shift <- mpliab_plot$tliab[1] - mpliab_plot$mpliab[1]

mpliab_plot <- mpliab_plot %>%
  mutate(
    mp_shifted = mpliab + mp_shift
  )


ggplot(mpliab_plot, aes(x = date)) +
  geom_line(aes(y = tliab, color = "Total liabilities"), linewidth = 1) +
  geom_line(aes(y = mp_shifted, color = "MP liabilities"), linewidth = 1) +
  
  scale_y_continuous(
    name = "Total liabilities (SEK, millions)",
    labels = comma,
    
    sec.axis = sec_axis(
      ~ . - mp_shift,
      name = "MP liabilities (SEK, millions)",
      labels = comma
    )
  ) +
  
  scale_color_manual(values = c(
    "Total liabilities" = "#D55E00",
    "MP liabilities" = "#0072B2"
  )) +
  
  labs(
    title = "Liquidity in the Swedish Banking System",
#    subtitle = "MP liabilities shifted to match initial level",
    x = NULL,
    color = NULL,
    caption = "Source: Sveriges Riksbank"
  ) +
  
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold"),
    axis.title.y.left = element_text(color = "#D55E00"),
    axis.title.y.right = element_text(color = "#0072B2")
  )