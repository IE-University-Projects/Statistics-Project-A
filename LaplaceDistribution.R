# =========================================================
# LAPLACE DISTRIBUTION PROJECT
# Bitcoin daily log-returns, Jan-Mar 2026
# =========================================================

# ---------------------------------------------------------
# 1. LOAD PACKAGES
# ---------------------------------------------------------
# Run install.packages(...) only once if needed

# install.packages("quantmod")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("fitdistrplus")

library(quantmod)
library(ggplot2)
library(dplyr)
library(fitdistrplus)

# ---------------------------------------------------------
# 2. DOWNLOAD BITCOIN PRICE DATA FROM YAHOO FINANCE
# ---------------------------------------------------------
# BTC-USD is the Yahoo Finance ticker for Bitcoin in USD

getSymbols("BTC-USD", src = "yahoo", from = "2025-12-15", to = "2026-03-31")

# Convert xts object to data frame
btc_df <- data.frame(
  date = index(`BTC-USD`),
  coredata(`BTC-USD`)
)

# Rename columns for clarity
colnames(btc_df) <- c("date", "open", "high", "low", "close", "volume", "adjusted")

# ---------------------------------------------------------
# 3. KEEP ONLY JANUARY TO MARCH 2026
# ---------------------------------------------------------

btc_df <- btc_df %>%
  filter(date >= as.Date("2026-01-01"),
         date <= as.Date("2026-03-31"))

View(btc_df)
head(btc_df)
summary(btc_df$close)

# ---------------------------------------------------------
# 4. COMPUTE DAILY LOG-RETURNS
# ---------------------------------------------------------
# r_t = log(P_t / P_{t-1})

btc_df <- btc_df %>%
  arrange(date) %>%
  mutate(log_return = log(close / lag(close)))

# Remove first NA return
returns_df <- btc_df %>%
  filter(!is.na(log_return))

View(returns_df)
head(returns_df)
summary(returns_df$log_return)

# ---------------------------------------------------------
# 5. HISTOGRAM OF BITCOIN LOG-RETURNS
# ---------------------------------------------------------

plot_returns <- ggplot(returns_df, aes(x = log_return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey70",
                 color = "black") +
  labs(
    title = "Bitcoin Daily Log-Returns",
    subtitle = "January to March 2026",
    x = "Log-return",
    y = "Density"
  )

print(plot_returns)

# ---------------------------------------------------------
# 6. ESTIMATE LAPLACE PARAMETERS
# ---------------------------------------------------------
# For Laplace:
# mu_hat = median
# b_hat  = mean absolute deviation from median

mu_hat <- median(returns_df$log_return, na.rm = TRUE)
b_hat <- mean(abs(returns_df$log_return - mu_hat), na.rm = TRUE)

mu_hat
b_hat

# ---------------------------------------------------------
# 7. DEFINE LAPLACE DENSITY FUNCTION
# ---------------------------------------------------------

dlaplace_custom <- function(x, mu, b) {
  1 / (2 * b) * exp(-abs(x - mu) / b)
}

# ---------------------------------------------------------
# 8. NORMAL BENCHMARK PARAMETERS
# ---------------------------------------------------------

norm_mean <- mean(returns_df$log_return, na.rm = TRUE)
norm_sd   <- sd(returns_df$log_return, na.rm = TRUE)

norm_mean
norm_sd

# ---------------------------------------------------------
# 9. CREATE X VALUES FOR DENSITY CURVES
# ---------------------------------------------------------

x_vals <- seq(
  min(returns_df$log_return, na.rm = TRUE),
  max(returns_df$log_return, na.rm = TRUE),
  length.out = 1000
)

laplace_density <- dlaplace_custom(x_vals, mu_hat, b_hat)
normal_density  <- dnorm(x_vals, mean = norm_mean, sd = norm_sd)

df_laplace <- data.frame(x = x_vals, density = laplace_density)
df_normal  <- data.frame(x = x_vals, density = normal_density)

# ---------------------------------------------------------
# 10. PLOT HISTOGRAM + LAPLACE + NORMAL
# ---------------------------------------------------------

plot_comparison <- ggplot(returns_df, aes(x = log_return)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 30,
                 fill = "grey70",
                 color = "black") +
  geom_line(data = df_laplace,
            aes(x = x, y = density),
            color = "red",
            linewidth = 1) +
  geom_line(data = df_normal,
            aes(x = x, y = density),
            color = "blue",
            linewidth = 1,
            linetype = "dashed") +
  labs(
    title = "Bitcoin Returns: Laplace vs Normal Fit",
    subtitle = "Red = Laplace, Blue dashed = Normal",
    x = "Log-return",
    y = "Density"
  )

print(plot_comparison)

# ---------------------------------------------------------
# 11. QQ PLOT AGAINST NORMAL
# ---------------------------------------------------------
# This helps show whether tails are heavier than normal

qqnorm(returns_df$log_return,
       main = "QQ Plot of Bitcoin Log-Returns")
qqline(returns_df$log_return, col = "red", lwd = 2)

# ---------------------------------------------------------
# 12. SIMPLE TAIL CHECK
# ---------------------------------------------------------
# Compare how many observations are beyond 2 standard deviations

threshold <- 2 * norm_sd

tail_count <- sum(abs(returns_df$log_return - norm_mean) > threshold, na.rm = TRUE)
tail_proportion <- tail_count / nrow(returns_df)

tail_count
tail_proportion

# ---------------------------------------------------------
# 13. OPTIONAL: FIT WITH fitdistrplus
# ---------------------------------------------------------
# This provides a more formal fitting workflow for the Laplace model

fit_laplace <- fitdist(
  data = returns_df$log_return,
  distr = "laplace_custom",
  start = list(mu = mu_hat, b = b_hat)
)

fit_normal <- fitdist(
  data = returns_df$log_return,
  distr = "norm"
)

plaplace_custom <- function(q, mu, b) {
  ifelse(q < mu,
         0.5 * exp((q - mu) / b),
         1 - 0.5 * exp(-(q - mu) / b))
}


summary(fit_laplace)
summary(fit_normal)

gofstat(list(fit_laplace, fit_normal))

denscomp(list(fit_laplace, fit_normal),
         legendtext = c("Laplace", "Normal"))

qqcomp(list(fit_laplace, fit_normal),
       legendtext = c("Laplace", "Normal"))