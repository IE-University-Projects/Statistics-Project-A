# ---------------------------------------------------------
# 1. SET WORKING DIRECTORY
# ---------------------------------------------------------
# Change this to the folder where all your CSV files are
setwd("C:/Users/Nicol/Downloads/FootballData")

# Check the files in the folder
list.files()


# ---------------------------------------------------------
# 2. LOAD PACKAGES
# ---------------------------------------------------------
# Run install.packages(...) only once if needed

# install.packages("readr")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("skellam")
# install.packages("tidyr")

library(readr)
library(dplyr)
library(ggplot2)
library(skellam)
library(tidyr)


# ---------------------------------------------------------
# 3. LOAD AND COMBINE ALL CSV FILES
# ---------------------------------------------------------
# We keep only the columns we need:
# FTHG = Full Time Home Goals
# FTAG = Full Time Away Goals
# B365H = Bet365 Home odds
# B365D = Bet365 Draw odds
# B365A = Bet365 Away odds

files <- list.files(pattern = "*.csv")

matches <- files %>%
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE) %>%
      select(any_of(c(
        "Div", "Date", "HomeTeam", "AwayTeam",
        "FTHG", "FTAG",
        "B365H", "B365D", "B365A"
      ))) %>%
      mutate(season_file = f)
  }) %>%
  bind_rows()

# Look at the data
View(matches)
head(matches)
str(matches)


# ---------------------------------------------------------
# 4. CREATE GOAL DIFFERENCE VARIABLE
# ---------------------------------------------------------
# goal_diff = home goals - away goals

matches <- matches %>%
  mutate(goal_diff = FTHG - FTAG)

summary(matches$goal_diff)
table(matches$goal_diff)


# ---------------------------------------------------------
# 5. EMPIRICAL DISTRIBUTION OF GOAL DIFFERENCES
# ---------------------------------------------------------
# This is what really happened in the data

plot_empirical <- ggplot(matches, aes(x = goal_diff)) +
  geom_bar(fill = "steelblue", color = "black") +
  labs(
    title = "Distribution of Goal Differences in La Liga",
    subtitle = "All matches from 2000 to 2025",
    x = "Goal Difference (Home - Away)",
    y = "Frequency"
  )

print(plot_empirical)


# ---------------------------------------------------------
# 6. ESTIMATE SKELLAM PARAMETERS
# ---------------------------------------------------------
# mu1_hat = average home goals
# mu2_hat = average away goals

mu1_hat <- mean(matches$FTHG, na.rm = TRUE)
mu2_hat <- mean(matches$FTAG, na.rm = TRUE)

mu1_hat
mu2_hat


# ---------------------------------------------------------
# 7. CREATE FITTED SKELLAM DISTRIBUTION
# ---------------------------------------------------------
# We calculate the Skellam probability for each possible
# goal difference value

k_vals <- seq(
  min(matches$goal_diff, na.rm = TRUE),
  max(matches$goal_diff, na.rm = TRUE),
  by = 1
)

skellam_probs <- dskellam(
  x = k_vals,
  lambda1 = mu1_hat,
  lambda2 = mu2_hat
)

df_skellam <- data.frame(
  goal_diff = k_vals,
  prob = skellam_probs
)

head(df_skellam)


# ---------------------------------------------------------
# 8. CREATE EMPIRICAL PROBABILITY DISTRIBUTION
# ---------------------------------------------------------
# Turn the real goal differences into probabilities

empirical_counts <- table(matches$goal_diff)
empirical_probs <- empirical_counts / sum(empirical_counts)

df_emp <- data.frame(
  goal_diff = as.numeric(names(empirical_probs)),
  prob = as.numeric(empirical_probs)
)

head(df_emp)


# ---------------------------------------------------------
# 9. COMPARE EMPIRICAL VS FITTED SKELLAM DISTRIBUTION
# ---------------------------------------------------------
# Grey bars = real data
# Red line = Skellam model

plot_comparison <- ggplot() +
  geom_bar(
    data = df_emp,
    aes(x = goal_diff, y = prob),
    stat = "identity",
    fill = "grey70",
    color = "black"
  ) +
  geom_line(
    data = df_skellam,
    aes(x = goal_diff, y = prob),
    color = "red",
    linewidth = 1
  ) +
  geom_point(
    data = df_skellam,
    aes(x = goal_diff, y = prob),
    color = "red",
    size = 2
  ) +
  labs(
    title = "Empirical vs Fitted Skellam Distribution",
    subtitle = "La Liga goal differences",
    x = "Goal Difference (Home - Away)",
    y = "Probability"
  )

print(plot_comparison)


# ---------------------------------------------------------
# 10. COMPUTE SKELLAM HOME / DRAW / AWAY PROBABILITIES
# ---------------------------------------------------------
# Home win  = goal difference > 0
# Draw      = goal difference = 0
# Away win  = goal difference < 0

p_draw <- dskellam(0, lambda1 = mu1_hat, lambda2 = mu2_hat)

# Build full PMF table first
df_skellam_full <- data.frame(
  goal_diff = k_vals,
  prob = skellam_probs
)

p_home <- sum(df_skellam_full$prob[df_skellam_full$goal_diff > 0])
p_away <- sum(df_skellam_full$prob[df_skellam_full$goal_diff < 0])

probabilities_skellam <- data.frame(
  Outcome = c("Home Win", "Draw", "Away Win"),
  Probability = c(p_home, p_draw, p_away)
)

print(probabilities_skellam)


# ---------------------------------------------------------
# 11. PLOT SKELLAM OUTCOME PROBABILITIES
# ---------------------------------------------------------

plot_skellam_outcomes <- ggplot(
  probabilities_skellam,
  aes(x = Outcome, y = Probability, fill = Outcome)
) +
  geom_col(color = "black") +
  labs(
    title = "Skellam Model Probabilities",
    subtitle = "La Liga outcomes based on fitted Skellam distribution",
    x = "",
    y = "Probability"
  ) +
  theme(legend.position = "none")

print(plot_skellam_outcomes)


# ---------------------------------------------------------
# 12. CONVERT BETTING ODDS INTO IMPLIED PROBABILITIES
# ---------------------------------------------------------
# First:
# raw_home = 1 / B365H
# raw_draw = 1 / B365D
# raw_away = 1 / B365A
#
# Then normalize them because bookmaker odds include margin

matches <- matches %>%
  mutate(
    raw_home = 1 / B365H,
    raw_draw = 1 / B365D,
    raw_away = 1 / B365A
  ) %>%
  mutate(
    total_raw = raw_home + raw_draw + raw_away,
    book_home = raw_home / total_raw,
    book_draw = raw_draw / total_raw,
    book_away = raw_away / total_raw
  )

head(matches %>% select(B365H, B365D, B365A, book_home, book_draw, book_away))


# ---------------------------------------------------------
# 13. COMPUTE AVERAGE BOOKMAKER PROBABILITIES
# ---------------------------------------------------------

avg_book_home <- mean(matches$book_home, na.rm = TRUE)
avg_book_draw <- mean(matches$book_draw, na.rm = TRUE)
avg_book_away <- mean(matches$book_away, na.rm = TRUE)

probabilities_bookmaker <- data.frame(
  Outcome = c("Home Win", "Draw", "Away Win"),
  Probability = c(avg_book_home, avg_book_draw, avg_book_away)
)

print(probabilities_bookmaker)


# ---------------------------------------------------------
# 14. COMPARE SKELLAM VS BOOKMAKER
# ---------------------------------------------------------

comparison <- data.frame(
  Outcome = c("Home Win", "Draw", "Away Win"),
  Skellam = c(p_home, p_draw, p_away),
  Bookmaker = c(avg_book_home, avg_book_draw, avg_book_away)
)

print(comparison)


# ---------------------------------------------------------
# 15. PLOT SKELLAM VS BOOKMAKER
# ---------------------------------------------------------

comparison_long <- comparison %>%
  pivot_longer(
    cols = c(Skellam, Bookmaker),
    names_to = "Source",
    values_to = "Probability"
  )

plot_betting_comparison <- ggplot(
  comparison_long,
  aes(x = Outcome, y = Probability, fill = Source)
) +
  geom_col(position = "dodge", color = "black") +
  labs(
    title = "Skellam vs Bookmaker Probabilities",
    subtitle = "La Liga comparison",
    x = "",
    y = "Probability"
  )

print(plot_betting_comparison)


# ---------------------------------------------------------
# 16. EMPIRICAL MATCH OUTCOME PROBABILITIES
# ---------------------------------------------------------
# This shows what actually happened in the real matches

matches <- matches %>%
  mutate(
    actual_result = case_when(
      goal_diff > 0 ~ "Home Win",
      goal_diff == 0 ~ "Draw",
      goal_diff < 0 ~ "Away Win"
    )
  )

empirical_outcomes <- matches %>%
  count(actual_result) %>%
  mutate(Probability = n / sum(n)) %>%
  select(Outcome = actual_result, Probability)

print(empirical_outcomes)


# ---------------------------------------------------------
# 17. FINAL COMPARISON: EMPIRICAL VS SKELLAM VS BOOKMAKER
# ---------------------------------------------------------

final_comparison <- empirical_outcomes %>%
  rename(Empirical = Probability) %>%
  left_join(probabilities_skellam %>% rename(Skellam = Probability), by = "Outcome") %>%
  left_join(probabilities_bookmaker %>% rename(Bookmaker = Probability), by = "Outcome")

print(final_comparison)

final_comparison_long <- final_comparison %>%
  pivot_longer(
    cols = c(Empirical, Skellam, Bookmaker),
    names_to = "Source",
    values_to = "Probability"
  )

plot_final <- ggplot(
  final_comparison_long,
  aes(x = Outcome, y = Probability, fill = Source)
) +
  geom_col(position = "dodge", color = "black") +
  labs(
    title = "Empirical vs Skellam vs Bookmaker Probabilities",
    subtitle = "La Liga outcomes",
    x = "",
    y = "Probability"
  )

print(plot_final)