# Skellam & Laplace Distributions: Football Scores and Bitcoin Returns

Two short R studies that fit a probability distribution to real-world data and check how well it describes what actually happens.

*Statistics course project, IE University (Spring 2026).*

| Part | Distribution | Data | Question |
|---|---|---|---|
| 1 | **Skellam** | La Liga match results, 2000–2025 | Can a Skellam model explain goal differences, and how does it compare with bookmaker odds? |
| 2 | **Laplace** | Bitcoin (BTC-USD) daily prices, Jan–Mar 2026 | Do Bitcoin's daily returns fit a Laplace distribution better than a Normal one? |

## Part 1: Skellam distribution and La Liga goal differences

The Skellam distribution describes the difference between two independent Poisson variables. If home and away goals are each Poisson, the goal difference (home − away) is Skellam.

**What the script does** (`SkellamDistribution.R`):
1. Loads and combines season-by-season La Liga CSV files (full-time goals plus Bet365 odds).
2. Computes the goal difference for every match and plots its empirical distribution.
3. Estimates the Skellam parameters as the average home goals (λ₁) and average away goals (λ₂).
4. Overlays the fitted Skellam PMF on the empirical distribution.
5. Derives home-win / draw / away-win probabilities from the model.
6. Converts Bet365 odds into implied probabilities, normalised to remove the bookmaker's margin.
7. Compares the three sources side by side: **actual results vs. Skellam model vs. bookmaker**.

## Part 2: Laplace distribution and Bitcoin returns

Financial returns often have heavier tails and a sharper peak than a Normal distribution predicts. The Laplace distribution is a simple model with both features.

**What the script does** (`LaplaceDistribution.R`):
1. Downloads BTC-USD prices from Yahoo Finance and keeps January–March 2026.
2. Computes daily log-returns, rₜ = log(Pₜ / Pₜ₋₁).
3. Estimates Laplace parameters: location μ = median, scale b = mean absolute deviation from the median.
4. Fits a Normal distribution as a benchmark.
5. Compares both fits with a histogram overlay, a QQ plot, and a tail check (share of returns beyond 2σ).
6. Fits both models formally with `fitdistrplus` and compares goodness-of-fit statistics (AIC/BIC, K-S, etc.).

## How to run

Requirements: R with the following packages.

```r
install.packages(c("readr", "dplyr", "ggplot2", "tidyr", "skellam",
                   "quantmod", "fitdistrplus"))
```

- **Skellam:** download the La Liga season CSVs, put them in one folder, and change the `setwd(...)` path at the top of the script to that folder.
- **Laplace:** no setup needed; the script downloads the data from Yahoo Finance.

## Tools

R · ggplot2 · dplyr · tidyr · skellam · quantmod · fitdistrplus

## Authors

- Nicolas Gonzalez Ferrari ([@NicolasGonzalezFerrari](https://github.com/NicolasGonzalezFerrari))
- Lou Guyot ([@lguyotk6-lgtm](https://github.com/lguyotk6-lgtm))
- Theo Coronges ([@imbricata](https://github.com/imbricata))
