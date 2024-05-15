sp500 = read.csv("../SPY.csv")

sp500$logclose = log(sp500$Close)
diffs = diff(sp500$logclose)
diffs = c(NA, diffs)

sp500$delta = diffs
n_years = 21.0

sp500$Date = as.Date(sp500$Date)

sp500 = sp500[order(sp500$Date), ]

sp500.subset = sp500[sp500$Date >= "2003-01-01" & sp500$Date <= "2023-12-31", ]
n_samples = nrow(sp500.subset)
total_change = sp500.subset[n_samples, ]$logclose - sp500.subset[1, ]$logclose

vol_sq = (-(total_change)^2 / n_samples + sum(sp500.subset$delta^2)) / n_years
vol = sqrt(vol_sq)

drift = total_change / n_years + 0.5 * vol_sq

cat("Drift: ", drift, "\n")
cat("Volatility: ", vol)
