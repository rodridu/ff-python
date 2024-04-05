# Assuming the data is in a CSV file named 'zfile.csv'
zstocks <- read.csv("stk-day-2019.csv", header = TRUE)
mstocks <- read_excel("stk-mon-2019.xlsx")

mm <- stk_mon_2019[stk_mon_2019$date >= "20150130" & stk_mon_2019$date <= "20191231", ]
# Filter data for the period 2015-2019
zstocks <- zstocks[zstocks$date >= 20150101 & zstocks$date <= 20191231, ]
#mstocks <- mstocks[mstocks$date >= "20150131" & mstocks$date <= "20191231", ]

# Extract relevant columns for the regression
stkrets <- zstocks[, c("NKE", "C", "TM", "BIIB")]
stkretsm <- mm[, c("NKE", "C", "TM", "BIIB")]

# Assuming 'stkrets' matrix has columns named PFE, NKE, C, TM, and spret
daily_mod <- lm(cbind(NKE, C, TM, BIIB) ~ spret, data = zstocks)
summary(daily_mod)
monthly_mod <- lm(cbind(NKE, C, TM, BIIB) ~ spret, data = mstocks)
summary(monthly_mod)

ew_portfolio <- rowMeans(stkrets[, 1:4])
ew_mod <- lm(ew_portfolio ~ spret, data = zstocks)
summary(ew_mod)

ew_portfoliom <- rowMeans(stkretsm[, 1:4])
ew_modm <- lm(ew_portfoliom ~ spret, data = mstocks)
summary(ew_modm)

# Calculate daily and monthly standard deviations
sigma_D <- apply(stkrets[, 1:4], 2, sd) * sqrt(252)  # Assuming 252 trading days in a year
sigma_M <- apply(stkretsm[, 1:4], 2, sd) * sqrt(12)   # Assuming 12 months in a year



# Calculate cross-correlation
cross_corr <- ccf(sigma_M, sigma_D, lag.max = length(sigma_M) - 1)

# Plot the cross-correlation function
plot(cross_corr, main = "Cross-Correlation Function", xlab = "Lag", ylab = "Cross-Correlation")

# Calculate the Residuals
monthly_residuals <- residuals(monthly_mod)
daily_residuals <- residuals(daily_mod)

# Combine residuals into a data matrix
residual_matrix_monthly <- cbind(monthly_residuals[, 1], monthly_residuals[, 2], monthly_residuals[, 3], monthly_residuals[, 4])
residual_matrix_daily <- cbind(daily_residuals[, 1], daily_residuals[, 2], daily_residuals[, 3], daily_residuals[, 4])

# Compute the correlation matrix for residuals
correlation_matrix_monthly <- cor(residual_matrix_monthly)
correlation_matrix_daily <- cor(residual_matrix_daily)

# Remove the ones from the correlation matrices
correlation_matrix_monthly[upper.tri(correlation_matrix_monthly)] <- NA
correlation_matrix_daily[upper.tri(correlation_matrix_daily)] <- NA

# Calculate the average of the off-diagonal elements
average_cross_correlation_monthly <- mean(correlation_matrix_monthly, na.rm = TRUE)
average_cross_correlation_daily <- mean(correlation_matrix_daily, na.rm = TRUE)

# Display the results
print("Monthly Average Cross-Correlation:")
print(average_cross_correlation_monthly)

print("Daily Average Cross-Correlation:")
print(average_cross_correlation_daily)
