library("nycflights13")
library("car")
data(flights)
attach(flights)

par(mfrow = c(1, 1))

#1
hist(na.omit(air_time), freq = FALSE, main = "Histogram of Air Time with Density", xlab = "Air Time (minutes)", breaks = "freedman-diaconis")
lines(density(air_time, na.rm = TRUE), lwd = 2, col = "red")

#2
hist(na.omit(distance), freq = FALSE, main = "Histogram of Distance with Density (Bandwidth = 20)", xlab = "Distance (miles)", breaks = "freedman-diaconis")
lines(density(distance, bw = 20, na.rm = TRUE), lwd = 2, col = "red")

hist(na.omit(distance), freq = FALSE, main = "Histogram of Distance with Density (Bandwidth = 100)", xlab = "Distance (miles)", breaks = "freedman-diaconis")
lines(density(distance, bw = 100, na.rm = TRUE), lwd = 2, col = "red")

#3
normalized_air_time <- (air_time - min(air_time)) / (max(air_time) - min(air_time))
normalized_distance <- (distance - min(distance)) / (max(distance) - min(distance))

normalized_air_time <- air_time / max(air_time, na.rm = TRUE)
normalized_distance <- distance / max(distance, na.rm = TRUE)
plot(ecdf(normalized_air_time), main = "Empirical Distribution Function Comparison - Min-Max Normalized",
     xlab = "Scaled Values (0 to 1)", ylab = "Empirical Probability",
     col = "red", xlim = c(0, 1))
lines(ecdf(normalized_distance), col = "blue")
legend("bottomright", legend = c("Air Time", "Distance"), col = c("red", "blue"), lty = 1)

plot(ecdf(log(air_time)), main = "Empirical Distribution Function Comparison - Logarithmic-Scale",
     xlab = "Normalized Values", ylab = "Cumulative Probability",
     col = "red", xlim = c(0, 10))
lines(ecdf(log(distance)), col = "blue")

legend("bottomright", legend = c("Air Time", "Distance"), col = c("red", "blue"), lty = 1)

#4
qqPlot(table(month), distribution = "unif", col = "red", main = "QQ Plot of Flights per Month",
       xlab = "uniformly distributed quantiles", ylab = "Flights by Day")
qqPlot(table(day), distribution = "unif", col = "red", main = "QQ Plot of Flights per Day",
       xlab = "uniformly distributed quantiles", ylab = "Flights by Month")

#5
qqPlot(table(dest), distribution = "norm", main = "Quantile-Quantile Plot for Flights to Destinations - Nomral",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "purple")

qqPlot(table(dest), distribution = "lnorm", main = "Quantile-Quantile Plot for Flights to Destinations - Log-Normal",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "purple")

qqPlot(table(dest), distribution = "exp", main = "Quantile-Quantile Plot for Flights to Destinations - Exponential",
       xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", col = "purple")

# Task 6: Compare departure times of carriers AA and WN with QQ-plot
aa_departure <- flights %>%
  filter(carrier == "AA") %>%
  pull(dep_time)

wn_departure <- flights %>%
  filter(carrier == "WN") %>%
  pull(dep_time)

qqplot(aa_departure, wn_departure, main = "Quantile-Quantile Plot for Departure Times (AA vs WN)", col="green",
       xlab = "Quantiles (AA) Departure Times", ylab = "Quantiles (WN) Departure Times")

abline(0, 1, col="orange")

# Task 7: Compare departure times of carriers AA and WN by hand
quantile_aa <- quantile(aa_departure, probs = seq(0, 1, by = 0.01), na.rm = TRUE)
quantile_wn <- quantile(wn_departure, probs = seq(0, 1, by = 0.01), na.rm = TRUE)

plot(quantile_aa, quantile_wn, main = "Manual Quantile-Quantile Plot for Departure Times (AA vs WN)",
     xlab = "Quantiles (AA) Departure Times", ylab = "Quantiles (WN) Departure Times", col = "green")

abline(0, 1, col="orange")

#8
main_carriers <- names(table(carrier)[carriers_counts >= 1000])

flights_carriers <- subset(flights, carrier %in% main_carriers)

density <- density(flights_carriers$arr_delay, na.rm = TRUE)

plot(density, xlim = c(-100, 250), ylim = c(0,0.03), main = "Density Estimate of Arrival Delay",
     xlab = "Arrival Delay (minutes)", ylab = "Density", type = "n")

colors <- rainbow(length(main_carriers))
for (i in seq_along(main_carriers)) {
  carrier_flights <- flights_carriers[flights_carriers$carrier == main_carriers[i],]
  density <- density(carrier_flights$arr_delay, na.rm = TRUE)
  lines(density, col = colors[i], lwd = 2)
}
legend("topright", legend = c("All", main_carriers), col = c("black", colors),lwd = 2)

#9
carriers_high_frequency <- names(which(table(flights$carrier) >= 1000))
colors <- rainbow(length(carriers_high_frequency))

prepared_flights_data <- flights %>%
  filter(carrier %in% carriers_high_frequency) %>%
  filter(is.finite(arr_delay)) %>%
  select(carrier, arr_delay)

ggplot(prepared_flights_data, aes(sample = arr_delay)) +
  stat_qq(aes(color = carrier)) +
  geom_abline(slope = 1, intercept = 0) +
  labs(title = "Overlayed QQ Plots of Arrival Delays by Carrier",
       y = "Arrival Delays Quantiles",
       x = "Theoretical Quantiles") +
  scale_color_manual(values = colors) + theme_minimal()
