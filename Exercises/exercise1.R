library("nycflights13")
library("epade")
data(flights)
attach(flights)
?flights
str(flights)

#1
par(mfrow = c(1, 2))
stripchart(air_time, method = "jitter", xlab = "Air Time (minutes)", main = "Univariate scatterplot of air time", col = "blue")
# Intepretation:

#2
stripchart(distance, method = "jitter", xlab = "Distance (miles)", main = "Univariate scatterplot of distance", col = "red")
# Intepretation:

#3
#par(mfrow = c(1, 2))
hist(air_time, main = "Histogram of air_time", xlab = "Air Time (minutes)", breaks = "freedman-diaconis")
hist(distance, main = "Histogram of distance", xlab = "Distance (miles)", breaks = "freedman-diaconis")

#4
plot(air_time, distance, col = ifelse(distance < 1000, "red", "blue"),
     main = "Air Time vs Distance", xlab = "Air Time (minutes)", ylab = "Distance (miles)")
legend("topright", legend = c("Short distance", "Long distance"),
       col = c("red", "blue"), pch = 1)

#5
hist(arr_delay[distance < 1000], main = "Arrival Delay for Short Distance Flights", xlab = "Arrival Delay (minutes)", xlim = c(-200, 300), col = "red", breaks = 20)
hist(arr_delay[distance > 1000], main = "Arrival Delay for Long Distance Flights", xlab = "Arrival Delay (minutes)", xlim = c(-200, 300), col = "blue", breaks = 20, add = TRUE)
legend("topright", legend = c("Short distance", "Long distance"), col = c("red", "blue"), pch = 15)

#6
hist(dep_delay[distance < 1000], main = "Departure Delay for Short Distance Flights", xlab = "Departure Delay (minutes)", xlim = c(-200, 300), col = "red", breaks = 20)
hist(dep_delay[distance > 1000], main = "Departure Delay for Long Distance Flights", xlab = "Departure Delay (minutes)", xlim = c(-200, 300), col = "blue", breaks = 20, add = TRUE)
legend("topright", legend = c("Short distance", "Long distance"), col = c("red", "blue"), pch = 15)

#7
arrival_delay_by_hours_median <- numeric(24)
arrival_delay_by_hours_mean <- numeric(24)
for (i in 1:24) {
  flights_in_hour <- subset(flights, hour == i)
  if (length(flights_in_hour$hour) == 0) {
    arrival_delay_by_hours_median[i] <- NA
    arrival_delay_by_hours_mean[i] <- NA
  } else {
    arrival_delay_by_hours_median[i] <- median(flights_in_hour$arr_delay, na.rm = TRUE)
    arrival_delay_by_hours_mean[i] <- mean(flights_in_hour$arr_delay, na.rm = TRUE)
  }
}

plot(
  seq(1,24),
  arrival_delay_by_hours_median,
  main = "Median Arrival Delays By Hours",
  xlab = "Hours",
  ylab = "Median Arrival Delays (in Minutes)",
)
axis(1, at = seq(1, 24, by = 1))

# Identify hour with the maximum average delay
max_delay_hour <- which.max(arrival_delay_by_hours_median)
points(max_delay_hour, arrival_delay_by_hours_median[max_delay_hour], col = "red", pch = 16)
text(max_delay_hour, arrival_delay_by_hours_median[max_delay_hour], max_delay_hour, pos = 1, col = "red")

# Identify hour with the minimum average delay
min_delay_hour <- which.min(arrival_delay_by_hours_median)
points(min_delay_hour, arrival_delay_by_hours_median[min_delay_hour], col = "blue", pch = 16)
text(min_delay_hour, arrival_delay_by_hours_median[min_delay_hour], min_delay_hour, pos = 3, col = "blue")

#8
hist(arr_delay[month %in% c(6, 7, 8)], main = "Arrival Delay in Summer Months", xlab = "Arrival Delay (minutes)", col = "lightblue", breaks = 20)
hist(arr_delay[month %in% c(12, 1, 2)], main = "Arrival Delay in Winter Months", xlab = "Arrival Delay (minutes)", col = "lightgreen", breaks = 20, add = TRUE)
legend("topright", legend = c("Summer", "Winter"), fill = c("lightblue", "lightgreen"))

#9
install.packages("epade")
library(epade)


carrier_data <- data.frame(
  carrier = factor(ifelse(carrier == "AA", "AA", "WN")),
  arr_delay = arr_delay
)

histogram.ade(
  data = carrier_data,
  x = "arr_delay",
  group = "carrier",
  main = "Arrival Delay Distribution by Carrier (AA vs. WN)",
  xlab = "Arrival Delay (minutes)",
  col = c("blue", "red"),
  breaks = "FD",  # Adjust the number of breaks for better visualization
  xlim = c(-100, 100),
  wall = 1
)

#10
aa <- subset(flights, carrier == "AA")
wn <- subset(flights, carrier == "WN")

legend("topright", legend = c("AA", "WN"), col = c("blue", "red"), lty = 1)

aa_ad_mean <- numeric(12)
wn_ad_mean <- numeric(12)

for (i in 1:12) {
  aa_month <- subset(aa, aa$month == i)
  wn_month <- subset(wn, wn$month == i)
  aa_ad_mean[i] <- mean(aa_month$arr_delay, na.rm = TRUE)
  wn_ad_mean[i] <- mean(wn_month$arr_delay, na.rm = TRUE)
}

plot(
  seq(1, 12),
  aa_ad_mean,
  main = "Average Arrival Delays of Carriers Across the Months",
  col = "red",
  type = "l",
  xlab = "Months",
  ylab = "average Arrival Delays",
  ylim = c(-40, 40)
)
axis(1, at = seq(1, 12, by = 1))
lines(seq(1,12), wn_ad_mean, col = "blue")

rmarkdown::pandoc_available