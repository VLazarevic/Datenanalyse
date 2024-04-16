library(nycflights13)
data("flights")
attach(flights)

library(laeken)
data(eusilc)
attach(eusilc)
library(DescTools)


#1

flights_arr_delay_hour <- flights$arr_delay~flights$hour

boxplot(
  flights_arr_delay_hour,
  outline = FALSE,
  main = "Boxplot (arrival delays by hours)",
  xlab = "Hours",
  ylab  = "Arrival Delay (minutes)"
)


#2
flights_arr_delay_month <- flights$arr_delay~flights$month

boxplot(
  flights_arr_delay_month,
  outline = FALSE,
  main = "Boxplot (arrival delays by months)",
  xlab = "Months",
  ylab  = "Arrival Delay (minutes)"
)

#3

december_flights <-  subset(flights, flights$month == 12)
flights_arr_delay_day <- december_flights$arr_delay~december_flights$day

boxplot(
  flights_arr_delay_day,
  outline = FALSE,
  main = "Boxplot (arrival delays by days in december)",
  xlab = "Days in December",
  ylab  = "Arrival Delay (minutes)"
)

#4

ordered_carrier_data <- with(flights, reorder(flights$carrier, flights$dep_delay, median, na.rm = TRUE))

boxplot(
  flights$dep_delay~ordered_carrier_data,
  outline = FALSE,
  notch = TRUE,
  main = "Boxplot (departure delays by carriers)",
  xlab = "Carriers",
  ylab = "Departure Delay (minutes)"
)

#5

boxplot(
  eusilc$hy040n~eusilc$db040,
  outline = TRUE,
  main = "Boxplot (income from rental)",
  ylab = "Incom from Rental (net)",
  xlab = "",
  xaxt = "n"
)
title(xlab = "Federal State of Austria", mgp = c(4, 1, 0))

axis(1, at=c(1:9), labels=levels(eusilc$db040), cex.axis=0.75,las = 3)


#6

men <- subset(eusilc, age >= 55 & age <= 59 & rb090 == "male")
women <- subset(eusilc, age >= 55 & age <= 59 & rb090 == "female")

men_agg <- aggregate(rb030~db040, data = men, FUN = length)
women_agg <- aggregate(rb030~db040, data = women, FUN = length)

men_agg_ret <- aggregate(rb030~db040, data = men[men$pl030 == 5, ], FUN = length)
men_agg_age <- aggregate(rb030~db040, data = men[men$py100n > 0, ], FUN = length)

women_agg_ret <- aggregate(rb030~db040, data = women[women$pl030 == 5, ], FUN = length)
women_agg_age <- aggregate(rb030~db040, data = women[women$py100n > 0, ], FUN = length)

dataset <- data.frame(
  state = levels(rbind(men, women)$db040),
  men_ret = men_agg_ret$rb030/men_agg$rb030,
  men_age = men_agg_age$rb030/men_agg$rb030,
  women_ret = women_agg_ret$rb030/women_agg$rb030,
  women_age = women_agg_age$rb030/women_agg$rb030
)

dataset_matrix <- as.matrix(dataset[, -1])
rownames(dataset_matrix) <- dataset$state
par(mar = c(6, 4, 4, 2))

col <- c(rgb(0,0.2,1,0.7), rgb(0,0.2,1,0.3), rgb(1,0,1,0.7), rgb(1,0,1,0.3))

barplot(t(dataset_matrix),beside = TRUE,
        main = "Probability by Federal State", xlab = "",
        ylab = "Probability", col = col,
        las = 3)
title(xlab = "Federal State of Austria", mgp = c(5, 1, 0))
legend("topright", legend = c("Male Retired", "Male Benefits", "Female Retired", "Female Benefits"),
       fill = col)

#7

person_benefits <- subset(rbind(men, women), rbind(men, women)$py100n > 0)

person_benefits$spec_group <- interaction(person_benefits$db040, person_benefits$rb090)

col_men <- rainbow(9, alpha = 0.7)
col_women <- rainbow(9, alpha = 0.4)

plot(Lc(person_benefits$py100n ~ (person_benefits$spec_group)),
     col=union(col_men, col_women))

legend( "topleft",legend = levels(person_benefits$spec_group)[1:9], col = col_men, pch=16, title = "Male/State")
legend( "bottomright",legend = levels(person_benefits$spec_group)[10:18], col = col_women, pch=16, title = "Female/State")

#8

boxplot(person_benefits$py100n~person_benefits$spec_group, outline = FALSE,
        col = ifelse(grepl("female", levels(person_benefits$spec_group)), rgb(1,0,1,0.7), rgb(0,0.2,1,0.7)),
        main = "Boxplot (old age benefits by state and gender)", xlab = "Federal State of Austria",
        ylab = "Income", xaxt="n", notch = TRUE
)


legend("topright", legend = c("Male", "Female"), fill = c(rgb(0,0.2,1,0.7), rgb(1,0,1,0.7)))

state <- strsplit(levels(person_benefits$spec_group), ".")[[1]]
axis(1,at=1:length(levels(person_benefits$spec_group)), labels = levels(person_benefits$spec_group),las=3)

print(person_benefits$spec_group)
