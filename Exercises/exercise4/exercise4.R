setwd("/Users/valentino/Documents/Studium/Semester 4/Datenanalyse/Exercises")

ChildSmokers = read.csv("ChildSmokers.csv") 
PISA = read.csv("PISA2018.csv") 
Diamonds = read.csv("Diamonds.csv") 

library(MASS)

#1
non_smokers <- subset(ChildSmokers, ChildSmokers$Smoker=="non") 
smokers <- subset(ChildSmokers, ChildSmokers$Smoker=="smoker")

boxplot(smokers$FEV,non_smokers$FEV, 
        outline = TRUE, notch = TRUE,  
        main="Boxplot (FEV of Smokers and Non Smokers)" , xlab="Smokers and Non Smokers",ylab="FEV", 
        col = c("brown","lightgreen"))

#2
plot(non_smokers$Age, non_smokers$FEV, xlim = c(min(ChildSmokers$Age),max(ChildSmokers$Age)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), main ="Scatterplot (Age in Comparison to FEV)" , 
     xlab="Age", ylab="FEV", pch=19, col=rgb(0.56, 0.93, 0.56, 0.5))
points(smokers$Age, smokers$FEV, pch=19, col=rgb(0.65, 0.16, 0.16, 0.5))
legend("topleft", title = "Legend", legend = c("Smokers","Non Smokers"), fill = c("brown", "lightgreen"))

#par(mfrow=c(2,1))
plot(smokers$Age, smokers$FEV, xlim = c(min(ChildSmokers$Age),max(ChildSmokers$Age)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), 
     main ="Scatterplot (Age in Comparison to FEV) - Smokers" , xlab="Age", ylab="FEV", 
     pch=19, col=rgb(0.65, 0.16, 0.16, 0.5))

plot(non_smokers$Age, non_smokers$FEV, xlim = c(min(ChildSmokers$Age),max(ChildSmokers$Age)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), 
     main ="Scatterplot (Age in Comparison to FEV) - Non Smokers", xlab="Age", 
     ylab="FEV", pch=19, col=rgb(0.56, 0.93, 0.56, 0.5))

#Female and Male betrachten

plot(non_smokers$Height, non_smokers$FEV, xlim = c(min(ChildSmokers$Height),max(ChildSmokers$Height)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), main ="Scatterplot (Height in Comparison to FEV)" , 
     xlab="Height", ylab="FEV", pch=19, col=rgb(0.56, 0.93, 0.56, 0.5))
points(smokers$Height, smokers$FEV, pch=19, col=rgb(0.65, 0.16, 0.16, 0.5))
legend("topleft", title = "Legend", legend = c("Smokers","Non Smokers"), fill = c("brown", "lightgreen"))

#par(mfrow=c(2,1))
plot(smokers$Height, smokers$FEV, xlim = c(min(ChildSmokers$Height),max(ChildSmokers$Height)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), 
     main ="Scatterplot (Height in Comparison to FEV) - Smokers" , xlab="Height", ylab="FEV", 
     pch=19, col=rgb(0.65, 0.16, 0.16, 0.5))

plot(non_smokers$Height, non_smokers$FEV, xlim = c(min(ChildSmokers$Height),max(ChildSmokers$Height)), 
     ylim=c(min(ChildSmokers$FEV),max(ChildSmokers$FEV)), 
     main ="Scatterplot (Height in Comparison to FEV) - Non Smokers", xlab="Height", 
     ylab="FEV", pch=19, col=rgb(0.56, 0.93, 0.56, 0.5))

#3

par(mfrow = c(1,2))

#Age
smokers_age <- kde2d(smokers$Age,smokers$FEV,
                         lims = c(c(min(ChildSmokers$Age),max(ChildSmokers$Age)),
                         c(min(ChildSmokers$FEV),max(ChildSmokers$FEV))), n = 100)
nonsmokers_age <- kde2d(non_smokers$Age,non_smokers$FEV ,
                         lims = c(c(min(ChildSmokers$Age),max(ChildSmokers$Age)), 
                         c(min(ChildSmokers$FEV),max(ChildSmokers$FEV))), n = 100)
image(smokers_age, main = "Smokers", xlab = "Age" , ylab = "FEV") 
image(nonsmokers_age, main = "Non Smokers", xlab = "Age" , ylab = "FEV")

#Height
smokers_height <- kde2d(smokers$Height,smokers$FEV,
                         lims = c(c(min(ChildSmokers$Height),max(ChildSmokers$Height)),
                         c(min(ChildSmokers$FEV),max(ChildSmokers$FEV))), n = 100)
nonsmokers_height <- kde2d(non_smokers$Height,non_smokers$FEV ,
                         lims = c(c(min(ChildSmokers$Height),max(ChildSmokers$Height)), 
                         c(min(ChildSmokers$FEV),max(ChildSmokers$FEV))), n = 100)
image(smokers_height, main = "Smokers", xlab = "Height" , ylab = "FEV") 
image(nonsmokers_height, main = "Non Smokers", xlab = "Height" , ylab = "FEV")

#4
par(mfrow = c(1,1))

plot(
  PISA$Overall.number.of.students.per.teacher,
  PISA$PISA_reading_boy,
  col = "blue",
  main = "Scatterplot (scores and number of students per teache)", xlab = "All Number of Students per Teacher",
  ylab = "PISA Reading Scores",
  ylim = c(min(PISA$PISA_reading, na.rm = TRUE), max(PISA$PISA_reading, na.rm = TRUE) + 50),
  pch=19
  )
points(
  PISA$Overall.number.of.students.per.teacher, PISA$PISA_reading_girl,
  col = "pink", pch=19
)
abline(
  lm(PISA_reading_boy ~ Overall.number.of.students.per.teacher, data = PISA), col = "lightblue"
) 
abline(
  lm(PISA_reading_girl ~ Overall.number.of.students.per.teacher, data = PISA),
  col = "lightpink" )
legend(
  "topright",
  legend = c("Boys", "Girls", "Boys lm()", "Girls lm()"), col = c("blue", "pink", "lightblue", "lightpink"),
  pch = 19,
  title = "Gender"
)

#5
PISA_EU <- subset(PISA, Continent == "Europe") 
plot(
  PISA_EU$Overall.number.of.students.per.teacher, PISA_EU$PISA_reading_boy, col = "blue",
  main = "Scatterplot (scores and number of students per teacher)", xlab = "All Number of Students per Teacher",
  ylab = "PISA Reading Scores",
  ylim = c(min(PISA_EU$PISA_reading, na.rm = TRUE), max(PISA_EU$PISA_reading, na.rm = TRUE) + 50)
)

points( PISA_EU$Overall.number.of.students.per.teacher, PISA_EU$PISA_reading_girl, col = "pink"
)
abline(
  lm(PISA_EU ~ Overall.number.of.students.per.teacher, data = PISA_EU), col = "lightblue"
) 
abline(
  lm(PISA_reading_girl ~ Overall.number.of.students.per.teacher, data = PISA_EU),
  col = "lightpink" 
)
legend("topright", legend = c("Boys", "Girls", "Boys lm()", "Girls lm()"), col = c("blue", "pink", "lightblue", "lightpink"),
  pch = 19, title = "Gender"
)

#6

xlim <- c(min(Diamonds$Colour, na.rm=TRUE) * 0.9, max(Diamonds$Colour, na.rm=TRUE) * 1.1) 
ylim <- c(min(Diamonds$Price, na.rm=TRUE) * 0.9, max(Diamonds$Price, na.rm=TRUE) * 1.1)
colour_price <- kde2d(Diamonds$Colour, Diamonds$Price , n=100 , lims = c(xlim, ylim))
image(colour_price, main = "DBE (Colour vs Price)", xlab = "Colour" , ylab = "Price")
contour(colour_price, add =TRUE)


#7

xlim <- c(min(Diamonds$Clarity, na.rm=TRUE) * 0.9, max(Diamonds$Clarity, na.rm=TRUE) * 1.1)
clarity_price <- kde2d(Diamonds$Clarity, Diamonds$Price , lims = c(xlim, ylim))
image(clarity_price, main = "BDE (Clarity vs Price)", xlab = "Clarity" , ylab = "Price")
contour(clarity_price, add =TRUE)

#8

boxplot(
  Diamonds$Price ~ Diamonds$Lab,
  main = "Parallel Boxplots (Price and Laboratories)", ylab = "Price",
  xlab = "Laboratories",
  col = c(rgb(0, 0, 1), rgb(0, 0, 0.7), rgb(0, 0, 0.4))
)

xlim <- c(min(Diamonds$Carat, na.rm=TRUE)*0.9 ,max(Diamonds$Carat, na.rm=TRUE)*1.1)
carat_price_bde <- kde2d(Diamonds$Carat, Diamonds$Price, n=100 , lims = c(xlim, ylim))
image(carat_price_bde, main = "BDE (Carat vs Price)", xlab = "Carat" , ylab = "Price (in $)")
contour(carat_price_bde, add =TRUE)


#9
log_transformed_price <- log(Diamonds$Price) plot(Diamonds$Carat, log_transformed_price
                                                  , pch=4, col = "blue"
                                                  , main="Log-Transformation of Price by Carat"
                                                  , xlab="Carat", ylab="Price (in $)"
                                                  , yaxt="n"
)
abline(lm( log_transformed_price ~ Diamonds$Carat ), col = "red" , lwd=2)
legend("topleft"
       ,legend = c("Meassured","Predicted") ,fill=c("blue","red"))
ticks <- seq(floor(min(log_transformed_price,na.rm=TRUE)),floor(max(log_transformed_price, na.rm=TRUE)))
axis(2, at=ticks, label=floor(exp(ticks)),cex.axis=0.7)