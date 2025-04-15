install.packages("corrplot")
install.packages("olsrr")

library(corrplot, olsrr)
# Imports the needed dataset "LE_Data"
data <- file.choose() |> read.csv()

# Declares Variables
Vehicles <- data$RegisteredVehicles
DR <- data$DeathRate
RoadL <- data$RoadLength
data <- data.frame(Vehicles, DR, RoadL)
data

# Normality Test of the variables
shapiro.test(Vehicles)
shapiro.test(DR)
shapiro.test(RoadL)

# Tests the Correlation using Pearson's R Moment as the data is normal
cor(data, method = "pearson")

# Tests is there is multicollinearity between IVs. If value is > 0.90, there is collinearity between IVs
corrplot(cor(data, method = "pearson"), method = "number")

# Command that starts simple linear regression
linearmodel <- lm(DR ~ Vehicles+RoadL, data = data)
summary(linearmodel)
coef(linearmodel)

print(c(coef(linearmodel)[1]))

# Tests for Collinearity. Data is collinear if Tolerance < 0.1 and VIF > 10
olsrr::ols_vif_tol(linearmodel)
