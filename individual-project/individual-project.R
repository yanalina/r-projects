# Open my dataset
insurance <- read.csv("insurance.csv", header=T)
summary(insurance)

# ITEMS 1 and 4: Generate boxplots, histograms, and bar plots

## Categorizing data
bmicat <- c()
bmicat[insurance$bmi <= 18.4] <- "underweight"
bmicat[insurance$bmi > 18.5 & insurance$bmi <= 24.9] <- "normal"
bmicat[insurance$bmi > 24.9 & insurance$bmi <= 29.9] <- "overweight"
bmicat[insurance$bmi > 29.9 & insurance$bmi <= 34.9] <- "obese"
bmicat[insurance$bmi > 34.9] <- "extremely obese"
insurance<- cbind(insurance, bmicat)

agecat <- c()
agecat[insurance$bmi > 17 & insurance$bmi <= 30] <- "young adults"
agecat[insurance$bmi > 30 & insurance$bmi <= 45] <- "middle-aged adults"
agecat[insurance$bmi > 45] <- "old adults"
insurance<- cbind(insurance, agecat)

## Boxplots and Barplots
boxplot(insurance$charges ~ insurance$region, main = "Insurance Charges\nAcross Four Regions",
	xlab = "Regions", ylab = "Insurance Charges in Dollars",
	col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,1,0,0.25)))

boxplot(insurance$charges ~ insurance$sex, main = "Insurance Charges\nBased on Gender",
	xlab = "Gender", ylab = "Insurance Charges in Dollars",
	col = c(rgb(1,0,1,0.25), rgb(0,1,1,0.25)))

# legend("topright", c("northeast", "northwest", "southeast", "southwest"), col=c("red","green","purple","yellow"),lwd=20)

boxplot(insurance$charges ~ insurance$children, main = "Insurance Charges\nBased on the Number of Children",
	xlab = "Number of Children", ylab = "Insurance Charges in Dollars",
	col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,1,0,0.25), "orange", "pink"))

boxplot(insurance$charges ~ insurance$bmicat, main = "Insurance Charges\nBased on the BMI Category",
	xlab = "BMI Categories", ylab = "Insurance Charges in Dollars",
	col=c(rgb(1,0,0,0.75), rgb(0,1,0,0.25), rgb(1,0,0,0.5), rgb(1,0,0,0.25), rgb(1,1,0,0.25)))

insurance_females <- subset(insurance, sex == "female")
insurance_males <- subset(insurance, sex == "male")

boxplot(insurance_females$charges ~ insurance_females$children, main = "Insurance Charges\nBased on the Number of Children\n(Females)",
	xlab = "BMI Categories", ylab = "Insurance Charges in Dollars",
	col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,1,0,0.25), "orange", "pink"))
boxplot(insurance_males$charges ~ insurance_males$children, main = "Insurance Charges\nBased on the Number of Children\n(Males)",
	xlab = "BMI Categories", ylab = "Insurance Charges in Dollars",
	col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,1,0,0.25), "orange", "pink"))

boxplot(insurance$charges ~ insurance$agecat, main = "Insurance Charges\nBased on the Age Category",
	xlab = "Age Categories", ylab = "Insurance Charges in Dollars",
	col=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5), rgb(1,1,0,0.5)))


childrentab <- table(insurance$children)
	barplot(childrentab, ylab="Count", main="Number of Children of Insurance Holders", ylim= c(0,1200),
	col=rgb(0,0,1,0.25))

smokertab <- table(insurance$smoker)
	barplot(smokertab, ylab="Count", main="Smoker Status of Insurance Holders", ylim= c(0,1200),
	col=c("green", "red"))

bmitab <- table(insurance$bmicat)
	barplot(bmitab, ylab="Count", main="BMI by Category of Insurance Holders", ylim= c(0,1200),
	col=c(rgb(1,0,0,0.75), rgb(0,1,0,0.25), rgb(1,0,0,0.5), rgb(1,0,0,0.25), rgb(1,1,0,0.25)))

agetab <- table(insurance$agecat)
	barplot(agetab, ylab="Count", main="Age by Category of Insurance Holders", ylim= c(0,1200),
	col=c(rgb(0,1,1,0.5), rgb(1,0,1,0.5), rgb(1,1,0,0.5)))



## Histograms
# Exploring trends before planning my histogram
# stem(insurance$charges[insurance$smoker=="yes"])
# stem(insurance$charges[insurance$smoker=="no"])

# Differences in Charges: Smoker Status
hist(insurance$charges[insurance$smoker=="yes"], col=rgb(1,0,0,0.25),
	ylim = c(0, 0.00009), main="Insurance Charges Based on Smoker Status", freq = F,
	xlim= c(1120,63800), xlab = "Insurance Charges in Dollars")

hist(insurance$charges[insurance$smoker=="no"], col=rgb(0,1,0,0.25), freq = F, add = T)

dens1 <- density(insurance$charges[insurance$smoker=="yes"])
dens2 <- density(insurance$charges[insurance$smoker=="no"])

lines(dens1, lwd = 2, col="red")
lines(dens2, lwd = 2, col="green")

abline(v = mean(insurance$charges), col="blue", lty=2, lwd=3)

legend("topright", c("Smoker", "Non-smoker"), col=c("red","green"),lwd=20)


# Differences in Charges: Gender
hist(insurance$charges[insurance$sex=="male"], col=rgb(0,1,1,0.25),
	ylim = c(0, 0.00009), main="Insurance Charges Based on Gender", freq = F,
	xlim= c(1120,63800), xlab = "Insurance Charges in Dollars")

hist(insurance$charges[insurance$sex=="female"], col=rgb(1,0,1,0.25), freq = F, add = T)

dens1 <- density(insurance$charges[insurance$sex=="male"])
dens2 <- density(insurance$charges[insurance$sex=="female"])

lines(dens1, lwd = 2, col="blue")
lines(dens2, lwd = 2, col="pink")

abline(v = mean(insurance$charges), col="blue", lty=2, lwd=3)

legend("topright", c("Male", "Female"), col=c(rgb(0,1,1,0.25),rgb(1,0,1,0.25)),lwd=20)




## ITEMS 2 AND 3: Scatter Plot with Regression Line

model1<-lm(charges~age, data=insurance)
abline(model1)
plot(model1)
plot(insurance$age, insurance$charges, col=insurance$age, ylab="Insurance Charges in Dollars", xlab="Age",
	main="Regression Model\nCan Age Help Predict Costs?")
abline(model1)
smooth <- smooth.spline(insurance$age, insurance$charges, spar=1)
lines(smooth, col='red', lwd = 2, lty = 2)
summary(model1)
# Equation: 3165.9 + 257.7 * x





## ITEM 4: Pie Charts

bmitab <- table(insurance$bmicat)
bmilab <- paste("Number of Children: ", names(bmitab))
bmilab <- paste(names(bmitab), "\n", childrentab)
pie(bmitab, main = "Number of People per each BMI Category")


agetab <- table(insurance$agecat)
agelab <- paste("Number of Children: ", names(agetab))
agelab <- paste(names(agetab), "\n", childrentab)
pie(agetab, main = "Number of People per each Age Category")