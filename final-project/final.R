library(tidyverse)
library(dplyr)

################## Reading the files ##################

housing <- read.csv("housing.csv", header = T)
housing <- as.data.frame(housing)

housing$neighborhood <- as.factor(housing$neighborhood)
housing$type <- as.factor(housing$type)
housing$levels <- as.factor(housing$levels)
housing$cooling <- as.factor(housing$cooling)
housing$heating <- as.factor(housing$heating)
housing$fireplace <- as.factor(housing$fireplace)
housing$elementary <- as.factor(housing$elementary)
housing$middle <- as.factor(housing$middle)
housing$high <- as.factor(housing$high)


schools <- read.csv("schools.csv", header = T)
schools <- as.data.frame(schools)


##########################################################


###################### Data Cleaning #####################

summary(housing)
# After running summary for housing, we see several NA's and abnormally large values,
# such as a house with 25 baths. This dataset requires some cleaning.

# Generated boxplots to investigate any outliers as evident from the summary
boxplot(housing$beds)
boxplot(housing$baths)
boxplot(housing$sqft)
boxplot(housing$lotsize)
boxplot(housing$year)


# Show how these columns with missing data do not have small enough p-value
# to indicate any correlations.


# Use Single Imputation to replace missing data with the mean value in columns
# sqft, lotsize, levels, cooling, heating, and fireplace.
housingClean <- housing
# Before replacing
summary(housingClean)
housingClean$sqft[is.na(housingClean$sqft)] <- mean(housingClean$sqft, na.rm = T)
housingClean$lotsize[is.na(housingClean$lotsize)] <- mean(housingClean$lotsize, na.rm = T)

housingClean <- mutate(housingClean, levels = recode(levels, "?" = "missing"))

housingClean$cooling = factor(housingClean$cooling, levels=c(levels(housingClean$cooling), "missing"))
housingClean$heating = factor(housingClean$heating, levels=c(levels(housingClean$heating), "missing"))
housingClean$fireplace = factor(housingClean$fireplace, levels=c(levels(housingClean$fireplace), "missing"))
housingClean[housingClean == ""] <- "missing"

# Getting rid of the blanks in data frame levels
housingClean$cooling <- droplevels(housingClean$cooling)
housingClean$heating <- droplevels(housingClean$heating)
housingClean$fireplace <- droplevels(housingClean$fireplace)


# Getting rid of two rows where beds are 999 and where the soldprice is 664
housingClean <- housingClean[-(which(housingClean$beds %in% 999)),]
housingClean <- housingClean[-(which(housingClean$soldprice %in% 664)),]

# Columns beds, baths, and year have incorrect values that were probably entered
# by mistake. They will be replaced.
housingClean[housingClean$baths == 25, "baths"] <- 2
housingClean[housingClean$year == 1495, "year"] <- 1995
housingClean[housingClean$year == 2111, "year"] <- 2011
# Town house and townhouse should be the same value as well
housingClean[housingClean$type == "town house", "type"] <- "townhouse"
housingClean$type <- droplevels(housingClean$type)

# After all the performed cleaning
summary(housingClean)


##########################################################


###################### Data Merging ######################

housingClean$elementary_size <- schools$size[match(housingClean$elementary, schools$school)]
housingClean$elementary_rating <- schools$rating[match(housingClean$elementary, schools$school)]

housingClean$middle_size <- schools$size[match(housingClean$middle, schools$school)]
housingClean$middle_rating <- schools$rating[match(housingClean$middle, schools$school)]

housingClean$high_size <- schools$size[match(housingClean$high, schools$school)]
housingClean$high_rating <- schools$rating[match(housingClean$high, schools$school)]

# Check the updated data
head(housingClean)


##########################################################


################## One-variable visuals ##################

# Histograms
hist(housingClean$soldprice, col="deeppink4", xlab="Selling Price of the Home",
	main="Frequency of Selling Prices", xlim= c(300000,2400000), freq = T)
hist(housingClean$elementary_rating, col=rgb(0,1,0,0.25), xlab="Elementary Schools Ratings", breaks = 10,
	main="Frequency of Elementary Schools Ratings", xlim= c(1,10), freq = T)
hist(housingClean$middle_rating, col="plum1", xlab="Middle Schools Ratings", breaks = 10,
	main="Frequency of Middle Schools Ratings", xlim= c(1,10), freq = T)
hist(housingClean$high_rating, col="cornflowerblue", xlab="High Schools Ratings", breaks = 10,
	main="Frequency of High Schools Ratings", xlim= c(1,10), freq = T)

# Boxplots
boxplot(housingClean$sqft, ylab="Unit Square Footage",
		col="salmon2", main="Unit Square Footage of Sold Houses")

boxplot(housingClean$lotsize, ylab="Lot Sizes",
		col="salmon2", main="Lot Sizes of Sold Houses")

boxplot(housingClean$year, ylab="Year the Unit Was Built",
		col="salmon2", main="Years of Sold Houses")

boxplot(housingClean$elementary_size, ylab="Sizes",
		col="salmon2", main="Sizes of Elementary Schools")

boxplot(housingClean$middle_size, ylab="Sizes",
		col="salmon2", main="Sizes of Middle Schools")

boxplot(housingClean$high_size, ylab="Sizes",
		col="salmon2", main="Sizes of High Schools")

# Barplots
bedstable <- table(housingClean$beds)
barplot(bedstable, ylab="Count", main="Number of Bedrooms in Sold Houses", ylim= c(0,200),
		xlab="Number of Bedrooms", col=rgb(0,0,1,0.25))

bathstable <- table(housingClean$baths)
barplot(bathstable, ylab="Count", main="Number of Bathrooms in Sold Houses", ylim= c(0,250),
		xlab="Number of Bathrooms", col=rgb(1,0,1,0.25))

neighborhoodtable <- table(housingClean$neighborhood)
barplot(neighborhoodtable, ylab="Count", main="Number of Neighborhoods of Sold Houses", ylim= c(0,150),
		xlab="Neighborhoods", col=rgb(1,1,0,0.25))

levelstable <- table(housingClean$levels)
barplot(levelstable, ylab="Count", main="Number of Levels in Sold Houses", ylim= c(0,500),
		xlab="Levels", col=rgb(1,1,0,0.25))

coolingtable <- table(housingClean$cooling)
barplot(coolingtable, ylab="Count", main="Cooling in Sold Houses", ylim= c(0,500),
		xlab="Cooling", col=rgb(1,1,0,0.5))

heatingtable <- table(housingClean$heating)
barplot(heatingtable, ylab="Count", main="Heating in Sold Houses", ylim= c(0,550),
		xlab="Heating", col=rgb(1,0,0,0.5))

fireplacetable <- table(housingClean$fireplace)
barplot(fireplacetable, ylab="Count", main="Fireplace in Sold Houses", ylim= c(0,500),
		xlab="Fireplace", col=rgb(0,1,0,0.5))

elementarytable <- table(housingClean$elementary)
barplot(elementarytable, ylab="Count", main="Elementary Schools Belonging", ylim=c(0,50), las = 2,
	cex.names = 0.5, font = 2, col=rgb(0,0,1,0.5))

middletable <- table(housingClean$middle)
barplot(middletable, ylab="Count", main="Middle Schools Belonging", ylim=c(0,60), las = 2,
	cex.names = 0.6, font = 2, col=rgb(1,0,1,0.75))

hightable <- table(housingClean$high)
barplot(hightable, ylab="Count", main="High Schools Belonging", ylim=c(0,70), las = 2,
	cex.names = 0.6, font = 2, col=rgb(0,1,1,0.75))


##########################################################


################## Two-variable visuals ##################

# Boxplot that explores the relationship between sold price and housing type
boxplot(housingClean$soldprice ~ housingClean$type, ylab = "Sold Price in $",
	main="Sold Price by Housing Type", xlab = "Housing Type",
	col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1","coral1"))

boxplot(housingClean$soldprice ~ housingClean$neighborhood, main="Sold Price by Neighborhood",
	xlab="Neighborhood", ylab="Sold Price", col=c("mediumpurple1","yellowgreen","burlywood2",
	"cadetblue1","coral1"))


# Histogram for difference in sold price with and without cooling
hist(housingClean$soldprice[housingClean$cooling=="No"], col=rgb(1,1,0,0.25),
	main="Difference in Sold Price With and Without Cooling",
	xlab="Sold Price", freq = F)

hist(housingClean$soldprice[housingClean$cooling=="Yes"], col=rgb(0,1,1,0.25), freq = F, add = T)

dens1 <- density(housingClean$soldprice[housingClean$cooling=="No"])
dens2 <- density(housingClean$soldprice[housingClean$cooling=="Yes"])

lines(dens1, lwd = 2, col="yellow")
lines(dens2, lwd = 2, col="cadetblue1")

legend("topright", c("without cooling", "with cooling"),col=c("yellow","cadetblue1"),lwd=20)

# Histogram for difference in sold price with and without fireplace
hist(housingClean$soldprice[housingClean$fireplace=="No"], col=rgb(1,1,0,0.25),
	main="Difference in Sold Price With and Without Fireplace",
	xlab="Sold Price", freq = F)

hist(housingClean$soldprice[housingClean$fireplace=="Yes"], col=rgb(0,1,1,0.25), freq = F, add = T)

dens1 <- density(housingClean$soldprice[housingClean$fireplace=="No"])
dens2 <- density(housingClean$soldprice[housingClean$fireplace=="Yes"])

lines(dens1, lwd = 2, col="yellow")
lines(dens2, lwd = 2, col="cadetblue1")

legend("topright", c("without fireplace", "with fireplace"),col=c("yellow","cadetblue1"),lwd=20)


# Simple Scatterplots
year_line <- lm(soldprice ~ year, data = housingClean)
plot(housingClean$year, housingClean$soldprice, main="Prices by Years",
   xlab="Years", ylab="Prices")
abline(year_line, col="black", lty=1, lwd=2)

plot(housingClean$beds, housingClean$soldprice, main="Prices by the Number of Bedrooms",
   xlab="Number of Bedrooms", ylab="Prices")

plot(housingClean$elementary_rating, housingClean$soldprice, main="Prices by Elementary Schools Ratings",
   xlab="Elementary Schools Ratings", ylab="Prices")

plot(housingClean$middle_rating, housingClean$soldprice, main="Prices by Middle Schools Ratings",
   xlab="Middle Schools Ratings", ylab="Prices")

plot(housingClean$high_rating, housingClean$soldprice, main="Prices by High Ratings",
   xlab="High Schools Ratings", ylab="Prices")

# High Density Plot for 4) Two-variable visuals was moved
# to the bottom due to conflicting libraries


##########################################################


######################## Analysis ########################

## Regression Models
# Explore relationship between housing factors
model1 <- lm(soldprice ~ sqft + lotsize + year + beds + baths, data = housingClean)
summary(model1)

# Explore relationship between housing factors
model2 <- lm(soldprice ~ neighborhood + type + levels + cooling + heating
		+ fireplace, data = housingClean)
summary(model2)

# Explore relationship between school factors
school_model <- lm(soldprice ~ elementary_rating + middle_rating + high_rating, data = housingClean)
summary(school_model)

# Focus only on the factors that are significant
model3 <- lm(soldprice ~ year + beds + neighborhood + type + cooling + heating
		+ middle_rating + high_rating, data = housingClean)
summary(model3)

# Notice that now cooling stopped being significant, remove it
model3 <- lm(soldprice ~ year + beds + neighborhood + type + heating
		+ middle_rating + high_rating, data = housingClean)
summary(model3)

# Thus, variables that have the most effect on housing prices are:
# year, number of beds and baths, if it's part of Gold or Green neighborhood,
# if the type is multi-family home, single-family home or townhouse, if it has
# heating, and the ratings of middle and high schools nearby.

# Regression equation (only significant coefficient values):
# -5047171.2 + 2687.5 * year + 81548.7 * beds + 83757.2 * neighborhoodGold
# + 37338.7 * neighborhoodGreen + 556288.2 * typemulti-family home + 574672.4 * typesingle-family home
# + 81928.7 * typetownhouse + 33776.4 * heatingYes + 11194.3 * middle_rating + 42529.6 * high_rating


## Clustering Models
(kmeans_2 <- kmeans(housingClean[,c("soldprice","sqft")], 2))
plot(housingClean[,c("soldprice","sqft")], col = kmeans_2$cluster,
	main="K-Means of Sold Prices by Unit Square Footage",
	xlab="Sold Prices", ylab="Unit Square Footage")
points(kmeans_2$centers, col = 1:2, pch = 8, cex = 5)

(kmeans_3 <- kmeans(housingClean[,c("soldprice","high_rating")], 3))
plot(housingClean[,c("soldprice","high_rating")], col = kmeans_3$cluster,
	main="K-Means of Sold Prices by High School Ratings", xlab="Sold Prices",
	ylab="High School Ratings")
points(kmeans_3$centers, col = 1:3, pch = 8, cex = 5)


##########################################################


################## Sensitivity Analysis ##################

# Since I used Single Imputation to replace missing data,
# this time I will use Listwise Deletion.

housing_alternative <- housing
housing_alternative[housing_alternative == ""] <- NA
housing_alternative[housing_alternative == "?"] <- NA

housing_alternative$levels <- droplevels(housing_alternative$levels)
housing_alternative$cooling <- droplevels(housing_alternative$cooling)
housing_alternative$heating <- droplevels(housing_alternative$heating)
housing_alternative$fireplace <- droplevels(housing_alternative$fireplace)

housing_alternative <- housing_alternative[!is.na(housing_alternative$sqft) & !is.na(housing_alternative$lotsize)
			& !is.na(housing_alternative$levels) & !is.na(housing_alternative$cooling)
			& !is.na(housing_alternative$heating) & !is.na(housing_alternative$fireplace),]

# Columns beds, baths, and year have incorrect values that were probably entered
# by mistake. They will be deleted.
housing_alternative <- housing_alternative[-(which(housing_alternative$soldprice %in% 664)),]
housing_alternative <- housing_alternative[-(which(housing_alternative$baths %in% 25)),]
housing_alternative <- housing_alternative[-(which(housing_alternative$year %in% 1495)),]
housing_alternative <- housing_alternative[-(which(housing_alternative$year %in% 2111)),]

# Town house and townhouse should be the same value as well
housing_alternative[housing_alternative$type == "town house", "type"] <- "townhouse"
housing_alternative$type <- droplevels(housing_alternative$type)

summary(housing_alternative)


###### Data Merging
housing_alternative$elementary_size <- schools$size[match(housing_alternative$elementary, schools$school)]
housing_alternative$elementary_rating <- schools$rating[match(housing_alternative$elementary, schools$school)]

housing_alternative$middle_size <- schools$size[match(housing_alternative$middle, schools$school)]
housing_alternative$middle_rating <- schools$rating[match(housing_alternative$middle, schools$school)]

housing_alternative$high_size <- schools$size[match(housing_alternative$high, schools$school)]
housing_alternative$high_rating <- schools$rating[match(housing_alternative$high, schools$school)]

# Check the updated data
head(housing_alternative)


###### Analysis
## Regression Models
# Explore relationship between housing factors
model4 <- lm(soldprice ~ sqft + lotsize + year + beds + baths, data = housing_alternative)
summary(model4)

# Explore relationship between housing factors
model5 <- lm(soldprice ~ neighborhood + type + levels + cooling + heating
		+ fireplace, data = housing_alternative)
summary(model5)

# Explore relationship between school factors
school_model2 <- lm(soldprice ~ elementary_rating + middle_rating + high_rating, data = housing_alternative)
summary(school_model2)

# Focus only on the factors that are significant
model6 <- lm(soldprice ~ year + beds + neighborhood + type + levels + heating
		+ middle_rating + high_rating, data = housing_alternative)
summary(model6)


## Clustering Models
(kmeans_4 <- kmeans(housing_alternative[,c("soldprice","sqft")], 2))
plot(housing_alternative[,c("soldprice","sqft")], col = kmeans_4$cluster,
	main="K-Means of Sold Prices by Unit Square Footage",
	xlab="Sold Prices", ylab="Unit Square Footage")
points(kmeans_4$centers, col = 1:2, pch = 8, cex = 5)

(kmeans_5 <- kmeans(housing_alternative[,c("soldprice","high_rating")], 3))
plot(housing_alternative[,c("soldprice","high_rating")], col = kmeans_5$cluster,
	main="K-Means of Sold Prices by High School Ratings", xlab="Sold Prices",
	ylab="High School Ratings")
points(kmeans_5$centers, col = 1:3, pch = 8, cex = 5)


library(car)
# High Density Plot for 4) Two-variable visuals
# Had to move it to the bottom because library(car) was overwriting
# some functions of dplyr required for data cleaning
sp(housingClean$middle_rating, housingClean$soldprice, jitter = list(x=2,y=2),
	main="Prices by Middle High Ratings", xlab="Middle Schools Ratings", ylab="Prices")
sp(housingClean$high_rating, housingClean$soldprice, jitter = list(x=2,y=2),
	main="Prices by High Ratings", xlab="High Schools Ratings", ylab="Prices")

sunflowerplot(housingClean$elementary, housingClean$elementary_rating,
	main="Number of Elementary Schools", xlab="Elementary Schools Ratings",
	ylab="Ratings (1-10)")


##########################################################
