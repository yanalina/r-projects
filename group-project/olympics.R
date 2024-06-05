#################################################
## Exploratory Data Analysis and Visualization
## Group Project
##
## 1. Summer Data
## 1.1. Data Cleaning
## 1.2. Summary Statistics
## 2.3. Exploratory Plots
## 2. Winter Data
## 2.1.  Data Cleaning
## 2.2. Summary Statistics
## 2.3. Exploratory Plots
## 3. Paralympic Data
## 3.1.  Data Cleaning
## 3.2. Summary Statistics
## 3.3. Exploratory Plots
## 4. Merged Data
## 4.1.  Data Cleaning
## 4.2. Exploratory Plots
#################################################


# Load any libraries that may be necessary
library(ggplot2)
library(dplyr)
library(car)
library(hexbin)
library(MASS)
library(reshape2)
library(e1071)
library(readxl)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 1. Summer Data

# read file into R
summer <- read.csv("summer_olympics.csv", header=T)

summary(summer)
head(summer)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.1. Data Checking and Cleaning:

# Changing characters to factors and changing Seniority to a factor
summer$Sex <- as.factor(summer$Sex)
summer$Games <- as.factor(summer$Games)
summer$Medal <- factor(summer$Medal)
summer$City <- as.factor(summer$City)
summer$Country <- as.factor(summer$Country)
summer$Season <- as.factor(summer$Season)
summer$Sport <- as.factor(summer$Sport)
summer$Event <- as.factor(summer$Event)
summer$Team <- as.factor(summer$Team)
summer$NOC <- as.factor(summer$NOC)

summary(summer)

# Filling in missing NAs for Age, Height and Weight using mean

summer2 <- impute(summer[,2:4], what='mean')

# Replace original variables with imputed variables
summer$Age <- summer2[,1]
summer$Height <- summer2[,2]
summer$Weight <- summer2[,3]

# View the summary of the fixed data frame
summary(summer)
head(summer)
str(summer)

# Changing column names to lowercase for easier coding
names(summer) <- tolower(names(summer))
head(summer)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.2 Summary Statistics 

# Checking for outliers:

# Using log for better visibility of boxplots:

summer$log_age <- log(summer$age)
summer$log_weight <- log(summer$weight)
summer$log_height <- log(summer$height)

# Boxplots:
boxplot(summer$log_age, main = "Boxplot of Age", ylab = "Log(Age)", col = "skyblue")
boxplot(summer$log_weight, main = "Boxplot of Weight", ylab = "Log(Weight)", col = "lightgreen")
boxplot(summer$log_height, main = "Boxplot of Height", ylab = "Log(Height)", col = "plum1")

#many outliers - this makes sense as the nature of sports is there are
#different builds for different sports. I will not remove outliers as this can result in 
#removing certain sports from the dataset which can affect the overall question we are asking

# Boxplot of year that did not need to be transformed
boxplot(summer$year, main = "Boxplot of Year", ylab = "Year", col = "orange")
# more events each olympics - that us why it is skewed right

# Pie chart of the "Sex" variable total


gender_counts <- table(summer$sex)
colors <- c("red", "blue")  
labels <- c("Female:\n4272 (27%)", "Male:\n11802 (73%)")

pie(gender_counts, labels = labels, col = colors,
    main = "Gender Distribution", cex = 0.8)
#females are only 27% of the total athletes - this is not evenly distributed

par(mar = c(5, 5, 4, 2) + 0.1)  
# Bar plot for the total count of country
barplot(table(summer$country), 
        main = "Total Count of Country", 
        xlab = "Team Country", 
        ylab = "Count", 
        col = "lightgreen",
        las = 2, 
        cex.names = 0.4)

# Bar plot for the total count of 'medal'
barplot(table(summer$medal), main = "Total Count of Medal", xlab = "Medal", ylab = "Count", col = c("peru", "grey", "gold"))


# Bar plot for the total count of 'sport'
par(mar = c(5, 5, 4, 2) + 0.1)  # Adjust the values as needed
barplot(table(summer$sport), main = "Total Count of Sport", xlab = "Sport", ylab = "Count", col = "blue",
        las = 2,  # Rotate x-axis labels vertically
        cex.names = 0.5)  # Set the size of x-axis labels)

# the data is not evenly distributed, this has to do with the amount of medals per sport which makes sense
# a sport like athletics has many different events where a sport like polo would only have one tournament
#resulting in one gold, silver and bronze medal

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 1.3. Exploratory Data Plots

#top countries with medals
top_countries <- table(summer$country)
top_countries <- top_countries[order(top_countries, decreasing = TRUE)]

# Select the top 20 countries
top_countries_subset <- head(top_countries, 20)

# Create a bar plot with rotated and smaller labels
barplot(top_countries_subset, main = "Top 20 Countries with Medals", xlab = "Country", ylab = "Number of Medals", col = "skyblue", las = 2, cex.names = 0.7)

# Breaking down the top five countries' medal distribution
top_countries <- table(summer$country)
top_countries <- top_countries[order(top_countries, decreasing = TRUE)]
top_five <- names(head(top_countries, 5))

# Subsetting the data for the top five countries
top_five_data <- subset(summer, country %in% top_five)

# Creating a new dataset with the medal counts
medal_counts_top_five <- top_five_data %>%
  group_by(country, medal) %>%
  summarise(count = n())

# Ordering the countries by the total number of medals
medal_counts_top_five$country <- factor(medal_counts_top_five$country, levels = names(sort(table(top_five_data$country), decreasing = TRUE)))

# Specifying the order of fill levels
medal_counts_top_five$medal <- factor(medal_counts_top_five$medal, levels = c("Bronze", "Silver", "Gold"))

# Creating a grouped bar plot with custom colors
ggplot(medal_counts_top_five, aes(x = country, y = count, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bronze" = "peru", "Silver" = "grey", "Gold" = "gold")) +
  labs(title = "Medal Distribution for Top 5 Countries", x = "Country", y = "Number of Medals", fill = "Medal Type") +
  theme_minimal()

# The top 5 countries are USA, Russia, Germany, UK and France
# USA and Russia's most medals are Gold
# UK most medals is Silver
#Germany and France most medals are Bronze

# Looking at total distribution of medals for a comparison of top countries distributions vs totals

# Defining the order of medal types
medal_order <- c("Bronze", "Silver", "Gold")

# Creating a new dataset with the total medal counts by year
total_medal_counts_by_year <- summer %>%
  group_by(year, medal) %>%
  summarise(count = n())

# Ordering the years chronologically
total_medal_counts_by_year$year <- factor(total_medal_counts_by_year$year, levels = unique(total_medal_counts_by_year$year))

# Ordering the medal types
total_medal_counts_by_year$medal <- factor(total_medal_counts_by_year$medal, levels = medal_order)

# Creating a bar plot with custom colors
ggplot(total_medal_counts_by_year, aes(x = year, y = count, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bronze" = "peru", "Silver" = "grey", "Gold" = "gold")) +
  labs(title = "Total Medal Distribution by Year", x = "Year", y = "Number of Medals", fill = "Medal Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Exploring the time series of the top 3 countries:

# Looking at USA over time

# Defining the order of medal types
medal_order <- c("Bronze", "Silver", "Gold")

# Subsetting the data for the USA
usa_data <- subset(summer, country == "USA")

# Creating a new dataset with the medal counts by year
usa_medal_counts_by_year <- usa_data %>%
  group_by(year, medal) %>%
  summarise(count = n())

# Ordering the years chronologically
usa_medal_counts_by_year$year <- factor(usa_medal_counts_by_year$year, levels = unique(usa_medal_counts_by_year$year))

# Ordering the medal types
usa_medal_counts_by_year$medal <- factor(usa_medal_counts_by_year$medal, levels = medal_order)

# Creating a bar plot with custom colors
ggplot(usa_medal_counts_by_year, aes(x = year, y = count, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bronze" = "peru", "Silver" = "grey", "Gold" = "gold")) +
  labs(title = "Medal Distribution for USA by Year", x = "Year", y = "Number of Medals", fill = "Medal Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# If we dive deeepr into a time series of medal distribution we see the USA consistently has more golds in each year. Something can also see their portion of medals has started to decline over the years

# Looking at Russia over the years
# Define the order of medal types
medal_order <- c("Bronze", "Silver", "Gold")

# Subsetting the data for Russia
russia_data <- subset(summer, country == "Russia")

# Creating a new dataset with the medal counts by year
russia_medal_counts_by_year <- russia_data %>%
  group_by(year, medal) %>%
  summarise(count = n())

# Ordering the years chronologically
russia_medal_counts_by_year$year <- factor(russia_medal_counts_by_year$year, levels = unique(russia_medal_counts_by_year$year))

# Ordering the medal types
russia_medal_counts_by_year$medal <- factor(russia_medal_counts_by_year$medal, levels = medal_order)

# Creating a bar plot with custom colors
ggplot(russia_medal_counts_by_year, aes(x = year, y = count, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bronze" = "peru", "Silver" = "grey", "Gold" = "gold")) +
  labs(title = "Medal Distribution for Russia by Year", x = "Year", y = "Number of Medals", fill = "Medal Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Russia's medal wins peaks in 1980 and then steadily goes down in more recent years

# Looking at Germany over time: 

# Defining the order of medal types
medal_order <- c("Bronze", "Silver", "Gold")

# Subsetting the data for the Germany
germany_data <- subset(summer, country == "Germany")

# Creating a new dataset with the medal counts by year
germany_medal_counts_by_year <- germany_data %>%
  group_by(year, medal) %>%
  summarise(count = n())

# Ordering the years chronologically
germany_medal_counts_by_year$year <- factor(germany_medal_counts_by_year$year, levels = unique(germany_medal_counts_by_year$year))

# Ordering the medal types
germany_medal_counts_by_year$medal <- factor(germany_medal_counts_by_year$medal, levels = medal_order)

# Creating a bar plot with custom colors
ggplot(germany_medal_counts_by_year, aes(x = year, y = count, fill = medal)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Bronze" = "peru", "Silver" = "grey", "Gold" = "gold")) +
  labs(title = "Medal Distribution for Germany by Year", x = "Year", y = "Number of Medals", fill = "Medal Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))  # Rotate x-axis labels for better visibility

# Finally with Germany we can see the same trend that after around 1894 their total medals reduce

##======================================================================================================


## 2. Winter Data

winter <- read.csv("winter.csv", header = T)

# Print out dataset's summary and head
summary(winter)
head(winter)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.1.  Data Cleaning

### Some data cleaning was performed in Excel. To be specific, I used Excel's filters to check the values
### and found no missing data or duplicate entries. Additionally, I changed country names for four entries
### "United Team of Germany" to simply "Germany" and "Olympic Athletes from Russia" to "Russia"

# Remove column Host_city
winter <- winter[-3]

# Remove column Country_code
winter <- winter[-4]

# Change column names to lowercase
names(winter) <- tolower(names(winter))

# Adding new column total_medals that sums up gold, silver, and bronze
winter$total_medals <- winter$gold + winter$silver + winter$bronze

summary(winter)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.2. Summary Statistics

# Creating a subset of year and host_country
reshapehost <- subset(winter, select = year:host_country)

# Removing the duplicates
reshapehost <- reshapehost[!duplicated(reshapehost), ]

# Question: How many times has each country hosted the games?
summary(reshapehost)

# Creating a table of host countries
hostcountrytab <- table(reshapehost$host_country)
# Adding labels to entries
hostcountrylab <- paste("Host Country: ", names(hostcountrytab))
# Connecting country names and the count of game hosting
hostcountrylab <- paste(names(hostcountrytab), "\n", hostcountrytab)
# Generating pie chart for game hosting per country
pie(hostcountrytab, label = hostcountrylab, main = "Count of Host Countries",
	col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
	"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"))


# Question: How many times did each country participate over the years?
# Creating a table of countries' participation in the games over time
countrytab <- table(winter$country_name)
# Order countries in decreasing order
countrytab <- countrytab[order(countrytab, decreasing = TRUE)]
# Generating a barplot to visualize count of participation per country
	barplot(countrytab, ylab="Count of Participation", main="Number of Participation 
	Over the Years per Country", ylim= c(0,25),
	col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
	"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"), las = 2)


# Question: How many countries were participating in the games over the years?
# Creating a table of how many countries participated each year over time
countrytab <- table(winter$year)
# Generating a barplot to visualize overall participation over the years per country
	barplot(countrytab, ylab="Count", main="Overall Participation Over the Years", ylim= c(0,30),
	col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
	"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"))


# Question: How many times did each country participate starting from 1994?
# Creating a subset of observations starting from 1994, thus focusing on only specific row numbers
modernparticip <- winter[232:409, ]
# Creating a table of countries' participation in the games starting from 1994
moderntab <- table(modernparticip$country_name)
# Order countries in decreasing order
moderntab <- moderntab[order(moderntab, decreasing = TRUE)]
# Generating a barplot to visualize count of participation per country starting from 1994
	barplot(moderntab, ylab="Count of Participation", main="Number of Participation per Country Starting from 1994",
	ylim= c(0,10), col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
	"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"), las = 2)


# Overall distribution of medals over the years
# Creating a subset of total number of all medals for all countries per year
total_sum_by_year <- aggregate(cbind(total_medals)~year,data=winter,FUN=sum)
# Generating a barplot to visualize year distribution of medals
	barplot(total_sum_by_year$total_medals, xlab="Years", ylab = "Medal Count", main="Year Distribution of Medals",
	ylim= c(0,310), names.arg=total_sum_by_year$year, col="tomato1")


# Generating boxplots of median total_medals for countries that participated the most
boxplot(winter$total_medals ~ winter$year, main = "Total Medals Won Over the Years",
	xlab = "Years", ylab = "Medal Count",
	col = c(rgb(1,0,0,0.25), rgb(0,1,0,0.25), rgb(0,0,1,0.25), rgb(1,1,0,0.25)))

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 2.3. Exploratory Plots

# Creating a table for total medals sum over the years by country
total_sum_by_country = aggregate(cbind(total_medals,gold,silver,bronze)~country_name,data=winter,FUN=sum)
# Order by the number of total medals won in decreasing order
ordered_total <- total_sum_by_country %>% arrange(desc(total_medals))


# Generating a barplot for number of medals won over the years by ALL countries
barplot(ordered_total$total_medals, ylab = "Count",names.arg=ordered_total$country_name, 
		main="Number of Medals Won Over\n the Years per Country", space=0.5,
		ylim= c(0,400), col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
		"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"), las = 2, cex.names=0.7)


# Storing only the top 20 countries of ordered_total
total_top <- head(ordered_total , 20)
# Generating a barplot for number of medals won over the years by top 20 countries (all years)
barplot(total_top$total_medals, ylab = "Count",names.arg=total_top$country_name, 
		main="Number of Medals Won Over\n the Years per Country (Top 20)", space=0.1,
		ylim= c(0,400), col=c("mediumpurple1","yellowgreen","burlywood2","cadetblue1", "coral1", "cyan3",
		"darkolivegreen2", "darkorange1", "dodgerblue", "gold1", "hotpink1", "lightsalmon"), las = 2, cex.names = 0.7, font = 2)


# Creating a variable for gold, silver, bronze, and total medals sum in recent years by country
total_sum_modern = aggregate(cbind(total_medals,gold,silver,bronze)~country_name,data=modernparticip,FUN=sum)
# Ordering the countries based on the number of medals in descending order
modern_ordered_total <- total_sum_modern %>% arrange(desc(total_medals))
# Selecting only top 20 over recent years
modern_total_top <- head(modern_ordered_total , 20)
# Generating number of medals won over the years by top 20 countries (1994-2018)
barplot(modern_total_top$total_medals, ylab = "Count",names.arg=modern_total_top$country_name, 
		main="Number of Medals Won per Country (Top 20, 1994-2018)", space=0.1,
		ylim= c(0,200), col=c("burlywood2","mediumpurple1","yellowgreen","coral1", "cyan3", "cadetblue1",
		"hotpink1", "darkorange1", "dodgerblue", "gold1", "darkolivegreen2", "lightsalmon"), las = 2, cex.names = 0.7, font = 2)


# Selecting only top 5 medal distribution by country over all years
top_five <- head(ordered_total, 5)
# Generating a barplot for top 5 number of different medals won over the recent years
barplot(t(top_five[c('gold','silver','bronze')]), names.arg=top_five$country_name,
		ylim= c(0,150), ylab = "Count", main="Medal Distribution for Top 5 Counties",
		beside=T, col=c("gold1", "azure3","darkgoldenrod4"))
# Adding a legend
legend("topright", c("Gold", "Silver", "Bronze"), col=c("gold1", "azure3","darkgoldenrod4"),lwd=20)


# Creating a variable for gold, silver, bronze, and total medals sum over the years by country
# total_sum_by_country = aggregate(cbind(total_medals,gold,silver,bronze)~country_name,data=winter,FUN=sum)
# Generating an csv file of all total medals per country
# write.csv(total_sum_by_country,file = "winter_olympics_total_country_medals.csv", row.names = T)
# Commented out because the genearted file is already included in assignment submission

# Creating a variable for sums of gold, silver, and bronze by year
total_sum_by_year = aggregate(cbind(gold,silver,bronze)~year,data=winter,FUN=sum) 
# Generating a barplot for gold, silver, and bronze medal distribution by year
barplot(t(total_sum_by_year[c('gold','silver','bronze')]), names.arg=total_sum_by_year$year,
		ylim= c(0,100), ylab = "Count", main="Total Medal Distribution by Year",
		beside=T, col=c("gold1", "azure3","darkgoldenrod4"))
# Adding a legend
legend("topleft", c("Gold", "Silver", "Bronze"), col=c("gold1", "azure3","darkgoldenrod4"),lwd=20)



# Medal Distribution over the years for top 3 countries overall

# Focusing on the first country, Norway
# Taking out gold, silver, and bronze counts by year specifically for Norway
norway_stats <- winter[which(winter$country_name == "Norway"), c("year","gold","silver","bronze")]
# Generating a barplot for gold, silver, and bronze medal distribution for Norway by year
barplot(t(norway_stats[c('gold','silver','bronze')]), names.arg=norway_stats$year,
		ylim= c(0,15), ylab = "Medal Count", main="Medal Distribution for Norway by Year",
		beside=T, col=c("gold1", "azure3","darkgoldenrod4"))
# Adding a legend
legend("topleft", c("Gold", "Silver", "Bronze"), col=c("gold1", "azure3","darkgoldenrod4"),lwd=20)


# Taking out gold, silver, and bronze counts by year specifically for the United States
us_stats <- winter[which(winter$country_name == "United States"), c("year","gold", "silver","bronze")]
# Generating a barplot for gold, silver, and bronze medal distribution for the United States by year
barplot(t(us_stats[c('gold','silver','bronze')]), names.arg=us_stats$year,
		ylim= c(0,15), ylab = "Medal Count", main="Medal Distribution for United States by Year",
		beside=T, col=c("gold1", "azure3","darkgoldenrod4"))
# Adding a legend
legend("topleft", c("Gold", "Silver", "Bronze"), col=c("gold1", "azure3","darkgoldenrod4"),lwd=20)


# Taking out gold, silver, and bronze counts by year specifically for Germany
germany_stats <- winter[which(winter$country_name == "Germany"), c("year","gold", "silver","bronze")]
# Generating a barplot for gold, silver, and bronze medal distribution for Germany by year
barplot(t(germany_stats[c('gold','silver','bronze')]), names.arg=germany_stats$year,
		ylim= c(0,15), ylab = "Medal Count", main="Medal Distribution for Germany by Year",
		beside=T, col=c("gold1", "azure3","darkgoldenrod4"))
# Adding a legend
legend("topleft", c("Gold", "Silver", "Bronze"), col=c("gold1", "azure3","darkgoldenrod4"),lwd=20)


##======================================================================================================

## 3. Paralympic Data

# read medal dataset
para = read_excel("clean_para.xlsx")

# number of countries each year.
num_country = read.csv("medal_athlete_Copy.csv", header = TRUE)

#read the host country data set
host_df = read.csv("host_country.csv", header = TRUE)

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 3.1.  Data Cleaning

#remove duplicate rows
num_country = unique(num_country)

#find the count of country each year
country_counts = as.data.frame(table(num_country$games_year))

#rename the header
names(country_counts)[names(country_counts) == "Var1"]  =  "Year"

#rename the header
names(country_counts)[names(country_counts) == "Freq"]  =  "Number_participation"

#remove duplicate rows
host_df = unique(host_df)

#count how many time hosted country host the events
host_counts = table(host_df$games_country)

# Create a data frame with count and country name
host_counts = data.frame(country = names(host_counts), 
                         count = as.numeric(host_counts))

#rename the header
names(host_counts)[names(host_counts) == "Var1"] = "country"

#rename the header
names(host_counts)[names(host_counts) == "Freq"] = "count"

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 3.2. Summary Statistics

#getting the host count in descending order
host_counts = host_counts[order(-host_counts$count),]

#adjust the margin of the graph due to length of the name of the country
par(mar = c(10, 5, 4, 2) + 0.1)

#graph the barplot 
barplot(host_counts$count, names.arg = host_counts$country, col = c("lightblue"),
        main = "Count of Host countries", ylab = "Count", las = 2,  cex.names = 0.8)

#From the barplot, we can see that a few country hosted the events more than two times.
#Although, it's a bit misleading because the US hosted 3 times and the UK 2 times. 
#We want to keep their name together to display the cooperation between two country 
#when hosting the events together.
#the chart shows the diversity of country that wanted to host the paralympics events
#even the hermit country such as North Korea.

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 3.3. Exploratory Plots

# Barplot for country count for each year
barplot(country_counts$Number_participation, names.arg = country_counts$Year, 
        col =  c("LightBlue"), main = "Number of country participated starting from 1960",
        ylab = "Count", las = 2, xlab = "Year")

#after 1992, the paralympic was divided into winter and summer events. 
#Therefore, in 1994, the dip in participants because there are less country participated in winter events
#However, we still see a increasing trend in the summer events.

# Barplot of distribution of medals
#find the total of gold, silver, bronze medal each year
medal_counts = aggregate(cbind(npc_gold, npc_silver, npc_bronze) ~ games_year, para, sum)

#convert the data frame to correct format to graph
melt_medal = melt(medal_counts, id.vars = "games_year", 
                  variable.name = "Medal_Type", 
                  value.name = "Count")

#factor the medal type to correct label
melt_medal$Medal_Type = factor(melt_medal$Medal_Type,
                               levels = c("npc_gold", "npc_silver", "npc_bronze"),
                               labels = c("Gold", "Silver", "Bronze"))

# Create a bar plot using ggplot
ggplot(melt_medal, aes(x = factor(games_year), y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gold", "grey", "brown")) +
  labs(x = "Year", y = "Number of Medals", fill = "Medal Type",
       title = "Number of medal won each year", width = 1.5) +
  theme_minimal()

#From the barplot, we can see the the incremental of medal 
#after each year. We can see the dip in medal after the split to winter and summer events.

# Creating a graph showing the 20 countries with the most medals
#count the total medal of each country
total_medals = tapply(para$npc_total, para$npc_name, sum)

#match the total medal with each country
country_medals = data.frame(npc_name = names(total_medals), 
                            total_medals = as.numeric(total_medals))

#sort the total medal from high to low and select the top 20
top_20 = country_medals[order(country_medals$total_medals, decreasing = TRUE)[1:20], ]

#graph the top 20
barplot(top_20$total_medals, names.arg = top_20$npc_name, col = "lightblue",
        main = "Top 20 Countries by Total Medals",
        xlab = "Country", ylab = "Total Medals",
        cex.names = 0.7, las = 2, ylim = c(0, 2500))

#by sorting the medal in descending order
#we can see a few country that did really well in sport and complete overwhelming 
#the competition

# Barplot showing time series of medals won by the USA
#filter out the United State data
us_data = subset(para, npc_name == "United States")

#find the total medal of each year from the US
usa_medal = aggregate(cbind(npc_gold, npc_silver, npc_bronze) ~ games_year, 
                      data = us_data, sum)

#convert to data frame with 3 level of medal
usa_melt = melt(usa_medal, id.vars = "games_year", 
                variable.name = "Medal_Type", 
                value.name = "Count")

#rename the medal type to make the legend nicer
usa_melt$Medal_Type = factor(usa_melt$Medal_Type,
                             levels = c("npc_gold", "npc_silver", "npc_bronze"),
                             labels = c("Gold", "Silver", "Bronze"))

# Create a bar plot using ggplot
ggplot(usa_melt, aes(x = factor(games_year), y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gold", "grey", "brown")) +
  labs(x = "Year", y = "Number of Medals", fill = "Medal Type",
       title = "USA medals winning by year") +
  theme_minimal()

#from the graph, we can see that the US did really well before 1994 by winning gold 
#more than silver or bronze. However, starting from 1996, the competition is getting tougher
#the US started to win bronze and silver more than gold.

# Barplot showing time series of medals won by Germany
#same method as the US
ger_data = subset(para, npc_name == "Germany")

ger_medal = aggregate(cbind(npc_gold, npc_silver, npc_bronze) ~ games_year, 
                      data = ger_data, sum)


ger_melt = melt(ger_medal, id.vars = "games_year", 
                variable.name = "Medal_Type", 
                value.name = "Count")

ger_melt$Medal_Type = factor(ger_melt$Medal_Type,
                             levels = c("npc_gold", "npc_silver", "npc_bronze"),
                             labels = c("Gold", "Silver", "Bronze"))

# Create a bar plot using ggplot
ggplot(ger_melt, aes(x = factor(games_year), y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gold", "grey", "brown")) +
  labs(x = "Year", y = "Number of Medals", fill = "Medal Type",
       title = "Germany medals winning by year") +
  theme_minimal()

#same as the US, before 1994, the Germany also got more gold than silver or bronze.
#However, after 1994, Germany's silver medal is higher than gold or bronze each year
#which mean that Germany placed second alot.

# Barplot showing time series of medals won by the UK
#same method as the US
uk_data = subset(para, npc_name == "United Kingdom")

uk_medal = aggregate(cbind(npc_gold, npc_silver, npc_bronze) ~ games_year, 
                     data = uk_data, sum)


uk_melt = melt(uk_medal, id.vars = "games_year", 
               variable.name = "Medal_Type", 
               value.name = "Count")

uk_melt$Medal_Type = factor(uk_melt$Medal_Type,
                            levels = c("npc_gold", "npc_silver", "npc_bronze"),
                            labels = c("Gold", "Silver", "Bronze"))

# Create a bar plot using ggplot
ggplot(uk_melt, aes(x = factor(games_year), y = Count, fill = Medal_Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("gold", "grey", "brown")) +
  labs(x = "Year", y = "Number of Medals", fill = "Medal Type",
       title = "United Kingdom medals winning by year") +
  theme_minimal()

#from the graph, we can see that the UK don't participate in the winter events except in 2014.
#However, compare the Germany, the UK gold, silver and bronze medal amount are fairly even across the year.
#In 2016, the UK did really well and got lots of gold.

##======================================================================================================

## 4. Merged Data

## 4.1.  Data Cleaning

# read files into R
summertotal <- read.csv("summer olympics total country medals.csv", header=T)
paratotal <- read.csv("paralympics total country medals.csv", header=T)
wintertotal <- read.csv("winter_olympics_total_country_medals.csv", header=T)

head(summertotal)
head(paratotal)
head(wintertotal)

#merge summer and paralymics
merged_olympics <- full_join(summertotal, paratotal, by = c("country"), suffix = c(".summer_olympics", ".paralympics"))

# Display the merged data
head(merged_olympics)

#merge first merge and winter olympics

merged_olympics2 <- full_join(merged_olympics, wintertotal, by = c("country"), suffix = c("", ""))

# Display the merged data
head(merged_olympics2)

# Write the data frame to a CSV file to explore data
write.csv(merged_olympics2, "merged_olympics.csv", row.names = FALSE)

#Bring data back in after cleaning for all in top 10
mergedolympicsclean <- read.csv("merged_olympics_clean.csv", header=T)

head(mergedolympicsclean)
summary(mergedolympicsclean)

#converting character data to factors
mergedolympicsclean$country <- as.factor(mergedolympicsclean$country)
mergedolympicsclean$games.summer_olympics <- as.factor(mergedolympicsclean$games.summer_olympics)
mergedolympicsclean$games.paralympics  <- as.factor(mergedolympicsclean$ games.paralympics)

# Melt the data frame
mergedolympicsclean_long <- melt(mergedolympicsclean, id.vars = c("country"), 
                                 measure.vars = c("total_medals.summer_olympics", "total_medals.paralympics",
                                                  "total_medals.winter_olympics"), 
                                 variable.name = "event", value.name = "total_medals")

##+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

## 4.2. Exploratory Plots

# Data manipulation to create one plot of the total medals per top 10 countries in each Olympic Games
# Create separate data frames for Summer Olympic, Paralympics and Winter Olympics

# Subsetting to create summer Olympics df

summer_olympics_df <- mergedolympicsclean_long %>%
  filter(event == "total_medals.summer_olympics")

# Select the top 10 countries

summer_olympics_df <- summer_olympics_df %>%
  arrange(desc(total_medals))

summer_olympics_df <- head(summer_olympics_df, 10)

# Subsetting to create paralympics df

paralympics_df <- mergedolympicsclean_long %>%
  filter(event == "total_medals.paralympics")

# Select the top 10 countries

paralympics_df <- paralympics_df %>%
  arrange(desc(total_medals))

paralympics_df <- head(paralympics_df, 10)

# Subsetting to create the winter olympics df

winter_olympics_df <- mergedolympicsclean_long %>%
  filter(event == "total_medals.winter_olympics")

# Select the top 10 countries

winter_olympics_df <- winter_olympics_df %>%
  arrange(desc(total_medals))

winter_olympics_df <- head(winter_olympics_df, 10)

# Merging the three data frames to create one plt of all top 10 countries
merged_data <- bind_rows(top_countries_summer_df, paralympics_df, winter_olympics_df )

#ordering for the graph by most to least medal winners

ordered_countries <- merged_data$country[order(merged_data$total_medals, decreasing = TRUE)]

# Changing country to a factor variable
merged_data$country <- factor(merged_data$country, levels = unique(ordered_countries))

# Create a stacked bar plot of top 10 medal winners

ggplot(merged_data, aes(x = country, y = total_medals, fill = event)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  labs(title = "Total Medals by Country",
       x = "Country", y = "Number of Medals",
       fill = "Event") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_manual(values = c("total_medals.summer_olympics" = "darkgreen",
                               "total_medals.paralympics" = "orange",
                               "total_medals.winter_olympics" = "blue"),
                    labels = c("Summer Olympics", "Paralympics", "Winter Olympics"))

# We can see that USA, Germany and Sweden all fall in the top 10 in each type of games. 
#And we can see that USA out peforms all other teams by more than 1000 medals, 
#making them the most successful team at Olympic sports.

#Its interesting to see that the some of  top winter Olympics teams do not achieve 
#in the summer and paralympics games where we see more of an overlap with summer and paralympics top achievers.


##======================================================================================================
