library(dplyr)
library(tidyverse)

# Using file.path where setwd is for setting the directory,
# whereas getwd is to return the path of working directory

content <- read.csv(file.path(getwd(), "datasets", "Content.csv"))
reactions <- read.csv(file.path(getwd(), "datasets", "Reactions.csv"))
reactiontypes <- read.csv(file.path(getwd(), "datasets", "ReactionTypes.csv"))

#################################
# Main objective of the analysis:
# Client wants to see “An analysis of their content categories
# showing the top 5 categories with the largest popularity.”
# To figure out popularity, we’ll have to add up which
# content categories have the largest score.
#################################


## Cleaning Content dataset first
# Start by performing data cleaning
summary(content)

# Getting rid of irrelevant columns: row number, user ID, and URL
cleanContent <- subset(content, select = -c(X,User.ID,URL))

# Rename columns for clarity
cleanContent <- cleanContent %>% 
	rename(
		content_id = Content.ID,
		content_type = Type,
		category = Category
	)

# No rows with missing values will remain once we leave
# only content ID and category columns. However, category names
# are formatted differently. Make sure all category names are consistent.

# Changing Category to factors
cleanContent$category <- as.factor(cleanContent$category)
# List all the possible recorded categories
unique(cleanContent$category)

# Rewrite incorrect categories for consistency
cleanContent <- mutate(cleanContent, category = recode(category, "Studying" = "studying",
			"Healthy Eating" = "healthy eating", "Technology" = "technology",
			"\"technology\"" = "technology", "Animals" = "animals", "\"culture\"" = "culture",
			"Fitness" = "fitness", "Veganism" = "veganism", "\"animals\"" = "animals",
			"Travel" = "travel", "\"soccer\"" = "soccer", "Education" = "education",
			"\"dogs\"" = "dogs", "Soccer" = "soccer", "\"tennis\"" = "tennis", "Culture" = "culture",
			"\"food\"" = "food", "Food" = "food", "\"cooking\"" = "cooking", "Science" = "science",
			"\"public speaking\"" = "public speaking", "\"veganism\"" = "veganism",
			"Public Speaking" = "public speaking", "\"science\"" = "science", "\"studying\"" = "studying"))

# Listing the categories again so now there are now only unique categories left
unique(cleanContent$category)

summary(cleanContent)
head(cleanContent)


#################################################


## Cleaning Reactions dataset
summary(reactions)

# Removing irrelevant columns: row number, user ID
cleanReactions <- subset(reactions, select = -c(X,User.ID))

# Rename columns for clarity
cleanReactions <- cleanReactions %>% 
	rename(
		content_id = Content.ID,
		reaction_type = Type,
		datetime = Datetime
	)

# Changing Type to factors
cleanReactions$reaction_type <- as.factor(cleanReactions$reaction_type)
# List all the possible recorded categories
unique(cleanReactions$reaction_type)

# Removing rows with missing values
# Add NAs instead of empty cells
cleanReactions$reaction_type[cleanReactions$reaction_type==""] <- "NA"
# Check the number of rows with NAs
cleanReactions[is.na(cleanReactions$reaction_type),]
# Remove all rows with NAs
cleanReactions <- cleanReactions[complete.cases(cleanReactions), ]

summary(cleanReactions)
head(cleanReactions)


#################################################


## Cleaning ReactionTypes dataset
summary(reactiontypes)

# Removing irrelevant columns: row number
cleanReactionTypes <- subset(reactiontypes, select = -c(X))

# Rename columns for clarity
cleanReactionTypes <- cleanReactionTypes %>% 
	rename(
		reaction_type = Type,
		sentiment = Sentiment,
		score = Score
	)

# No inconsistencies were found in this dataset

summary(cleanReactionTypes)
head(cleanReactionTypes)


#################################################


# Merging three cleaned datasets into one final dataset for analysis
(finalData  <- merge(cleanReactions, cleanContent, by.x = "content_id", all.x = T))
(finalData  <- merge(finalData, cleanReactionTypes, by.x = "reaction_type", all.x = T))

summary(finalData)
head(finalData)


# Generate final clean merged dataset as an Excel file to view
write.csv(finalData, file = "top_categories.csv", row.names = F)


#################################################


finalData <- read.csv("top_categories.csv", header = T)
summary(finalData)
head(finalData)

# Find score sums for each category
categoryScores <- aggregate(score ~ category, data=finalData, sum)
categoryScores <- categoryScores %>% arrange(desc(score))
write.csv(categoryScores, file = "category_scores.csv", row.names = F)

# Selecting only top 5 categories
top_5 <- categoryScores %>% top_n(5)
head(top_5)





