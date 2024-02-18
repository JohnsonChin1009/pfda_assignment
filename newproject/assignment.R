# Group
# 1. Chin Hong Wei TP065390
# 2. Loo Hui En TP065181
# 3. Lim Ee Chian TP065138
# 4. Yong Jie Yee TP078458

# Objectives

# Step 1: Importing the csv file as dataset
# -----------------------------------------------------
dataSet = read.csv("kl_property_data.csv")

# Step 2: Data Exploration
# -----------------------------------------------------
# Exploring the data to further understand how the data is structured
# and what are the data types present in the data set

# Initiating dependencies
library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)

## Exploring variable types in the data set
  str(dataSet)

## Identify the number of rows and cols in the data set
  nrow(dataSet)
  ncol(dataSet)

## Identifying the names of the columns
  names(dataSet)

## Counting the number of missing values in the data set
  sum(is.na(dataSet))

## Checking the structure of the data set
  summary(dataSet)

# Checking whether they are empty values within the $Price column
  sum(is.na(dataSet$Price))
  
# Step 3: Data Cleaning & Pre-processing
# -----------------------------------------------------
# Pre-processing the data to ensure that the data is suitable for
# data analysis

## Formatting the $Price column into numeric data type
    dataSet$Price <- gsub("RM", "", dataSet$Price)  # Removing the RM heading
    dataSet$Price <- gsub("\\s", "", dataSet$Price) # Removing any white-spaces
  
  # Removing commas between the values and turning it into integer variable type
    dataSet$Price <- as.numeric(gsub(",", "", dataSet$Price)) 
  
  # Filter out rows where $Price is below 100,000
    dataSet <- dataSet[dataSet$Price >= 100000,]

## Formatting the $Rooms column into integer data type
    
  initialCount <- sum(is.na(dataSet$Rooms) | dataSet$Rooms == "")
  
  dataSet <- dataSet %>%
    mutate(
      # Convert "Studio" to 1
      Rooms = ifelse(Rooms == "Studio", 1, Rooms),
      
      # Convert Empty Strings to NA
      Rooms = ifelse(Rooms == "", NA, Rooms),
      
      # Convert NA values to 0
      Rooms = ifelse(is.na(Rooms), 0, Rooms),
      
      # Remove "Above" from values like "20 Above"
      Rooms = ifelse(grepl("\\d+ Above", Rooms), as.numeric(str_extract(Rooms, "\\d+")), Rooms),
    )
  
  # Identify values that match the X+Y pattern
  x_plus_y_values <- dataSet$Rooms[grepl("\\d+\\+\\d+", dataSet$Rooms)]
  
  # Compute X+Y and replace the original values
  dataSet$Rooms[grepl("\\d+\\+\\d+", dataSet$Rooms)] <- 
    sapply(strsplit(x_plus_y_values, "\\+"), function(x) sum(as.numeric(x)))
  
  # Identify values that match the X+ pattern
  x_plus_values <- dataSet$Rooms[grepl("\\d+\\+", dataSet$Rooms)]
  
  # Compute X+ and replace the original values
  dataSet$Rooms[grepl("\\d+\\+", dataSet$Rooms)] <-
    sapply(strsplit(x_plus_values, "\\+"), function(x) as.numeric(x[1]) + 1)
  
  # Change the $Rooms column to numeric
  dataSet$Rooms <- as.numeric(dataSet$Rooms)
  
## Standardize the $Furnishing column ensuring no N/A values
  # Extract furnishing information using regular expression
    dataSet$Furnishing <- str_extract(dataSet$Furnishing, "(?i)Fully Furnished|Unfurnished|Partly Furnished")
  
  # If the furnishing value is empty, replace with "Unknown"
    dataSet$Furnishing[is.na(dataSet$Furnishing)] <- "Unknown"

## $Rooms, $Bathrooms, $Carparks Column

#Replacing empty strings and NA with 0 in the $Bathrooms column
  dataSet$Bathrooms[dataSet$Bathrooms == ""] <- "0"
  dataSet$Bathrooms[is.na(dataSet$Bathrooms)] <- "0"
  
#Replacing empty strings and NA with 0 in the $Carparks column
  dataSet$Car.Parks[dataSet$Car.Parks == ""] <- "0"
  dataSet$Car.Parks[is.na(dataSet$Car.Parks)] <- "0"

# Check for any rows that only have values in the $Location column
  location_only_rows <- dataSet[rowSums(dataSet != "" & !is.na(dataSet)) == 1, ]
  
# Remove the rows where $Price value is empty, "NA" or 0
  dataSet <- dataSet[!(dataSet$Price == "" | is.na(dataSet$Price) | dataSet$Price == 0), ]   
  
# Remove the rows that only have values in the $Location column
  dataSet <- dataSet[!(rownames(dataSet) %in% rownames(location_only_rows)), ]
  
# Remove the rows where $Size value is empty, "NA"
  dataSet <- dataSet[!(dataSet$Size == "" | is.na(dataSet$Size)), ]
  
# Separate the data in the `Size` column in the original dataset into 2 columns, which are `Area Type` and `Size`.
# `Size` column is character type
  install.packages("tidyr")
  library(tidyr)
  
  
# *** Separate `Size` column into `Area Type` and `Size`, Use the `separate` function from the `tidyr` package in R
  dataSet <- separate(dataSet, Size, into = c("Area.Type", "Size"), sep = ": ")
  dataSet
  nrow(dataSet)
  
  
  
# Clean the `Size` column, by removing "sq. ft." and commas, and handle multiplication sign "x" (if present).
# *** Remove "sq. ft."
  dataSet$Size <- gsub(" sq\\. ft\\.", "", dataSet$Size)
  dataSet
  nrow(dataSet)
# *** Remove commas
  dataSet$Size <- gsub(",", "", dataSet$Size)
  dataSet
  nrow(dataSet)
# *** Handle multiplication sign "x" (if present)
# Split Size into two columns based on "x" and convert to numeric
  dataSet$Size <- ifelse(grepl("x", dataSet$Size),
                         sapply(strsplit(dataSet$Size, "x"), function(x) as.numeric(x[1]) * as.numeric(x[2])),
                         as.numeric(dataSet$Size))
  dataSet
  nrow(dataSet)
  
  
  
# Separate the data in the `Property Type` column into 2 columns, which are `Property Type` and `Property Style`.
# *** Separate Property.Type column into PropertyType and PropertyStyle
  dataSet <- separate(dataSet, Property.Type, into = c("Property.Type", "Property.Style"), sep = "\\(")
# *** Remove the trailing bracket in Property.Style column
  dataSet$Property.Style <- gsub("\\)", "", dataSet$Property.Style)
  dataSet
  nrow(dataSet)
  
# Step 17: Replace empty values/strings in ` Property Type` column with NA, using `na_if` function.
# *** Replace empty strings with <NA> in the `Property.Style` column
  dataSet <- dataSet %>%
    mutate(Property.Style = na_if(Property.Style, ""))
  dataSet
  nrow(dataSet)
  
  
# Step 4: Data Analysis
# -----------------------------------------------------
# Performing data analysis on the processed and cleaned data to discover
# answers for objectives and to justify the hypothesis

# Loo Hui En TP065181 (Objective 1)
# -----------------------------------------------------
# Add your code here
# -----------------------------------------------------
  
# Yong Jie Yee TP078458 (Objective 2)
# -----------------------------------------------------
# Add your code here
 # -----------------------------------------------------
  
# Chin Hong Wei TP065390 (Objective 3)
# -----------------------------------------------------
# Question 1: Does furnishing status impact the average property price in Kuala Lumpur?
  
  # Analysis 1.1: What are the average property prices for different property types?
  averagePrices <- dataSet %>% group_by(Furnishing) %>% summarize(Average_Price = mean(Price, na.rm=TRUE))
  
  ggplot(averagePrices, aes(x = Furnishing, y = Average_Price)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Average Property Prices by Furnishing Status", x = "Furnishing Status", y = "Average Price (RM)")
  
  # Violin for Distribution
  ggplot(dataSet, aes(x = Furnishing, y = Price)) +
    geom_violin(fill = "skyblue") +
    labs(title = "Property Prices Distribution by Furnishing Status", x = "Furnishing Status", y = "Price")
  
  median_prices <- dataSet %>%
    group_by(Furnishing) %>%
    summarize(Median_Price = median(Price, na.rm = TRUE))
  
  ggplot(median_prices, aes(x = Furnishing, y = Median_Price)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(title = "Median Property Prices by Furnishing Status", x = "Furnishing Status", y = "Median Price")
  
  summary_by_furnishing <- dataSet %>%
    group_by(Furnishing) %>%
    summarize(mean_price = mean(Price, na.rm = TRUE))
  
  ggplot(summary_by_furnishing, aes(x = Furnishing, y = mean_price, fill = Furnishing)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Mean Price by Furnishing Status",
         x = "Furnishing Status",
         y = "Mean Price") +
    theme_minimal()
  
  
# -----------------------------------------------------
  
# Lim Ee Chian TP065138 (Objective 4)
# -----------------------------------------------------
# Add your code here
# -----------------------------------------------------


# Step 12:	Replace the empty values/strings in `Size` column with NA, using `na_if` function from the “dplyr” package.
library(dplyr)
# *** Replace empty strings with <NA> in the `Size` column
dataSet <- dataSet %>%
  mutate(Size = na_if(Size, ""))
dataSet
nrow(dataSet)



# Step 15: Replace empty values/strings in `Area Type` column with NA, using `na_if` function.
library(dplyr)
dataSet <- dataSet %>%
  mutate(Area.Type = na_if(Area.Type, ""))
dataSet
nrow(dataSet)









#Extra Notes
# Cannot go below 51189
