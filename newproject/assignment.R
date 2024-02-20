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
library(tidyr)

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
  sum(dataSet$Price == "")
  
# Step 3: Data Cleaning & Pre-processing
# -----------------------------------------------------
# Pre-processing the data to ensure that the data is suitable for
# data analysis

###
# Formatting the $Price column into numeric data type
    dataSet$Price <- gsub("RM", "", dataSet$Price)  # Removing the RM heading
    dataSet$Price <- gsub("\\s", "", dataSet$Price) # Removing any white-spaces
  
  # Removing commas between the values and turning it into integer variable type
    dataSet$Price <- as.numeric(gsub(",", "", dataSet$Price)) 
  
  # Filter out rows where $Price is below 100,000
    dataSet <- dataSet[dataSet$Price >= 100000,]

  # Remove the rows where $Price value is empty, "NA" or 0
    dataSet <- dataSet[!(dataSet$Price == "" | is.na(dataSet$Price) | dataSet$Price == 0), ] 
###   
  
###    
# Checking for outliers in the $Price column using boxplot
    boxplot(dataSet$Price)
      
  # Identifying rows that are above 30 million & 15 million that is not Residential Land
    rowsAbove30m <- nrow(dataSet[dataSet$Price >= 3.0e+07,])
    rowsAbove15m <- nrow(dataSet[dataSet$Price >= 1.5e+07 & !grepl("^Residential Land", dataSet$Property.Type), ])
      
  # Removing rows that are above 30 million & 15 million that is not Residential Land
    dataSet <- dataSet[dataSet$Price < 3.0e+07, ]
    dataSet <- dataSet[dataSet$Price < 1.5e+07 | grepl("^Residential Land", dataSet$Property.Type), ]
### 

###
# Formatting the $Rooms column into integer data type  
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
###
    
## Pre-processing $Rooms and $Bathrooms columns based on $Property.Type column
  # Identify rows with "Residential Land" or its variants
    residential_land_rows <- which(grepl("Residential Land", dataSet$Property.Type))
  
  # Change Rooms and Bathrooms to NA for the identified rows
    dataSet$Rooms[residential_land_rows] <- NA
    dataSet$Bathrooms[residential_land_rows] <- NA

## Standardize the $Furnishing column ensuring no N/A values
  # Extract furnishing information using regular expression
    dataSet$Furnishing <- str_extract(dataSet$Furnishing, "(?i)Fully Furnished|Unfurnished|Partly Furnished")
  
  # If the Property.Type starts with "Residential Land", replace furnishing value with "Vacant Land"
    dataSet$Furnishing[grep("^Residential Land", dataSet$Property.Type)] <- "Vacant Land"
    
  # If the furnishing value is empty, replace with "Unknown"
    dataSet$Furnishing[is.na(dataSet$Furnishing)] <- "Unknown"

## $Rooms, $Bathrooms, $Carparks Column

#Replacing empty strings and NA with 0 in the $Bathrooms column
  dataSet$Bathrooms[dataSet$Bathrooms == ""] <- "0"
  dataSet$Bathrooms[is.na(dataSet$Bathrooms)] <- "0"
  
#Replacing empty strings and NA with 0 in the $Carparks column
  dataSet$Car.Parks[dataSet$Car.Parks == ""] <- "0"
  dataSet$Car.Parks[is.na(dataSet$Car.Parks)] <- "0"
  
  
  
### Remove and Format Size column  
# Remove the rows where $Size value is empty, "NA"
  dataSet <- dataSet[!(dataSet$Size == "" | is.na(dataSet$Size)), ]
  nrow(dataSet)
  
# Separate the data in the `Size` column in the original dataset into 2 columns, which are `Area Type` and `Size`.
  # `Size` column is character type
  # Separate `Size` column into `Area Type` and `Size`, Use the `separate` function from the `tidyr` package in R
  dataSet <- separate(dataSet, Size, into = c("Area.Type", "Size"), sep = ": ")
  
  
# Clean the `Size` column, by removing "sq. ft." and commas
  # Remove "sq. ft."
  dataSet$Size <- gsub(" sq\\. ft\\.", "", dataSet$Size)
  # Remove commas
  dataSet$Size <- gsub(",", "", dataSet$Size)
  
  
# Identify values that need processing 
  size_values_to_process <- dataSet$Size[grepl("[^0-9]", dataSet$Size)]
  
  # Process values with non-digit characters
  for (value in size_values_to_process) {
    if (grepl("\\+", value)) {
      # Addition
      parts <- strsplit(value, "\\+")[[1]]
      result <- sum(as.numeric(parts))
    } else if (grepl("-", value)) {
      # Subtraction
      parts <- strsplit(value, "-")[[1]]
      result <- as.numeric(parts[1]) - sum(as.numeric(parts[-1]))
    } else if (grepl("x", value)) {
      # Multiplication
      parts <- strsplit(value, "x")[[1]]
      result <- prod(as.numeric(parts))
    } else if (grepl("/", value)) {
      # Division
      parts <- strsplit(value, "/")[[1]]
      result <- as.numeric(parts[1]) / prod(as.numeric(parts[-1]))
    } else {
      # No operation needed
      result <- as.numeric(value)
    }
    
    # Replace the original value with the result
    dataSet$Size[dataSet$Size == value] <- result
  }  
  
# Remove "-"  
  dataSet$Size <- gsub("-", "", dataSet$Size)  # Remove "-"
  
#Replacing empty strings and NA with 0 in the $Size column
  # Replace NA values with 0
  dataSet$Size[is.na(dataSet$Size)] <- 0
  # Replace "" values with 0
  dataSet$Size <- ifelse(dataSet$Size == "", 0, dataSet$Size)
  
# Change the $Size column to integer
  dataSet$Size <- as.integer(dataSet$Size)
  
###  
  


# Separate Property.Type column into Property.Type and Property.Style
dataSet <- dataSet %>%
  mutate(Property.Style = ifelse(grepl("\\(", Property.Type), 
                                 str_extract(Property.Type, "\\(.+\\)"), NA),
         Property.Type = ifelse(!is.na(Property.Style),
                                str_remove_all(Property.Type, "\\(.+\\)"), 
                                Property.Type))

# Remove leading and trailing spaces from Property.Style
dataSet$Property.Style <- trimws(dataSet$Property.Style)

# Remove parentheses from Property.Style
dataSet$Property.Style <- gsub("[()]", "", dataSet$Property.Style)
  
# Replace empty values/strings in ` Property Style` column with NA, using `na_if` function.
  dataSet <- dataSet %>%
    mutate(Property.Style = na_if(Property.Style, ""))


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
# Analysis 1: Analysis of Average Property Prices by Furnishing Status
  # Group dataset by furnishing status and calculate mean price for each group
  averagePricebyFurnishingStatus <- dataSet %>%
    group_by(Furnishing) %>%
    summarize(AveragePricebyFurnishingStatus = mean(Price, na.rm = TRUE))
  
  # Visualize the results using a bar chart
  ggplot(averagePricebyFurnishingStatus, aes(x = Furnishing, y = AveragePricebyFurnishingStatus, fill = Furnishing)) +
    geom_bar(stat = "identity") +
    labs(title = "Average Property Prices by Furnishing Status", x = "Furnishing Status", y = "Average Price (RM)") +
    theme_minimal()
  
# Analysis 2: Analysis of Distribution of Furnishing Status
  furnishingCounts <- dataSet %>% count(Furnishing)
  
  ggplot(dataSet, aes(x = Furnishing, y = Price, fill = Furnishing)) +
    geom_violin() +
    scale_fill_brewer(palette = "Set2") +
    labs(title = "Distribution of Furnishing Status", x = "Furnishing Status", y = "Price") +
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
