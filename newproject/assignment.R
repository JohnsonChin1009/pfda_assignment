# Group
# 1. Chin Hong Wei TP065390
# 2. Loo Hui En TP065181
# 3. Lim Ee Chian TP065138
# 4. Yong Jie Yee TP078458

# Objectives
# Step 1: Importing the csv file as dataset

# Cannot go below 51189
# Ensuring that you extract the 
dataSet = read.csv("kl_property_data.csv")

#Step 2: Cleaning the data
#Removing duplicated rows

#


##Converting the $Price column into int variable type

  #Removing the RM heading
  dataSet$Price <- gsub("RM", "", dataSet$Price)
  
  #Removing any white-spaces
  dataSet$Price <- gsub("\\s", "", dataSet$Price)
  
  #Removing commas between the values and turning it into numerical variable type
  dataSet$Price <- as.numeric(gsub(",", "", dataSet$Price))
  
  #Filter out rows where $Price is below 100,000
  dataSet <- dataSet[dataSet$Price >= 100000,]
  
#---------------------------------------------- Ee Chian's Add on code ---------------------------------------------------

# Step 1: Import the csv file which is the original dataset. 
install.packages("tidyverse")   # The downloaded binary packages are in "C:\Users\Lim Ee Chian\AppData\Local\Temp\RtmpYDAMW1\downloaded_packages"
library(tidyverse)
# *** Ensuring that you extract the dataSet = read.csv("5. kl_property_data.csv")
dataSet <- read.csv("C:\\Users\\Lim Ee Chian\\Documents\\Ee Chian APU Degree SE\\SEM 1\\Programming For Data Analysis\\PFDA ass\\5. kl_property_data.csv")


# Step 2: Identify how many rows there are in the original dataset. Rows: 53883
dataSet
nrow(dataSet)


# Step 3:	Check the structure of the dataset, using `summary(dataSet)`.
summary(dataSet)


# Step 4:	Find rows where only the “Location” column has data while all other columns are empty & Remove the identified rows from the original dataset.
location_only_rows <- dataSet[rowSums(dataSet != "" & !is.na(dataSet)) == 1, ]
# Print or view the rows
print(location_only_rows)
nrow(location_only_rows)
# [1] 25
dataSet <- dataSet[!(rownames(dataSet) %in% rownames(location_only_rows)), ]


# Step 5:	Identify how many rows there are after the 1st row cleaning. Rows: 53858
nrow(dataSet)
# [1] 53858   [First Row Cleaning]


# Step 6: Identify how many duplicated rows. Row: 25 & Remove duplicated rows in the dataset.
# ?? duplicated(dataSet)
dataSet = unique(dataSet)
nrow(dataSet)
# [1] 49398   [Second Row Cleaning]

# OR

# *** Remove duplicate rows
dataSet <- dataSet[!duplicated(dataSet), ]   


# Step 7:	Identify how many rows there are after 2nd row cleaning. Rows: 49398
nrow(dataSet)
# [1] 49398   [Second Row Cleaning]


# Step 8:	Replace missing values in `Car Parks` column with integer 0.
dataSet$Car.Parks[is.na(dataSet$Car.Parks)] <- 0
dataSet


# Step 9:	Replace missing values in `Furnishing` column with “Unknown”.
install.packages("stringr")   # The downloaded binary packages are in "C:\Users\Lim Ee Chian\AppData\Local\Temp\RtmpARXFjH\downloaded_packages"
library(stringr)
# *** Extract Furnishing information using regular expression
dataSet$Furnishing <- str_extract(dataSet$Furnishing, "(?i)Fully Furnished|Unfurnished|Partly Furnished")
# *** If there is no Furnishing information, set it to Unknown
dataSet$Furnishing[is.na(dataSet$Furnishing)] <- "Unknown"
dataSet
nrow(dataSet)


# Step 10: Replace “Studio” in `Rooms` column with integer 1.
# *** Replace the occurrences of "Studio" in the `Rooms` column with the integer 1, use the mutate function from the dplyr package. 
library(dplyr)
# Change "Studio" to 1 in the Rooms column, leave other NAs unaffected
dataSet <- dataSet %>%
  mutate(Rooms = replace(Rooms, Rooms == "Studio", 1))
dataSet


# Step 11: Replace empty values/strings in `Rooms` column with NA, using `na_if` function from the “dplyr” package.
library(dplyr)
# *** Replace empty strings with <NA> in the `Rooms` column
dataSet <- dataSet %>%
  mutate(Rooms = na_if(Rooms, ""))
dataSet
nrow(dataSet)

# Step 12:	Replace the empty values/strings in `Size` column with NA, using `na_if` function from the “dplyr” package.
library(dplyr)
# *** Replace empty strings with <NA> in the `Size` column
dataSet <- dataSet %>%
  mutate(Size = na_if(Size, ""))
dataSet
nrow(dataSet)


# Step 13: Replace empty values/strings in `Price` column with NA, using `na_if` function.
library(dplyr)
# *** Replace empty strings with <NA> in the `Price` column
dataSet <- dataSet %>%
  mutate(Price = na_if(Price, ""))
dataSet
nrow(dataSet)


# Step 14: Separate the data in the `Size` column in the original dataset into 2 columns, which are `Area Type` and `Size`.
# `Size` column is character type
install.packages("tidyr")
library(tidyr)
# *** Separate `Size` column into `Area Type` and `Size`, Use the `separate` function from the `tidyr` package in R
dataSet <- separate(dataSet, Size, into = c("Area.Type", "Size"), sep = ": ")
dataSet
nrow(dataSet)


# Step 15: Replace empty values/strings in `Area Type` column with NA, using `na_if` function.
library(dplyr)
dataSet <- dataSet %>%
  mutate(Area.Type = na_if(Area.Type, ""))
dataSet
nrow(dataSet)


# Step 16: Separate the data in the `Property Type` column into 2 columns, which are `Property Type` and `Property Style`.
# *** Separate Property.Type column into PropertyType and PropertyStyle
dataSet <- separate(dataSet, Property.Type, into = c("Property.Type", "Property.Style"), sep = "\\(")
# *** Remove the trailing bracket in Property.Style column
dataSet$Property.Style <- gsub("\\)", "", dataSet$Property.Style)
dataSet
nrow(dataSet)


# Step 17: Replace empty values/strings in ` Property Type` column with NA, using `na_if` function.
library(dplyr)
# *** Replace empty strings with <NA> in the `Property.Style` column
dataSet <- dataSet %>%
  mutate(Property.Style = na_if(Property.Style, ""))
dataSet
nrow(dataSet)


# Step 18: Convert the `Price` column into integer variable datatype.
# *** Converting the $Price column into int variable type
dataSet$Price <- gsub("RM", "", dataSet$Price)   #Removing the RM heading
dataSet$Price <- gsub("\\s", "", dataSet$Price)   #Removing any white-spaces
dataSet$Price <- as.numeric(gsub(",", "", dataSet$Price))   #Removing commas between the values and turning it into numerical variable type
dataSet

# OR

# *** Convert `Price` column to numeric (remove currency symbol and commas)
dataSet$Price <- as.numeric(gsub("[^0-9.]", "", dataSet$Price))
dataSet
nrow(dataSet)


# Step 19: Clean the `Size` column, by removing "sq. ft." and commas, and handle multiplication sign "x" (if present).
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


# Step 20: Check the structure of the current dataset again, using `summary(dataSet)`.
summary(dataSet)


