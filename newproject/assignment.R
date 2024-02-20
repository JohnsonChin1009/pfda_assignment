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
  # Objective 2: Does properties that have a proportionate number of rooms and bathrooms have a higher price?
  
  # Analysis 2-1: What are the average property prices for numbers of rooms?
  # Convert the Rooms column to numeric
  dataSet$Rooms <- as.numeric(dataSet$Rooms)
  
  # Check if there are any non-numeric values or missing values in the Rooms column
  non_numeric_rooms <- dataSet[!grepl("^\\d+$", as.character(dataSet$Rooms)) | is.na(dataSet$Rooms), ]
  print(non_numeric_rooms)
  
  # Categorize rooms into ranges
  dataSet$Room_Range <- cut(dataSet$Rooms, breaks = c(0, 2, 4, 6, 8, 10, 12, Inf), labels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "12+"))
  
  # Calculate the average property prices for different ranges of rooms
  average_prices_by_room_range <- dataSet %>%
    group_by(Room_Range) %>%
    summarize(Average_Price = mean(Price, na.rm = TRUE))
  
  # Visualize the results using a bar plot with customized aesthetics
  ggplot(average_prices_by_room_range, aes(x = Room_Range, y = Average_Price, fill = Room_Range)) +
    geom_bar(stat = "identity", color = "black") +  # Set bar outline color
    geom_text(aes(label = paste0(round(Average_Price, 2)), y = Average_Price), vjust = -0.5, size = 3.5, color = "black") +
    geom_line(aes(x = Room_Range, y = Average_Price, group = 1), color = "black", size = 1.5) +  # Add a solid line
    labs(title = "Average Property Prices by Range of Rooms",
         x = "Range of Rooms",
         y = "Average Price") +
    scale_y_continuous(labels = scales::comma) +  # Format y-axis labels with comma separator
    theme_minimal() +  # Apply minimal theme for cleaner look
    theme(legend.position = "none",  # Remove legend
          axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
          panel.grid.major = element_line(color = "gray", linetype = "dashed"),  # Add dashed grid lines for major axes
          panel.background = element_blank()  # Remove background lines
    ) +
    scale_fill_brewer(palette = "Set2")  # Set color palette
  
  
  #Analysis 2-2 Which number of rooms is preferred by the market?
  # Categorize rooms into ranges (same ranges as used in the previous bar plot)
  dataSet$Room_Range <- cut(dataSet$Rooms, 
                            breaks = c(0, 2, 4, 6, 8, 10, 12, Inf), 
                            labels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "12+"))
  
  # Count the frequency of each range of rooms
  room_frequency <- dataSet %>%
    group_by(Room_Range) %>%
    summarize(Frequency = n())
  
  # Plot the frequency of each range of rooms using a line graph
  ggplot(room_frequency, aes(x = Room_Range, y = Frequency, color = Room_Range, group = 1)) +
    geom_line() +
    geom_point() +
    labs(
      title = "Frequency of Properties by Number of Rooms",
      x = "Number of Rooms",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
    ) +
    scale_color_brewer(palette = "Set2") +  # Set color palette for lines and points
    geom_text(aes(label = Frequency, y = Frequency), vjust = -0.5, size = 3.5) # Add text labels with Frequency
  
  
  # Analysis 2.3: What are the average property prices for numbers of bathrooms?
  library(scales)  # Load the scales package for formatting
  
 
  
  # Analysis 2.3: What are the average property prices for numbers of bathrooms?
  library(scales)  # Load the scales package for formatting

  # Categorize properties based on the number of bathrooms into ranges
  dataSet$Bathroom_Range <- cut(dataSet$Bathrooms, 
                                breaks = c(-Inf, 1, 3, 5, 8, Inf), 
                                labels = c("0-1", "2-3", "4-5", "6-8", "9+"))
  
  # Calculate the average property price for each range of bathrooms
  avg_price_by_bathroom <- dataSet %>%
    group_by(Bathroom_Range) %>%
    summarize(Average_Price = mean(Price))
  
  # Create the bar chart with a different color for the bars
  bar_chart <- ggplot(avg_price_by_bathroom, aes(x = Bathroom_Range, y = Average_Price)) +
    geom_bar(stat = "identity", fill = "#FFA500") +  # Bar chart with orange fill color
    labs(
      title = "Average Property Prices by Range of Bathrooms",
      x = "Number of Bathrooms",
      y = "Average Price"
    ) +
    theme_minimal() +
    theme(
      legend.position = "none",  # Remove legend
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
      axis.title = element_text(size = 12, face = "bold"),  # Axis title font size and weight
      axis.text = element_text(size = 10),  # Axis text font size
      plot.title = element_text(size = 16, face = "bold")  # Plot title font size and weight
    ) +
    scale_y_continuous(labels = scales::comma)  # Format y-axis labels with commas
  
  # Display the bar chart
  print(bar_chart)
  
  
  # Analysis 2-4: Which number of bathrooms is preferred by the market?
  
  # Convert 'Bathrooms' to numeric
  dataSet$Bathrooms <- as.numeric(as.character(dataSet$Bathrooms))
  
  # Now, you can proceed with the categorization and plotting
  # Categorize bathrooms into ranges
  dataSet$Bathroom_Range <- cut(dataSet$Bathrooms, 
                                breaks = c(0, 1, 4, 7, 10, Inf), 
                                labels = c("1", "2-4", "5-7", "8-9", "10+"))
  
  # Count the frequency of each range of bathrooms
  bathroom_frequency <- dataSet %>%
    group_by(Bathroom_Range) %>%
    summarize(Frequency = n())
  
  # Calculate the percentage for each category
  bathroom_frequency$Percentage <- bathroom_frequency$Frequency / sum(bathroom_frequency$Frequency) * 100
  
  # Plot the frequency of each range of bathrooms using a pie chart
  ggplot(bathroom_frequency, aes(x = "", y = Percentage, fill = Bathroom_Range)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +  # Convert the bar chart into a pie chart
    labs(
      title = "Search Percentage of Properties by Number of Bathrooms",
      fill = "Number of Bathrooms"
    ) +
    theme_void() +  # Remove unnecessary elements from the plot
    geom_text(aes(label = paste0(round(Percentage, 2), "%")), position = position_stack(vjust = 0.5)) +
    scale_fill_brewer(palette = "Set3")  # Set color palette for the pie chart
  
  
  #Analysis 2-5 What are the average property prices for numbers of bathrooms and numbers of rooms?
  

  # Calculate the number of rooms without attached bathrooms, rooms with attached bathrooms, and public bathrooms
  dataSet$Rooms_No_Attach_Bathroom <- pmax(0, dataSet$Rooms - dataSet$Bathrooms)
  dataSet$Rooms_With_Attach_Bathroom <- pmin(dataSet$Rooms, dataSet$Bathrooms)
  dataSet$Public_Bathrooms <- dataSet$Bathrooms - dataSet$Rooms_With_Attach_Bathroom
  
  # Categorize properties based on the relationship between rooms and bathrooms
  dataSet$Relationship <- case_when(
    dataSet$Rooms > dataSet$Bathrooms ~ "All Rooms Attach Bathroom",
    dataSet$Bathrooms == 1 ~ "All Rooms No Attach Bathroom",
    dataSet$Public_Bathrooms > 1 ~ "Multiple Public Bathrooms",
    TRUE ~ "Mixed Relationship"
  )
  
  # Calculate the average price for each category
  avg_price_by_bathroom_type <- dataSet %>%
    group_by(Relationship) %>%
    summarize(Avg_Price = mean(Price, na.rm = TRUE))
  
  # Print the average price for each category
  print(avg_price_by_bathroom_type)
  # Create a lollipop chart to visualize the average price based on the relationship between bathrooms and rooms
  ggplot(avg_price_by_bathroom_type, aes(x = Relationship, y = Avg_Price)) +
    geom_segment(aes(xend = Relationship, yend = 0), color = "skyblue", size = 1.5) +  # Add segments with thicker lines
    geom_point(color = "blue", size = 5, shape = 21, fill = "white") +  # Add larger points with white fill
    labs(title = "Average Price based on Bathrooms and Rooms Relationship", x = "Relationship", y = "Average Price") +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
      axis.title = element_text(size = 14),  # Increase the font size of axis titles
      plot.title = element_text(size = 18, hjust = 0.5),  # Adjust the title font size and alignment
      panel.grid.major.y = element_line(color = "lightgray", linetype = "dashed")  # Add dashed horizontal grid lines
    ) +
    scale_y_continuous(labels = scales::comma)  # Format y-axis labels as comma-separated values
  
  
  
  #Analysis 2-6: How many of the number of bathrooms and rooms are more prefer by market.
  # Categorize bathrooms into ranges
  dataSet$Bathroom_Range <- cut(dataSet$Bathrooms, breaks = c(-Inf, 1, 3, 5, 8, Inf), labels = c("0-1", "2-3", "4-5", "6-8", "9+"))
  # Count the frequency of each range of bathrooms
  bathroom_frequency <- dataSet %>%
    group_by(Bathroom_Range) %>%
    summarize(Bathroom_Frequency = n())
  group_by(Bathroom_Range) %>%
  summarize(Bathroom_Frequency = n())
  # Categorize rooms into ranges
  dataSet$Room_Range <- cut(dataSet$Rooms, breaks = c(0, 2, 4, 6, 8, 10, 12, Inf), labels = c("1-2", "3-4", "5-6", "7-8", "9-10", "11-12", "12+"))
  # Count the frequency of each range of rooms
  room_frequency <- dataSet %>%
    group_by(Room_Range) %>%
    summarize(Room_Frequency = n())
  
  # Combine the data for both bathrooms and rooms
  combined_data <- merge(bathroom_frequency, room_frequency, by.x = "Bathroom_Range", by.y = "Room_Range", all = TRUE)
  
  # Plot the frequency of properties by number of bathrooms and rooms using a line graph
  ggplot(combined_data, aes(x = Bathroom_Range)) +
    geom_line(aes(y = Bathroom_Frequency, color = "Bathrooms"), size = 1.5) +
    geom_point(aes(y = Bathroom_Frequency, color = "Bathrooms"), size = 3) +
    geom_line(aes(y = Room_Frequency, color = "Rooms"), linetype = "dashed", size = 1.5) +
    geom_point(aes(y = Room_Frequency, color = "Rooms"), shape = 1, size = 3) +
    geom_text(aes(y = ifelse(!is.na(Bathroom_Frequency), Bathroom_Frequency, Room_Frequency),
                  label = ifelse(!is.na(Bathroom_Frequency), as.character(Bathroom_Frequency), as.character(Room_Frequency))), 
              hjust = -0.2, vjust = 0.5, size = 3.5, color = "black", fontface = "bold") +  
    labs(
      title = "Frequency of Properties by Number of Bathrooms and Rooms",
      x = "Number of Bathrooms and Rooms",
      y = "Frequency"
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",  
      axis.text.x = element_text(angle = 45, hjust = 1)  
    ) +
    scale_color_manual(values = c("blue", "red"), labels = c("Bathrooms", "Rooms"))
  group_by(Room_Range) %>%
  summarize(Room_Frequency = n())

  # Combine the data for both bathrooms and rooms
  combined_data <- merge(bathroom_frequency, room_frequency, by.x = "Bathroom_Range", by.y = "Room_Range", all = TRUE)

  # Plot the frequency of properties by number of bathrooms and rooms using a line graph
  ggplot(combined_data, aes(x = Bathroom_Range)) +
  geom_line(aes(y = Bathroom_Frequency, color = "Bathrooms"), size = 1.5) +
  geom_point(aes(y = Bathroom_Frequency, color = "Bathrooms"), size = 3) +
  geom_line(aes(y = Room_Frequency, color = "Rooms"), linetype = "dashed", size = 1.5) +
  geom_point(aes(y = Room_Frequency, color = "Rooms"), shape = 1, size = 3) +
  geom_text(aes(y = ifelse(!is.na(Bathroom_Frequency), Bathroom_Frequency, Room_Frequency),
                label = ifelse(!is.na(Bathroom_Frequency), as.character(Bathroom_Frequency), as.character(Room_Frequency))), 
            hjust = -0.2, vjust = 0.5, size = 3.5, color = "black", fontface = "bold") +  
  labs(
    title = "Frequency of Properties by Number of Bathrooms and Rooms",
    x = "Number of Bathrooms and Rooms",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",  
    axis.text.x = element_text(angle = 45, hjust = 1)  
  ) +
  scale_color_manual(values = c("blue", "red"), labels = c("Bathrooms", "Rooms"))
  
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
  
  # Create a stacked bar chart using ggplot2
  ggplot(furnishingCounts, aes(x = Furnishing, y = n, fill = Furnishing)) +
    geom_bar(stat = "identity") +
    labs(title = "Distribution of Furnishing Status", x = "Furnishing Status", y = "Count") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
# Analysis 3: Analysis of correlation between property type and the likelihood of it being furnished.
  
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
