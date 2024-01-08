# Group
# 1. Chin Hong Wei TP065390
# 2. Loo Hui En TP065181
# 3. Lim Ee Chian TP06513
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
  
#