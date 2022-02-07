library(readr)
education <- read_csv("C:/Users/jamit/Desktop/Aispry/EDA_Dataset/education.csv")

# First Moment Business Decisions
mean(education$workex)
median(education$workex)
# There is no default function for creating mode. Hence we create a custom function named modes.
modes <- function(x){
  ux <- unique(x)  # Grabs the unique values in x and creates a tuple named ux
  tab <- tabulate(match(x,ux))  # Creates a table how many times the the individual variable appears in x
  ux[tab==max(tab)] # Displays the items with max times of repeating based on index.
}
modes(education$workex)

# Second moment Business Decision: Measures of dispersion.
var(education$workex)
attach(education) # Attaching the dataframe instead of referring every time.
sd(workex)

# Third moment Business decision:  Skewness
install.packages("moments")  # Getting the missing library for calculating skewness
library(moments)
skewness(gmat)
skewness(workex)
# Positive Skewness means right tailed distribution of the data
# Negative Skewness means left tailed distribution of the data

# Fourth moment Business decision: Kurtosis
kurtosis(gmat)
kurtosis(workex)
# Positive kurtosis means the data is peaked at the middle and it is a narrower curve
# Negative kurtosis means the data is widely spread  and a wider curve 

# Graphical representation of the data

# Bar plot
barplot(workex)

# Dot chart
dotchart(workex)

# Histogram
hist(workex)

# Density plot
plot(density(workex))

# Box plot
boxplot(gmat)
z <- boxplot(gmat)
z$out   # Displays the outliers

# Quantile Quantile plot. Used for formally determining the data is normally distributed
qqnorm(gmat)
qqline(gmat)  # As almost all the dots are about the line the data is normally distributed

######End of Session###########

rm(list = ls()) # Removes all the dataframes and variables from memory

####################------------------- Data Cleansing -------------------####################

### Type Casting ###

data <- read.csv(file.choose()) # Load the Ethnic Diversity dataset for EDA_Dataset

str(data) # str - gives the structure of the data all columns  along with its data types
summary(data) # summary gives the statistical summary of the numerical data 
attach(data)  # Attaching the dataframe to the memory

# Typecasting is forcibly converting one data type to another 

is.numeric(data$Position) # Returns true or false on the data types
str(data$age) # returns int as the data type
data$age <- as.numeric(data$age) # Type casting the age data as num type and reassigning it to the age column in the dataframe
str(data$age) # Returns num as data type
data$age <- as.integer(data$age)  # Converting the age data to integer again
str(data$Sex) # checking the data type of Sex, which returns as chr
data$Sex <- as.factor(data$Sex) # Typecasting the Sex as a factor and assigning it to Sex column
str(data$Sex) # Returns the data type. which is now a Factor with 2 levels 

# We can directly take Strings as factor while importing the data itself

data1 <- read.csv(file.choose(),stringsAsFactors = TRUE) # Load the Ethnic Diversity dataset for EDA_Dataset
str(data1) # All the characters are converted as factors

### End of Session ###
rm(list=ls())

### Handling Duplicates ###
data <- read.csv(file.choose())# Load the mtcars_dup dataset for EDA_Dataset
dup <- duplicated(data) # creating an array of True or False if a row is duplicated or not 
data_new <- data[!duplicated(data),] # A new data frame with no duplicate rows is created



























