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
dim(data) # gives the dimensions of the dataset 
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

### End of Session ###
rm(list=ls())

### Zero Variance Data ###

data <- read.csv(file.choose())# Load the Ethnic Diversity dataset for EDA_Dataset
apply(data, 2, var)   #Calculate variance on every row for the dataset
which(apply(data,2,var)==0) # gives the index of the column where variance is zero

### End of Session ###
rm(list=ls())

### Missing Values treatment ###
data <- read.csv(file.choose())# Load the Modified Ethnic dataset for EDA_Dataset
sum(is.na(data))# Returns the count of the missing values
summary(data) # Shows the Count of missing values for the numerical columns
data_new <- na.omit(data) # This removes the entire row of the dataset where there is even a single NA value

## Imputation techniques ##

boxplot(data$Salaries)$out # Getting the outlier data

# Mean Imputation #
# Calculating mean after removing NA values and assigning it to the index values of the column where value is NA
data$Salaries[is.na(data$Salaries)] <- mean(data$Salaries, na.rm = TRUE)
sum(is.na(data$Salaries)) # Checking if there are any NA values in the column 
data$age[is.na(data$age)]<- mean(data$age,na.rm = TRUE) # Mean-Imupting the Age 

# Mode imputation #
# Mode imputation is done primarily in categorical data. We convert the categorical data into factor first
data$Race <- as.factor(data$Race)
summary(data$Race)
# creating a function calculate mode
modes <- function(x){
  ux <- unique(x)  # Grabs the unique values in x and creates a tuple named ux
  tab <- tabulate(match(x,ux))  # Creates a table how many times the the individual variable appears in x
  ux[tab==max(tab)] # Displays the items with max times of repeating based on index.
}
# Imputing mode to the missing values in Race Column
data$Race[is.na(data$Race)] <-modes(na.omit(data$Race))
sum(is.na(data$Race))
summary(data$Race)

# Another way to Calculate Mode
Moder <-function(x){
  a=table(x)
  names(a[which.max(a)])
}
# Imputing mode to the missing values in Position Column
data$Position[is.na(data$Position)]<-Moder(na.omit(data$Position))
data$Position <- as.factor(data$Position)
summary(data$Position)
# Imputing mode to the missing values in Department Column
data$Department <- as.factor(data$Department)
data$Department[is.na(data$Department)] <- Moder(na.omit(data$Department))

### End of Session ###
rm(list=ls())

### Dummy Variable Creation ###
data <- read.csv(file.choose()) # Load ethnic diversity data set from EDA_Dataset
install.packages("fastDummies") # Installing the package 'fastDummies'
library(fastDummies) # Calling the library 'fastDummies'
str(data)
summary(data)

## One Hot Encoding ##
# Creating dummies in the dataset using the fastDummies library, dummy_cols function.
data_dummy <- dummy_cols(data,select_columns = c("Position","State","Sex","MaritalDesc","CitizenDesc","EmploymentStatus","Department",
                                                 "Race"),remove_first_dummy = TRUE,remove_selected_columns = TRUE)
# remove_first_dummy removes the first dummy. So that n-1 dummies are created for n Variables.

## Label Encoding ##
install.packages("CatEncoders")
library(CatEncoders)
# Creating Dummies in the dataset using CatEncoders library, LabelEncoder
data_label <- LabelEncoder.fit(data$Position) # Creates an object.... just run the name 'data_label' to see all the names and the labels
position_new <- transform(data_label,data$Position)# Create a vector with the label Values for the Position Column
position_new
data_label_new <- cbind(data,position_new) # Adding the labels vector as a column at the end
str(data_label)

### End of Session ###
rm(list=ls())

### Standardization and Normalization ###

## Normalization ##
data <- read.csv(file.choose()) # Load ethnic diversity data set from EDA_Dataset
# Writing a function for normalization
norm <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
}
data_norm <- data
data_norm$Salaries <- norm(data_norm$Salaries)
data_norm$age <- norm(data_norm$age)

### End of Session ###
rm(list=ls())

## Standardization ##
data <- read.csv(file.choose()) # Load mtcars data set from EDA_Dataset
data_std <- as.data.frame(scale(data)) # Scale is built-in Standardization formula

#### Outlier Treatment ####
data <- read.csv(file.choose()) # Load ethnic diversity data set from EDA_Dataset
boxplot(data$Salaries)$out # We see outliers here
boxplot(data$age)$out # We donot have any outliers here.
# Identify the 25% and 75% values in the distribution
qunt <- quantile(data$Salaries,probs = c(0.25,0.75))
qunt
H <- 1.5*IQR(data$Salaries,na.rm = TRUE) # Calculating the 1.5*IQR for the salaries after removing NA values
# Since the Outliers lie beneath Q1-1.5*IQR and beyond Q3+1.5*IQR
#Now replace the outliers with the 25% and 75% values
data$Salaries[data$Salaries < (qunt[1] - H)] <- qunt[1] # Replacing values less than IQR*1.5 with 25% value
data$Salaries[data$Salaries > (qunt[2] + H)] <- qunt[2] # Replacing values greater than IQR*1.5 with 75% value
# Check if there are any outliers now..
boxplot(data$Salaries)$out  # There are no outliers present now.
 
# We use 'robustHD' package for winsorize function.
install.packages("robustHD")
library(robustHD)
data <- read.csv(file.choose()) # Load ethnic diversity data set from EDA_Dataset
boxplot(data$Salaries)$out # Outliers are present here
data$Salaries <- winsorize(data$Salaries) # by default winsorize takes 5% and 95%. These arguments can be changed
boxplot(data$Salaries)$out  # No outliers are present 

### End of Session ###
rm(list=ls())


#### Scatter Plot ####
data <- read.csv(file.choose()) # Import the wc_at dataset from EDA_Dataset folder
attach(data)
plot(Waist,AT)
# 1.We can straight way say that there is a positive correlation.
# 2.However, we cannot comment if it is a moderate or weak correlation as it is subjective.
# 3.The Data also appears to be Linear
cor(Waist,AT) # Finding out the coefficient of correlation(r)
# Since 0.82 is a positive value and below 0.85 We Can say the two variables are moderately positively correlated.
cov(Waist,AT) # Calculating Co-variance for waist and AT
# Since the value is 635.91 . We can comment on direction but not strength (as the change in units change the strength here)

### End of Session ###
rm(list=ls())



####################------------------- Hierarchical Clustering  -------------------####################
install.packages("readxl") # Installing the 'readxl' package to handle excel files
library(readxl)
data <- read_excel(file.choose()) # Load University clustering data set from Hierarchical_clustering
# Dropping the state name 
my_data <-data[,-c(2)]
summary(my_data)
# Normalize the data excluding the University name
norm_data <- scale(my_data[,2:7])  # scale function is used to standardize the data 
summary(norm_data)
#Calculating the distance matrix 
d <- dist(norm_data,method = "euclidean") # d is a distance matrix.
fit <- hclust(d,method = "complete") # Using complete linkage generate an object
# Displaying Dendogram
plot(fit,hang=-1)
# In Hierarchical clustering we decide upon the number of clusters after running the dendogram
# At times no.of clusters were given by the business. Then Business requirement gets priority
rect.hclust(fit,k=3,border="red")# Displaying as 3 groups of the data
# Cut the tree into 3 clusters
groups <- cutree(fit,k=3) # This creates a set of values as per index showing which group the university belong to
membership <- as.matrix(groups) # Converting the set of values into a Dataframe
final <- data.frame(membership,my_data) # appending the Column to the data to show the group
# Aggregating the group data.
aggregate(final[,3:8],by=list(final$membership),FUN = mean) # by>> group by what. FUN>> aggregating function
# Writing the data into a CSV file 
library(readr)
write_csv(final,"hclust_output.csv")
getwd()  # To get the current working directory.
































