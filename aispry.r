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






