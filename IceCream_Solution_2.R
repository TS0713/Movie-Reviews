# Required Libraries 
library(ggplot2) # For visualizations
background_theme <- theme(
  panel.background = element_rect(fill = "lightblue",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                  colour = "white")
)
library(dplyr) # Data Preprocessing
library(fastDummies) # Converting categorical data into one hot encoding

# Loading Ice Cream data set from github link
Ice_cream_data <- "https://raw.githubusercontent.com/sedaerdem/Statistics/master/data/icecream.csv"
data_df <- read.csv(Ice_cream_data,sep=",",stringsAsFactors = F)

colnames(data_df)
## "ShopID"         "icecream_sales" "income"         "price"          "temperature"    "country"        "seasons"

# We have only 2 - countries data
unique(data_df$country) # A B - countries

#### Let's Analyze the distribution of Ice Cream Sales, Price, Income over Seasons, Countries

####################### Exploratory Data Analysis Starts Here ###########################

### Scatter Plot between Ice Cream Sales Vs Income to understand if the income has any effect over ice cream sales
ggplot(data=data_df,aes(x=icecream_sales,y=income))+geom_point()+ background_theme

## Scatter Plot between Ice Cream Sales Vs Income Over each country - to understand their distribution separately
ggplot(data=data_df,aes(x=icecream_sales,y=income,color=country))+geom_point()+facet_wrap(~country) + background_theme

### Scatter Plot between Ice Cream Sales Vs Income to understand if the income has any effect over ice cream sales
### Over Seasons
ggplot(data=data_df,aes(x=icecream_sales,y=income,color=seasons))+geom_point()+facet_wrap(~seasons) + background_theme

## Scatter Plot - Ice Cream Sales Vs Ice Cream Average Price - Over Seasons + Country
ggplot(data=data_df,aes(x=icecream_sales,y=income,color=seasons))+geom_point()+facet_wrap(country~seasons) + background_theme

#### All About Ice Cream Sales ####

## Jitter Plot over Box-Plot - Ice Cream Sales Over Country
ggplot(data=data_df,aes(x=country,y=icecream_sales,color=country))+geom_boxplot()+geom_jitter() + background_theme

## Density Plot - Ice Cream Sales Over Country 
ggplot(data_df, aes(x=icecream_sales,color=country)) + geom_density() + background_theme

## Jitter Plot over Box-Plot - Ice Cream Sales Over Seasons
ggplot(data=data_df,aes(x=seasons,y=icecream_sales,color=seasons))+geom_boxplot()+geom_jitter() + background_theme

## Density Plot - Ice Cream Sales Over Seasons 
ggplot(data_df, aes(x=icecream_sales,color=seasons)) + geom_density() + background_theme

## Jitter Plot Over Box-Plot - Ice Cream Sales Over Country  +  Seasons
ggplot(data=data_df,aes(x=country,y=icecream_sales,color=country))+geom_boxplot()+geom_jitter()+facet_wrap(~seasons,ncol=3) + background_theme

## Density Plot - Ice Cream Sales Over Country  +  Seasons 
ggplot(data_df, aes(x=icecream_sales,color=country)) + geom_density()  +facet_wrap(~seasons,ncol=3) + background_theme


#### All About Ice Cream Average Price ####

## Jitter Plot Over Box-Plot - Ice Cream Average Price Over Country
ggplot(data=data_df,aes(x=country,y=price,color=country))+geom_boxplot()+geom_jitter() + background_theme

## Density Plot - Ice Cream Average Price Over Country 
ggplot(data_df, aes(x=price,color=country)) + geom_density() + background_theme

## ## Jitter Plot Over Box-Plot - Ice Cream Average Price Over Seasons
ggplot(data=data_df,aes(x=seasons,y=price,color=seasons))+geom_boxplot()+geom_jitter() + background_theme

## Density Plot - Ice Cream Average Price Over Seasons
ggplot(data_df, aes(x=price,color=seasons)) + geom_density() + background_theme

## Jitter Plot Over Box-Plot - Ice Cream Average Price Over Country  +  Seasons
ggplot(data=data_df,aes(x=country,y=price,color=country))+geom_boxplot()+geom_jitter()+facet_wrap(~seasons,ncol=3) + background_theme

## Density Plot - Ice Cream Average Price Over Country  +  Seasons 
ggplot(data_df, aes(x=price,color=country)) + geom_density()  +facet_wrap(~seasons,ncol=3) + background_theme

######################## Exploratory Data Analysis Ends Here ###########################


################################# Multi Linear Regression - Model Building #################################

# Converting Categorical country & season into one hot encoding 
data_df <- data_df %>% select(-ShopID)
ice_df <- dummy_cols(data_df) 
ice_df <- ice_df %>% select(-country,-seasons) # removing 

# Dependent Feature - Target
# icecream_sales

# Independent Features used in Multi-linear regression
# "income"    "price"    "temperature"  "country_A"  "country_B"   "seasons_Autumn" "seasons_Spring" "seasons_Summer" "seasons_Winter"

# Building Multi-linear regression model for prediction ice cream sales given features 
# Training on data 
lm_ice_reg <- lm(icecream_sales~.,data=ice_df)
summary(lm_ice_reg)

# Confidence intervals of regression coefficients at 95% confidence level
confint(lm_ice_reg)

##### C - 8
# 8.	Explain if your data meets the regression conditions.
# Below plots explains about whether data follows regression conditions
plot (lm_ice_reg)


### D What is the predicted value of ice cream sales in a location in Country B where the average income of residents is
### £30,000 when the temperature is 3 degrees in Winter, and average price per serving of ice cream is £5

# Preparing the DATA as per the details
new_IceCream_data = list()


new_IceCream_data$income <- 30000
new_IceCream_data$temperature <- 3
new_IceCream_data$seasons_Winter <- 1
new_IceCream_data$seasons_Autumn <- 0
new_IceCream_data$seasons_Spring <- 0
new_IceCream_data$seasons_Summer <- 0
new_IceCream_data$country_B <- 1
new_IceCream_data$country_A <- 0
new_IceCream_data$price <- 5

new_IceCream_data_df <- data.frame(t(sapply(new_IceCream_data,c)))

# Using the model to predict the ice cream sales
predict(lm_ice_reg,newdata = new_IceCream_data_df)

# Predicting and calculating the confidence interval of 90% confidence level
predict(lm_ice_reg,newdata = new_IceCream_data_df,interval="confidence",level=0.90)

# fit         lower      upper
# 126.2      45.7       206.6


######################### Hypothesis Test Starts  here ###################################

# B. Hypothesis testing: (10 points) Construct your hypotheses for testing the average temperature in
#    location in Country A relative to the average temperature in a location in a location in Country B. 
#    Test the hypothesis and explain the result. (100 words max)


### Checking Temperature Density Distribution plot for each Country 

ggplot(data=data_df,aes(x=temperature,color=country)) + geom_density() + background_theme

ggplot(data=data_df,aes(x=temperature,color=country)) + geom_boxplot() + background_theme


# Calculating variance of temperatures of across each country
data_df %>% group_by(country) %>% summarise(variance_country = var(temperature)) %>% data.frame(stringsAsFactors = F)

# Below are the values for country => variance_country => Temperature 
#   A      63.54061
#   B      63.31720

### Looking at the values we can say variance across the temperature is almost equal 

# Filtering out the Country A & B temperatures
Country_A_temp <- data_df$temperature[data_df$country=="A"]
Country_B_temp <- data_df$temperature[data_df$country=="B"]

# Before we are going to check existence of differences across their temperature means,
# First ensure that data should pass a test of homoscedasticity - are temperature variances same  
# We do this in R using Fisher's F-test

# H0: No difference in their temperature in variance between 2 countries.
# H1: Difference exists in variance of temperatures between 2 countries.
var_temp_p_value <-  var.test(Country_A_temp, Country_B_temp)$p.value

ifelse(var_temp_p_value > 0.05,
       ("No difference in their temperature in variance between 2 countries - Null Hypothesis"),
       ("Difference exists in variance of temperatures between 2 countries - Alternate Hypothesis")
)

# In the above case if p > 0.05, then we can assume that the variances of temperature's of both countries are homogeneous. 
# In this case, we run a classic Student's two-sample t-test by setting the parameter var.equal = TRUE

# H0: No difference in average of temperatures between 2 countries.
# H1: Difference exists in average of temperatures between 2 countries.
temperature_test_p_value <- t.test(Country_A_temp, Country_B_temp, var.equal = TRUE)$p.value

ifelse(temperature_test_p_value > 0.05,
       ("No difference in average of temperatures between 2 countries - Null Hypothesis"),
       ("Difference exists in average of temperatures between 2 countries - Alternate Hypothesis")
)

######################### Hypothesis Test Ends here ###################################