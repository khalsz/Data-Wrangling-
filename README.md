# R_CW2_Exploratory_and_Regression_Analysis
Exploratory_Analysis_and_Regression


  # Introduction
  # GY7702 R for Data Science Course Work 2
library(dplyr)
library(tidyverse)
library(knitr)
library(readr)
library(lubridate)
library(ggplot2)
library(psych)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(car)
library(magrittr)
library(lmtest)
library(usethis)

### 1.0 Loading and selecting data needed for analysis
#Loading in data for analysis
OAC_Raw_uVariables_2011 <- 
  read.csv("GY7702_2021-22_Assignment_2_v1-1_datapack/2011_OAC_Raw_uVariables-GY7702_2021-22_CW2.csv")

#Loading data that would be used to extract my Output Area
LAD_Allocation_data <- read.csv("GY7702_2021-22_Assignment_2_v1-1_datapack/new.csv")

#Filtering out my allocated LAD
LAD_Allocation_data <- LAD_Allocation_data %>%
  filter(LAD11CD == "E09000006")

#Joining the two data to select my allocated Output Area only
OwnLadd <- LAD_Allocation_data   %>%
  left_join(
    OAC_Raw_uVariables_2011,  by = c("OA11CD" = "OA")
  ) %>%
  select(- c(LSOA11CD, LSO11ANM, MSOA11CD, MSOA11NM, LAD11CD, LAD11NM, LAD11NMW))

#Selecting variables needed for Analysis
explorData <- OwnLadd %>%
  select( u104:u115, u159:u167)


### 1.1 Exploratory analysis of the data 
describe(explorData,skew=TRUE, IQR = TRUE)






### 1.11 Showing the structure of the data 

str(explorData) %>%
  knitr::kable()



### 1.12 Visualizing the distribution of the data with Histogram and QQ plot


par(mar=c(5,5,3,0)) ##This margin command should do the trick

explorData %>% gather() %>%
  ggplot2::ggplot(
    aes(
      x = value
    )
  )+ 
  ggplot2:: geom_histogram(binwidth = 5)  +
  facet_wrap(~key, scales = 'free_x')

hist.data.frame(explorData)

for (i in 1:ncol(explorData)) {
  plt <-  ggplot2::ggplot(explorData, 
                          aes(
                            sample = explorData[,i]
                          )
  ) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line()+ 
    ggplot2::xlab(colnames( explorData[i]))
  print(plt)
  
}


### 1.13 Tranforming the data with Inverse hyperbolic sine function

for (i in 1:ncol(explorData)) {
  plt <-  ggplot2::ggplot(explorData, 
                          aes(
                            #adding inverse hyperbolic sine function
                            sample = asinh(explorData[,i])
                          )
  ) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line()+ 
    ggplot2::xlab(colnames( explorData[i]))
  print(plt)
  
}



### 1.21 Statistical approach to exploratory analysis and descriptive statistics of the data


stat_view <- explorData  %>%
  pastecs::stat.desc(norm = TRUE) %>%
  round(5)

print(stat_view) 




### 1.3 Kendall's regression correlation plot

corrplot(cor(explorData, method = "kendall"), type = "upper", 
         tl.cex=0.5, method = 'shade', order = 'AOE', 
         diag = FALSE,  tl.col="black")

### 2.0 Part 2
### 2.11 Selecting the data needed for the regression analysis
regression_data <- OwnLadd %>%
  select( Total_Population, u104:u115, u159:u167) %>%
  #converting each column to represent percentage of population
  mutate(
    across(u104:u167, 
           function(x){
             (x/Total_Population)*100
           })
  ) %>%
  #renaming the variables
  rename_with(
    function(x){paste('perc', x, sep = "_")}, 
    u104:u167
  )

### 2.12 Checking the normality of the variables after normalizing them with percentage population
stat_view2 <- regression_data  %>%
  pastecs::stat.desc(norm = TRUE) %>%
  round(5)
print(stat_view2)



### 2.13 Selecting the variable to be used for the regression analysis

forregression <- regression_data %>%
  select(perc_u106, perc_u112, perc_u162)



### 2.21 Pearson correlation between variable perc_u106 and perc_u112


forregression %$%
  cor.test(perc_u106, perc_u112)


### 2.22 Pearson regression between variables perc_u106 and perc_u162
forregression %$%
  cor.test( perc_u106, perc_u162)


### 2.31 Regression analysis between variable perc_u106 (dependent) ~ perc_u114 + perc_u165(Independent)

health_model <- forregression %$%
  lm(perc_u106 ~ perc_u112 + perc_u162)

#2.32 Summary of the model
summary(health_model)

# 
#   - The p-value is **0.00001625: p-value < 0.01**; Hence the model is significant. 
# We can reject the null hypothesis that none of the predictors have relationship with 
# the response variable. 
# # * This result is gotten by comparing the F-statistic to F distribution **11.15** where the 
# degrees of freedom is **(2, 1017)**
#   * F (2, 1017) = **11.15**
#   * Adjusted R-squared = **0.01953** 
#   
#   - Coefficient 
# * the coefficient  = **30.79670 (significant)**
# #   * The coefficient of slope for % of people with with Level 1, Level 2 or Apprenticeship 
# qualifications is estimated as **0.08282 (significant)**
# #   * The coefficient of slope for % of people with with Administrative and secretarial 
#   occupations is estimated as **0.16799 (Significant)**
  
  ### 2.4 Test for normality, homoscedasticity, independence and multocollinearity   

# 2.41 Test for normality 
health_model %>% 
  stats::rstandard() %>% 
  stats::shapiro.test()

# 2.42 Test for homoscedasticity 
health_model %>% 
  lmtest::bptest()

# 2.43 Test for independence 
health_model %>%
  lmtest::dwtest()

# 2.43 Test for multocollinearity 
health_model %>%
  vif()


# The output of the model indicates that the model fits **(F (2, 1017) = 11.15), 
# p-Value < 0.01**. However, the model on the percentage of people with  Level 1, Level 2 or 
# Apprenticeship qualifications and people with Administrative and secretarial occupations
# can only predict **2%** of people with good health. The model have normally 
# distributed residuals **(Shapiro-Wilk test, W=0.99, p=0.01678)**, no multicollinearity with 
# average **VIF 1.028645**, the residuals satisfy the assumption of homoscedasticity 
# **(Breusch-Pagan test, BP = 1.7153, p-value = 0.4242)** and assumptions of independence 
# **(Durbin-Watson test, DW = 1.9789, p-value = 0.3613)**, However, we can say that the model 
# is partially robust because of the low adjusted R-squared value. 
# # Based on the result, the model indicates that for every one percent increase in the 
# percentage of people with with Level 1, Level 2 or Apprenticeship qualifications, 
# there will be **0.08282** increase in the percentage of people with good health. 
# Similarly, for every one percent increase in the percentage of people with 
# Administrative and secretarial occupations, there will be **0.16799** increase in the 
# percentage of people with good health. 

### 2.51 Residual vs Fitted plot
health_model %>%
  plot(which = c(1))


# 2.51 gives an insight into the homoskedasticity of the residual. Since the red line is close 
# to the dash line, the linearity of the model seems to hold well, the model is homoskedastic as 
# the variance is not increasing, and point 770, 923 and 319 are outliers. 

### 2.52 Normal Q-Q plot

health_model %>%
  plot(which = c(2))


# 2.52 shows the normality of the residuals. The fact that the qq plot lies on the line shows 
# that it is normally distributed.

### 2.53 Residual vs Leverage plot
health_model %>%
  plot(which = c(5))

# 2.53 gives insight about the Cook's distance. No point fall outside the Cook's Distance, 
# indicating that there is no influential point in the regression model. 

