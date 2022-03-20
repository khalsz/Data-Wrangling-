

library(tidyverse)
library(knitr)
library(readr)
library(lubridate)

##### Part 1  Part 1.1 #####
#Loading the data into a variable, filtering it, converting it into wid format and replacing NA with 0
covid19_vaccinations <- readr::read_csv("GY7702_2021-22_Assignment_1_v1-0/covid19_vaccinations_2020-12-06_2021-10-02_long.csv")

covid19 <- readr::read_csv(
  "GY7702_2021-22_Assignment_1_v1-0/covid19_vaccinations_2020-12-06_2021-10-02_long.csv") %>% 
  dplyr::filter(area_name == ("East Suffolk") | area_name == ("Derby") | area_name == ("Cambridge")
  )  %>%
  #converting to wider format
  tidyr::pivot_wider(
    names_from = area_name,
    values_from = value
  ) %>%
  tidyr::replace_na(list("East Suffolk" = 0, "Derby" = 0,  "Cambridge" = 0) ) 



###### Part 1.2 #######
#Loading the population per Local Authority District data
ukpopest <- readr::read_csv("GY7702_2021-22_Assignment_1_v1-0/ukpopestimatesmid2020on2021geography.csv")

#Joining Covid_19 data with population per Local Authority District data to contain all in the former 
join<- covid19_vaccinations %>%
  dplyr::filter(area_name == ("East Suffolk") | area_name == ("Derby") | area_name == ("Cambridge")
  )  %>%
  dplyr::left_join(
    ukpopest, 
    by = c("area_name" = "Name")
  ) %>%
  #remove column code becuase it is replicated
  dplyr::select(-Code)

head(join)


##### Part 2  Part 2.1 #######
#creating highest number of new covid cases in each LAD, 
#and new cases as a ratio of total population
 join %>%
  tidyr::pivot_wider(
    names_from = metric,
    values_from = value
  ) %>%
  #Replacing NA values in the date with 0
  tidyr::replace_na(list(new_cases = 0, cumulative_cases = 0, 
                         new_first_vaccine_dose =0, 
                         new_second_vaccine_dose = 0) ) %>%
  dplyr::mutate(New_cases_perc_of_pop = new_cases/`All ages` *100 ) %>%
  dplyr::select(date_reported, area_name, new_cases,New_cases_perc_of_pop) %>%
  dplyr::arrange(date_reported) %>%
  dplyr::group_by(area_name) %>%
  dplyr::slice_max(new_cases)%>%
  knitr::kable()
  
  # llol <- covid19_vaccinations %>%
  #   dplyr::filter(metric == "new_cases") %>%
  #   dplyr::filter(area_name == ("East Suffolk") | area_name == ("Derby") | area_name == ("Cambridge")) %>%
  #   dplyr::select(date_reported, area_name, value)%>%
  #   tidyr::pivot_wider(
  #     names_from = area_name, 
  #     values_from = value
  #   )
  # llo <- covid19_vaccinations %>%
  #   dplyr::filter(metric == "new_cases") %>%
  #   dplyr::filter(area_name == ("East Suffolk") | area_name == ("Derby") | area_name == ("Cambridge"))
  
  ##### Part 2  Part 2.2 #######
# creating table for new cases in each LAD from 2021-06-24 to 2021-07-31
 gv <-  covid19_vaccinations %>% 
   dplyr::filter(between( date_reported, as.Date("2021-06-24"), as.Date("2021-07-31")))%>%   
   dplyr::filter(metric == "new_cases") %>%
   dplyr::filter(area_name == ("East Suffolk") | area_name == ("Derby") | area_name == ("Cambridge")) %>%
   dplyr::select(date_reported, area_name, value)%>%
    tidyr::pivot_wider(
      names_from = area_name, 
      values_from = value
    )
   
#Creating new variable to store present new cases value minus 7 days ago value
mmk <- gv

 mmk$bbb <- 0
 mmk$ccc <- 0
 mmk$vvv <- 0
 
 # Running a loop to create table showing change in new cases as a percentage of 7 days before 
for (i in 1: length(mmk$date_reported) ) {
  if(i >7){
 mmk$bbb[i] <- paste((((mmk$Derby[i] - mmk$Derby[i-7])/mmk$Derby[i-7]) *100),"%")
 mmk$ccc[i] <-  paste((((mmk$`East Suffolk`[i] - mmk$`East Suffolk`[i-7])  /mmk$`East Suffolk`[i-7]) *100),"%") 
  mmk$vvv[i] <- paste((((mmk$Cambridge[i] - mmk$Cambridge[i-7]) /mmk$Cambridge[i-7]) *100),"%") 
  }
  print(paste( mmk$ccc[i], mmk$bbb[i], mmk$vvv[i]))
}

 # Filtering out to have only the dates needed
gbbcv <- mmk %>% 
dplyr::select(date_reported, ccc, bbb, vvv) %>%
dplyr::filter(between(date_reported, as.Date("2021-07-01"), as.Date("2021-07-31"))) 
  
#Renaming the variable back to normal LAD names
 colnames(gbbcv)[2] = "East Suffolk"
 colnames(gbbcv)[3] = "Derby"
 colnames(gbbcv)[4] = "Cambridge"
 gbbcv %>% 
   knitr::kable()
 
 ##### Part 2  Part 2.3 #######
 # Creating function that returns weekly mean for first vaccine doses, 
 # for the month that with the highest first vaccine doses  

 H_ <- function(lad){
   H_first_dose <- 
     #extracting the month with the highest daily new cases
   lubridate::ymd(
     covid19_vaccinations %>%
       dplyr::filter(area_name == lad) %>%
       dplyr::filter(metric == "new_first_vaccine_dose") %>%
       dplyr::slice_max(value) %>%
       dplyr::select(date_reported) %>%
       dplyr::pull(1, 1) 
   )%>%
   month()
   
   #Framework function for analyzing the weekly average
   av_first_week <- covid19_vaccinations %>%
     dplyr::select(date_reported, metric, area_name, value) %>%
     dplyr::filter(area_name == lad) %>%
     dplyr::filter(metric == "new_first_vaccine_dose") %>%
     dplyr::filter(lubridate::month(date_reported) == H_first_dose)%>%
     dplyr::group_by(Week_st = cut(date_reported, seq(date_reported[1], last(date_reported), by = 7))) %>%
     dplyr::group_by(Week_st)%>%
     dplyr::mutate(weeks = paste( date_reported[[1]], "to", date_reported[[1]]+ lubridate::ddays(6)))%>%
     dplyr::ungroup()%>%
     dplyr::select(weeks, value) %>%
     dplyr::group_by(weeks)%>%
     dplyr::summarise(Average_number_of_first_doses = mean(value)) %>%
     knitr::kable()
   return(av_first_week)
 }
 
 
 
 ####### Weekly first doses average for LAD Derby in the month with the highest first dosage########
H_("East Suffolk")
 
####### Weekly first doses average for LAD Derby in the month with the highest first dosage########
H_("Derby")
 
####### Weekly first doses average for LAD Cambridge in the month with the highest first dosage########
H_("Cambridge")


 
 #### Part 3 ########

#function for sum of monthly incidents in East_Suffolk
East_Suffolk_monthly_sum <- function( x){
  East_Suffolk_monthly <- covid19_vaccinations %>%
    dplyr::filter(area_name == ("East Suffolk") )%>%
    dplyr::filter(metric == x) %>%
    dplyr::mutate(monts = month(date_reported)) %>%
    dplyr::group_by(monts) %>%
    dplyr::summarise(mont_new = sum(value))
  return(East_Suffolk_monthly$mont_new)
}

#function for sum of monthly incidents in England
covid19_monthly_sum <- function(x) {
  covid19_monthly <- covid19_vaccinations %>%
    dplyr::filter(metric == x) %>%
    dplyr::mutate(monts = month(date_reported)) %>%
    dplyr::group_by(monts) %>%
    dplyr::summarise(mont_new = sum(value))
  return(covid19_monthly$mont_new)
}


#function for average monthly incidents in East_Suffolk
East_Suffolk_monthly_average <- function(x){
  East_Suffolk_monthly <- covid19_vaccinations %>%
    dplyr::filter(area_name == ("East Suffolk") )%>%
    dplyr::filter(metric == x) %>%
    dplyr::mutate(monts = month(date_reported)) %>%
    dplyr::group_by(monts) %>%
    dplyr::summarise(mont_new = mean(value))
  return(East_Suffolk_monthly$mont_new)
}

#function for average monthly incidents in England
covid19_monthly_average <- function(x) {
  covid19_monthly <- covid19_vaccinations %>%
    dplyr::filter(metric == x) %>%
    dplyr::mutate(monts = month(date_reported)) %>%
    dplyr::group_by(monts) %>%
    dplyr::summarise(mont_new = mean(value))
  return(covid19_monthly$mont_new)
}


#function for total sum of  incidents in East_Suffolk
East_Suffolk_overall_sum <- function(x){
  East_Suffolk_overall <- covid19_vaccinations %>%
    dplyr::filter(area_name == ("East Suffolk") )%>%
    dplyr::filter(metric == x) %>%
    dplyr::summarise(suum =  sum(value))
  return(East_Suffolk_overall)
}

#function for total sum of  incidents in East_Suffolk
covid19_overall_sum <- function(x){
  covid19_overall <- covid19_vaccinations %>%
    dplyr::filter(metric == x) %>%
    dplyr::summarise(suum =  sum(value))
  return(covid19_overall)
}

# #Comparing the overall number of new cases in my allocated LAD to overall new cases in England
East_Suffolk_overall_sum("new_cases")/ covid19_overall_sum("new_cases")*100


# #Comparing the overall number of new_first_vaccine_dose in my allocated LAD to overall new cases in England
East_Suffolk_overall_sum("new_first_vaccine_dose")/ covid19_overall_sum("new_first_vaccine_dose")*100
# 
# #Comparing the overall number of cumulative_cases in my allocated LAD to overall new cases in England
East_Suffolk_overall_sum("cumulative_cases")/ covid19_overall_sum("cumulative_cases")*100

# 
# #Comparing the overall number of new_second_vaccine_dose in my allocated LAD to overall new cases in England
East_Suffolk_overall_sum("new_second_vaccine_dose")/ covid19_overall_sum("new_second_vaccine_dose")*100


#Comparing the number of new cases in my allocated LAD to overall new cases in England
East_Suffolk_monthly_sum("new_cases")/covid19_monthly_sum ("new_cases") *100



#Comparing the number of new_first_vaccine_dose in my allocated LAD to overall new cases in England
East_Suffolk_monthly_sum("new_first_vaccine_dose")/covid19_monthly_sum ("new_first_vaccine_dose") *100


#Comparing the number of cumulative_cases in my allocated LAD to overall new cases in England
East_Suffolk_monthly_sum("cumulative_cases")/covid19_monthly_sum ("cumulative_cases") *100


#Comparing the number of new_second_vaccine_dose in my allocated LAD to overall new cases in England
East_Suffolk_monthly_sum("new_second_vaccine_dose")/covid19_monthly_sum ("new_second_vaccine_dose") *100


#Figure 1: Creating side by side plot of monthly average new cases in my allocated LAD and in England 
options(scipen = 1000) 
par(mfrow = c (1, 2))
plot(covid19_monthly_average ("new_cases"), col = "red", type = "b", main = "Monthly Average New Cases in England",
     xlab = "months", ylab = "number of cases", cex.main=1)
plot(East_Suffolk_monthly_average("new_cases"), col = "blue", type = "b", main = "Monthly Average New Cases in East_Suffolk",
     xlab = "months", ylab = "number of cases", cex.main=1)


#Figure 2: Creating side by side plot of monthly sum of new cases in my allocated LAD and in England 
par(mfrow = c (1, 2))
plot(covid19_monthly_sum ("new_cases"), col = "red", type = "b", main = "Monthly Sum of New Cases in England", 
     xlab = "months", ylab = "number of cases", cex.main=1)
plot(East_Suffolk_monthly_sum("new_cases"), col = "blue", type = "b", main = "Monthly Sum of New Cases in East_Suffolk", 
     xlab = "months", ylab = "number of cases", cex.main=1)


#Figure 3: Creating side by side plot of monthly average new first vaccination collected in my allocated LAD and in England 
par(mfrow = c (1, 2))
plot(covid19_monthly_average ("new_first_vaccine_dose"), col = "red", type = "b", 
     main = "Average Monthly First Vaccination in England", xlab = "months", ylab = "number of first vaccination", cex.main=1)
plot(East_Suffolk_monthly_average("new_first_vaccine_dose"), col = "blue", type = "b",  
     main = "Average Monthly First Vaccination in East_Suffolk", xlab = "months", ylab = "number of first vaccination", cex.main=1)

#Figure 4: Creating side by side plot of monthly sum of new first vaccination collected in my allocated LAD and in England 
par(mfrow = c (1, 2))
plot(covid19_monthly_sum ("new_first_vaccine_dose"), col = "red", type = "b",  
     main = "Sum of New Cases in England", xlab = "months", ylab = "number of first vaccination", cex.main=1)
plot(East_Suffolk_monthly_sum("new_first_vaccine_dose"), col = "blue", type = "b", 
     main = "Sum of New Cases in East_Suffolk", xlab = "months", ylab = "number of first vaccination", cex.main=1)

#Figure 5: Creating side by side plot of monthly average cumulative COVID cases in my allocated LAD and in England 
par(mfrow = c (1, 2))
plot(covid19_monthly_average ("cumulative_cases"), col = "red", type = "b", 
     main = "Average Monthly cumulative_cases in England", xlab = "months", ylab = "Average number of cumulative_cases", cex.main=1)
plot(East_Suffolk_monthly_average("cumulative_cases"), col = "blue", type = "b", 
     main = "Average Monthly cumulative_cases in East_Suffolk", xlab = "months", ylab = "Average number of cumulative_cases", cex.main=1)

#Figure 6: Creating side by side plot of monthly sum of cumulative COVID cases in East_Suffolk and England 
par(mfrow = c (1, 2))
plot(covid19_monthly_sum ("cumulative_cases"), col = "red", type = "b", 
      main = "Sum of Monthly cumulative_cases in England", xlab = "months", ylab = "number of cumulative_cases", cex.main=1)
plot(East_Suffolk_monthly_sum("cumulative_cases"), col = "blue", type = "b", 
      main = "Sum of Monthly cumulative_cases in East_Suffolk", xlab = "months", ylab = "number of cumulative_cases", cex.main=1)

#Figure 7: Creating side by side plot of monthly average of new second vaccination collected in my allocated LAD and in England 
par(mfrow = c (1, 2))
plot(covid19_monthly_average ("new_second_vaccine_dose"), col = "red", type = "b", 
     main = "Monthly Average second_vaccine in England", xlab = "months", ylab = "number of second_vaccine", cex.main=1)
plot(East_Suffolk_monthly_average("new_second_vaccine_dose"), col = "blue", type = "b", 
     main = "Monthly Average second_vaccine in East_Suffolk", xlab = "months", ylab = "number of second_vaccine", cex.main=1)

#Figure 8: Creating side by side plot of monthly sum of new second vaccination collected in East_Suffolk and England 
options(scipen = 1000) 
par(mfrow = c (1, 2))
plot(covid19_monthly_sum ("new_second_vaccine_dose"), col = "red", type = "b", 
     main = "Monthly Sum of second_vaccine in England", xlab = "months", ylab = "number of second_vaccine", cex.main=1)
plot(East_Suffolk_monthly_sum("new_second_vaccine_dose"), col = "blue", type = "b", 
     main = "Monthly Sum of second_vaccine in East_Suffolk", xlab = "months", ylab = "number of second_vaccine", cex.main=1)


# The data a covid data for the entire England, consisting of 380 Local Authority District (LAD) in England. The data include information 
# about new covid cases, cumulative covid cases, first and second vaccines administered in all the district. I am required to 
# compared the incidents or activity in one the LAD assigned to me (East_Suffolk). 
# With the aim of comparing both areas, I built a two functions that would be used to analyze the average monthly incidents and total 
#monthly incidents, to avoid replications. Various incidents like new cases, new first covid vaccine, cumulative cases and second 
# covid vaccine was the required imput for the function. The function required a number of Dplyr functions like filter, select and others. 
# Line charts were also built to show compare results from both regions. 
#Based on analysis, the total number of cases in East_Suffolk is about 0.24% of the total new covid cases in ENgland
#Based on analysis, the number of new vaccines given in East_Suffolk represents 0.4% of all vaccines given in ENgland
#Result shows that East_Suffolk is responsible about 0.2% of all cumulative covid cases in the UK. 
#0.4% of all second vaccines in ENgland was administered in East_Suffolk. 
#Starting from January, the number of new vaccines collected in East_Suffolk as a percentage of the overall 
#new vaccines in England reduced gradually until May. 
#According to figure 1, the highest average number of new covid cases was witnessed in January 2021 in 
#East_Suffolk and in England, and the lowest was seen around May for both regions. 
#According to figure 2, the highest total number of covid new cases was seen in January 2021 for both East_Suffolk and the entire 
#England. The sum of covid cases monthly bottomed to lowest in May and October 2021 for both regions. 
#Even though there are some differences in the trend of the chart for average monthly first vaccine doses given in the 
# entire England and East_Suffolk Both regions administered the highest in February 2021, and lowest in September 2021,
# figure 3 shows. 
#While the entire England maintain highest number of vaccines given in February and March according to figure 4, East_Suffolk administered the 
#highest number in February and there was a drastic decline in March. 
# According to figure 5 and 6, the average number of cumulative cases and the total number of cumulative cases in the entire England 
#and in East_Suffolk alone maintained the same trend. The trend began with a low value in January 2021. While the 
#average cumulative cases in both region peaked in October 2021, the total cumulative cases peaked in 
# September 2021.
# According to figure 7 and 8, the lowest number of second vaccine dosage administered in the entire ENgland and East_Suffolk
# was lowest February. While a double month (April and May) was seen in the entire England, East_Suffolk had the highest in 
#April and began to fall in the following month. 



# Part 4 #####
# Thanks to some of the practical classes we did and support from online platforms, I was able to produce this analysis and
#document. In order to achieve the required task, I use several based, tidyverse and dplyr library functions, among others. 
# Several analysis were performed including filtering out, selection, converting table to wide or long format and joining of two data. 
# Also, I built functions and group by select by specific criterion. 
#It took me several weeks to complete the task becuase i was doing it in badges, and I had problem in some of the phases
# so I had to search on internet. 
#https://stackoverflow.com/questions/1249548/side-by-side-plots-with-ggplot2
#https://stackoverflow.com/questions/41793931/plotting-images-side-by-side-using-matplotlib
#https://stackoverflow.com/questions/5352099/how-to-disable-scientific-notation