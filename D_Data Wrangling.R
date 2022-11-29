library(pastecs)
library(dplyr)
library(ggplot2)
library(reshape)

#Part 1: Loading Data into R
link <- 'C:/Users/khalsz/Downloads/data5/data6/'
setwd(link)
data1 <- read.csv('covid19_vaccinations_2020-12-06_2021-10-02_long.csv')
data2 <- read.csv('ukpopestimatesmid2020on2021geography.csv')
laddata1 <- data1 %>% 
  dplyr::filter(area_name == c('Bolton', 'Waltham Forest', 'Halton'))

#Checking NA 
laddata1[is.na(laddata1)]
#data contains zero NA

#Converting dataframe to wide format
widelad <- laddata1 %>% reshape(timevar = 'metric', v.names = 'value', 
                                idvar = c('date_reported', 'area_name'), direction = 'wide')

#Replacing all NA values with Zero
widelad[is.na(widelad)] <- 0

#Joining data 
joinedlad <- widelad %>% dplyr::left_join(
  data2, by = c('area_code' = 'ï..Code')
)

# Removing prefix 'value' from columns
new_colnames <- gsub("^value.", "", colnames(joinedlad))
names(joinedlad) <- new_colnames

#Joining data 
joinedlad <- joinedlad %>% 
  dplyr::select(-Name) %>% 
  dplyr::rename( Population = All.ages )


#Replacing coma and converting variables to numeric
joinedlad$Population <- as.numeric( gsub(',', '', joinedlad$Population))

#Converting variable 'date_reported' to data format
joinedlad$date_reported <- as.Date(joinedlad$date_reported)

#Creating new variable 'Perc_New_Cases'
joinedlad_w_p <- joinedlad %>% 
  dplyr::mutate(Perc_New_cases =  round( (new_cases/Population) *100, 3) )

joinedlad_w_p$Perc_New_cases <- as.character(paste0(joinedlad_w_p$Perc_New_cases, '%'))

# 
# joinedlad %>% 
#   sort(joinedlad$date_reported)

#Part 2: 2.1
joinedlad_w_p[order(joinedlad_w_p$date_reported),] %>% 
  dplyr::select(date_reported, area_name, new_cases, Perc_New_cases) %>% 
  dplyr::group_by(area_name) %>% 
  head(3) %>% 
  knitr::kable()


#Part 2: 2.2
widen_joinlad <- joinedlad_w_p %>% 
  dplyr::select(date_reported, area_name, new_cases) %>% 
  dplyr::arrange( date_reported) %>% 
  dplyr::group_by(area_name) %>% 
  dplyr::mutate(pre = dplyr::lag(new_cases, 7, default = NA)) %>% 
  dplyr::mutate(pre_pec = ((new_cases - pre)/pre)*100) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(date_reported, area_name, pre_pec) %>% 
  tidyr::pivot_wider(names_from = area_name, values_from =  pre_pec) %>% 
  dplyr::filter(dplyr::between(date_reported, as.Date('2021-07-01'), as.Date('2021-07-31')))


widen_joinlad <- data.frame(widen_joinlad)

widen_joinlad[widen_joinlad == Inf] <- 0
widen_joinlad[is.na(widen_joinlad)] <- 0

widen_joinlad <- data.frame( lapply(widen_joinlad, function(x) round(x, 2)))

for (i in 2:length(names(widen_joinlad))){
  widen_joinlad[,i] = as.character(paste0(widen_joinlad[,i], '%'))
}
widen_joinlad %>% 
  knitr::kable()


#Part 2: 2.3
#Building function for Max of each month
ave_month <- function(LAD){
high_M <- joinedlad %>% 
  dplyr::filter(area_name == LAD) %>% 
  dplyr::select(date_reported, new_first_vaccine_dose) %>% 
  dplyr::slice_max(new_first_vaccine_dose) %>% 
  dplyr::select(date_reported) %>% 
  dplyr::pull(1,1) %>% 
  lubridate::month()

joinedlad %>% 
  dplyr::filter(area_name == LAD) %>% 
  dplyr::filter(lubridate::month(date_reported) == high_M) %>% 
  dplyr::mutate(week_no =lubridate::week(date_reported) ) %>%
  dplyr::group_by(week_no) %>% 
  dplyr::select(new_first_vaccine_dose, date_reported) %>% 
  dplyr::mutate(Weeks = as.character(paste(first(date_reported), 'to', last(date_reported)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Weeks) %>% 
  dplyr::summarise( Average_number_of_first_doses = mean(new_first_vaccine_dose)) %>% 
  dplyr::select(Weeks, Average_number_of_first_doses) %>% 
  knitr::kable()

}  

ave_month('Halton')
ave_month('Waltham Forest') 
ave_month('Bolton') 

#Part 2: 2.3 average daily new first vaccination per week
high_M2 <- joinedlad %>% 
  dplyr::select(date_reported, new_first_vaccine_dose) %>% 
  dplyr::slice_max(new_first_vaccine_dose) %>% 
  dplyr::select(date_reported) %>% 
  dplyr::pull(1,1) %>% 
  lubridate::month()


joinedlad %>% 
  dplyr::filter(lubridate::month(date_reported) == high_M2) %>% 
  dplyr::mutate(week_no =lubridate::week(date_reported) ) %>%
  dplyr::group_by(week_no) %>% 
  dplyr::select(new_first_vaccine_dose, date_reported) %>% 
  dplyr::mutate(Weeks = as.character(paste(first(date_reported), 'to', last(date_reported)))) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(Weeks) %>% 
  dplyr::summarise( Average_number_of_first_doses = mean(new_first_vaccine_dose)) %>% 
  dplyr::select(Weeks, Average_number_of_first_doses) %>% 
  knitr::kable()




#Part 3
boltcomp <- function(x){
  data1 %>% 
    filter(area_name == 'Bolton') %>% 
    filter(metric == x) %>% 
    dplyr::mutate(Month = lubridate::month(date_reported)) %>% 
    dplyr::group_by(Month) %>% 
    dplyr::summarise(mon_max = max(value))
}

englcomp <- function(x){
  data1 %>% 
    filter(metric == x) %>% 
    dplyr::mutate(Month = lubridate::month(date_reported)) %>% 
    dplyr::group_by(Month) %>% 
    dplyr::summarise(mon_max = max(value))
}

boltcomp('cumulative_cases')
englcomp('cumulative_cases')

boltcomp('new_cases')
englcomp('new_cases')
  
boltcomp('new_first_vaccine_dose')
englcomp('new_first_vaccine_dose')

boltcomp('new_second_vaccine_dose')
englcomp('new_second_vaccine_dose')



plot(englcomp('new_cases'), type = 'l', col = 'red',
     main = 'Monthly Maximum New Cases Comparison', xlab= 'Month', 
     ylab = 'New Cases', ylim=c(1, 1700))
lines(boltcomp('new_cases'),  type = "l", col = "green")  
legend("topright",                                       
       legend = c("Bolton", "England"),
       col = c("green", "red"),
       lty = 1)

#The chart shows that England had its highest maximum new cases in January while Bolton had its highest maximum 
#new cases in May. However, both England and Bolton had their lowest maximum new cases on April. This shows 
#that their is high chances that new cases were low across the country in April. Also, England and 
#Bolton had the same new cases trend from January to April. 


plot(englcomp('new_first_vaccine_dose'), type = 'l', col = 'red',
     main = 'Monthly Maximum First Vaccine Comparison', 
     xlab= 'Month', ylab = 'First Vaccine', ylim = c(1, 12000))
lines(boltcomp('new_first_vaccine_dose'),  type = "l", col = "green")  
legend("topright",                                       
       legend = c("Bolton", "England"),
       col = c("green", "red"),
       lty = 1)
#Unlike England which had its highest maximum first vaccine in March, Bolton had its highest maximum 
#first vaccine in May. WHile England had it lowest maximum first vaccine in September, Bolton had its own 
#in October. Both England and Bolton had similar first vaccine trend from October to December. 

plot(englcomp('new_second_vaccine_dose'), type = 'l', col = 'red',
     main = 'Monthly Maximum Second Vaccine Comparison', 
     xlab= 'Month', ylab = 'Second Vaccine', ylim = c(1,11500))
lines(boltcomp('new_second_vaccine_dose'),  type = "l", col = "green")  
legend("topright",                                       
       legend = c("Bolton", "England"),
       col = c("green", "red"),
       lty = 1)


#Both England and Bolton displayed similar Maximum second vaccine trend from January to APril. However, ENgland reached its peak in 
#May while Bolton peaked in April. A fall in trend was also noticed in both from May to December and both reach 
#Their lowest maximum second vaccine in December. 

#Part 4: 4.0
Based on the information offered, it is evident that the data is a UK covid-19 data from 2020-12-06 to 
2021-10-02, containing information about number of new and cummulative covid cases and number of first 
and second vaccins collected by people in different Local Authority District (LAD) of England. For the
purpose of analysis, three LAD: Bolton, Waltham Forest and Halton were assigned to me. 
To efficiently perform this analysis, I used libraries like dplyr, tidyverse and pastec, among others. 

