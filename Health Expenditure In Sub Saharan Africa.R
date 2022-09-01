#Analyzing Health Expenditure In Sub-Saharan Africa


# load dependencies and prepare the working directory
setwd("C:/Users/pc/Downloads/Github/Healthcare Expenditure")

library(ggthemes)
library(readr)
library(dplyr)
library(lubridate)
library(skimr)
library(psych)
library(tidyverse)
library(ggplot2)
library(ggthemes)

# read in files ---> format comma separated values
df <- read.csv("Current_health_expenditure.csv")  


#Data cleansing and data preparation for exploratory analysis

#remove redundant column and rows
df = select(df, -c(X))  
df = df[-c(1),]

# pivot1 --> convert wide form data to long form data
health_expense <- df %>%
  pivot_longer(df$Indicators, cols = c('X2000' : 'X2019'),  
               names_to = "yearly_health_expense",
               values_to = "health_expenditure") 

#format data frame 
health_expense$health_expenditure <- as.numeric(gsub( "," , "", health_expense$health_expenditure))
health_expense$yearly_health_expense  <-  substr(health_expense$yearly_health_expense, 2, 5)
health_expense$Indicators <- substr(health_expense$Indicators, 1, 26)
health_expense$yearly_health_expense <- as.Date(as.character(health_expense$yearly_health_expense), format = "%Y")


skim(health_expense)  

#Due to data skewness, replace missing values with the median
health_expense <- health_expense %>% 
  mutate(health_expenditure = ifelse(is.na(health_expenditure),
                                     median(health_expenditure, na.rm = T),    # logical function that checks for na's in the variable health_exp, if found, replaces with median and removes the na, Else returns health_exp
                                     health_expenditure))

# health_expense --> Make a subregional category column that maps each country to its region 
source("Region.R")
health_expense <-health_expense %>%
  mutate(
    Sub_reg_grp = case_when(
      Countries %in% Central ~ 'Central Africa',
      Countries %in% Eastern ~ 'Eastern Africa',  
      Countries %in% Southern ~ 'Southern Africa',
      TRUE ~ 'Western Africa'
    )
  )


#Get all unique records
health_expense <- unique(health_expense, incomparables = F, fromLast = F)  

#Exploratory data analysis

#explore all countries present in the data frame
unique(health_expense$Countries) # there 47 unique Countries


#summarization by country
Country_summary <- health_expense %>%
  group_by(Countries) %>%
  summarise(Total_health_expense = sum(health_expenditure)) %>%
  mutate(percentage_expenditure = Total_health_expense / sum(Total_health_expense) * 100)

Country_summary %>%
  arrange(desc(Total_health_expense))   #The highest spending countries, Seychelles and South Africa, make up 12.9% and 10.7% respectively of total current health expenditures.
Country_summary %>%
  arrange(Total_health_expense)                                     


#Subset the countries with health expenditures greater than $7000k and call them Top Spenders
Top_spenders <- Country_summary %>%    
  group_by(Countries) %>%
  select(Countries, Total_health_expense, percentage_expenditure) %>%
  summarise(Total_health_expense = Total_health_expense / 1000)%>%
  filter(Total_health_expense  >= 7) #In order of spending, Seychelles, South Africa, Namibia, Mauritius, Botswana rank highest.They make up 49.32 % of health expenditure.

#In order of spending, Seychelles, South Africa, Namibia, Mauritius, Botswana rank highest.They make up 49.32 % of health expenditure.


#Subset the countries with health expenditures greater than $500k and call them Bottom Spenders
Bottom_spenders <- Country_summary %>%    
  group_by(Countries) %>%
  select(Countries, Total_health_expense, percentage_expenditure) %>%
  summarise(Total_health_expense = Total_health_expense / 100)%>%
  filter(Total_health_expense <5)

Bottom_spenders %>%
  arrange(desc(Total_health_expense))  # 7 out of 47 countries have current health expenditures under 500 million dollars. They make up 2.77 % of health expenditure.


# What region  does these bottom spenders belong?
source("Region.R")
reg_bottom_spenders <-Bottom_spenders %>%
  mutate(
    Sub_reg_grp = case_when(
      Countries %in% Central ~ 'Central Africa',
      Countries %in% Eastern ~ 'Eastern Africa',  # For carrying sub-Regional classification.
      Countries %in% Southern ~ 'Southern Africa',
      TRUE ~ 'Western Africa'
    )
  )

# take a count of the countries mapped to the regional slackers
reg_bottom_spenders_count <- reg_bottom_spenders %>%
  group_by( Sub_reg_grp) %>%
  summarise(regional_frequency = n()) %>%                     #Western Africa accounts for 3 out of 7 Bottom spenders
  mutate(regional_frequency / sum(regional_frequency) * 100) 


#summarization by region
regional_summary <- health_expense %>%
  group_by(Sub_reg_grp) %>%
  summarise(Total_health_expense = sum(health_expenditure)) %>%
  mutate(percentage_expenditure = Total_health_expense / sum(Total_health_expense) * 100)


  regional_summary %>%
  arrange(desc(Total_health_expense))  # Southern African countries spend the most on health, with 39.8% of the total, followed by Western African countries with 31.5 %, then Eastern and Central Africa Countries with share of health expenditure of 26.5 % and 5.12% respectively.


# summarize by year_health_expenditure
yearly_summary <- health_expense %>%
  group_by(yearly_health_expense) %>%
  summarise(Total_health_expense = sum(health_expenditure))%>%
  mutate(yearly_percentage = Total_health_expense/ sum(Total_health_expense) * 100) #The government's health expenditures have increased steadily by 13.8% between 2000 and 2010, but have fluctuated in recent years.

# calculate year over year percentage change
yearly_exp_change <-yearly_summary %>%                     
  mutate(pre_exp = lag(Total_health_expense),          #The Government's health expenditures are down -4.14 % from 2018 to 2019.
         Change_exp = Total_health_expense - pre_exp,
         ChangePercent = (Change_exp/pre_exp) * 100)


#Data visualization -- Countries whose health expenditure >= $7000k
Top_spenders %>%
  group_by(Countries, Total_health_expense) %>%
  summarise(total = Total_health_expense)%>%
  ggplot(aes(x = Countries, y = Total_health_expense, width = 0.5, fill =  Countries)) +
  labs(title = " Government Health Expenditure",
       subtitle = " Top five Countries whose health spending between 2000-2019 exceeds $7M",
       y = "Health Expenditure")+
scale_y_continuous(labels=scales::dollar_format())+
  geom_bar(stat= "identity") +
  scale_fill_manual(values = c("Seychelles" = "#8D230F",
                               "South Africa" ="#07575B",
                               "Namibia" = "#66A5AD",
                               "Mauritius" = "#C4DFE6",
                               "Botswane" = "#B7B8B6")) +
  theme_minimal()
  


#Data Visualization -- Countries whose health expenditure is less than $500k
Bottom_spenders %>%
  group_by(Countries, Total_health_expense) %>%
  summarise(Total_health_expense)%>%
  ggplot(aes(x = Countries, y = Total_health_expense, width = 0.3, fill = Countries)) +
  labs(title = " Government Health Expenditure",
       subtitle = "Countries whose health spending is below $5M between 2000-2019",
       y = "Health Expenditure") +
  scale_y_continuous(labels=scales::dollar_format())+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Burundi" = "#0F0F0F",
                               "Central African Republic" ="#636363",
                               "Eritrea" = "#8C979A",
                               "Ethiopia" = "#B8B8B8",
                               "Madagascar" = "#4D4C5C",
                               "Niger" = "#333333",
                               "Democratic Republic of the Congo" = "#900D09")) +
  theme_minimal() 
  


#Data Visualization -- Health Expenditure trend
yearly_summary %>%
  group_by(yearly_health_expense) %>%
  summarise(yearly_percentage)%>%
  ggplot( aes(x = yearly_health_expense, y = yearly_percentage)) +
  labs(title = "Health Expenditure trends",
       x = "year",
       y = "percentage Health Expenditure") +
  geom_line()+
  geom_smooth() +
  theme_minimal()
  

#data Visualization -- year over year health Expenditure 
yearly_exp_change %>%
  group_by(yearly_health_expense) %>%
  summarise(ChangePercent) %>%
  ggplot(aes(x = yearly_health_expense, y = ChangePercent)) +
  labs(title = "Year over year  Health Expenditure trends",
       x = "year",
       y = "percentage Health expenditure")+
  geom_line()+
  geom_smooth() +
  theme_minimal()
  

#Data visualization -- Health expenditure trends by Sub-Regional Category
regional_summary2 <- health_expense %>%
  group_by(Sub_reg_grp, yearly_health_expense) %>%
  select(yearly_health_expense, health_expenditure)%>%
  summarise( Avg_health_expenditure = mean(health_expenditure) / 100)

ggplot(regional_summary2, aes(x = yearly_health_expense, y = Avg_health_expenditure, color = Sub_reg_grp))+  # Eastern African countries are projecting an increase in health spending, whereas Southern African countries are projecting a sharp fall, Central and Western African countries are projecting a steady decline in health spending. 
  labs(title = " Trends in Average Health Expenditure by Sub-region, In Million Current  USD")+
  geom_line()+
  geom_smooth()+
  facet_wrap(~ Sub_reg_grp)+ 
  scale_y_continuous(labels=scales::dollar_format())+
  theme_minimal()




