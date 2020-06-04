setwd('C:/Users/dwagn/Downloads/R')
data <- read.csv('owid-covid-data.csv')
library('dplyr')

summary(lm(data$new_cases ~ data$male_smokers + data$female_smokers))
summary(lm(data$new_deaths_per_million ~ data$aged_65_older))
# We see that new deaths are strongly correlated with those aged 65 or older


# Will compare USA's Covid-19 new case growth with China's 
# China's data:
China_data <- data %>%
  select(iso_code, date, new_cases, new_cases_per_million) %>%
  filter(iso_code == 'CHN')

#update with descending dates, adds cumulative cases column, adds percent of country infected
class(data$date) #factor
China_data = China_data %>%
  mutate(date = as.Date(date),
         'Cumulative New Cases' = cumsum(new_cases), 
         'Percent infected' = (cumsum(new_cases_per_million)/10000)) %>%
  # or China_data[,"Cumulative New Cases"] <- cumsum(China_data$new_cases)
  arrange(date)

# USA data:
USA_data <- data %>%
  select(iso_code, date, new_cases, new_cases_per_million) %>%
  filter(iso_code == 'USA')

USA_data = USA_data %>%
  mutate(date = as.Date(date),
         'Cumulative New Cases' = cumsum(new_cases),
         'Percent infected' = cumsum(new_cases_per_million/10000)) %>%
  arrange(date)



class(China_data$date)

