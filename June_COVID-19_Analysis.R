setwd('C:/Users/dwagn/Downloads/R')
data <- read.csv('owid-covid-data.csv')
library('dplyr')

summary(lm(data$new_cases ~ data$male_smokers + data$female_smokers))
summary(lm(data$new_deaths_per_million ~ data$aged_65_older))
# We see that new deaths per million are strongly correlated with those aged 65 or older


# Will compare USA's Covid-19 new case growth with China's 
# China's data:
China_data <- data %>%
  select(iso_code, date, new_cases, new_cases_per_million) %>%
  filter(iso_code == 'CHN')

#update with descending dates, adds cumulative cases column, adds percent of country infected
class(data$date) #factor
China_data = China_data %>%
  mutate(date = as.Date(date),
         'Cumulative_New_Cases' = cumsum(new_cases), 
         'Percent_Infected' = (cumsum(new_cases_per_million)/10000)) %>%
  # or China_data[,"Cumulative New Cases"] <- cumsum(China_data$new_cases)
  arrange(date)

# USA data:
USA_data <- data %>%
  select(iso_code, date, new_cases, new_cases_per_million) %>%
  filter(iso_code == 'USA')

USA_data = USA_data %>%
  mutate(date = as.Date(date),
         'Cumulative_New_Cases' = cumsum(new_cases),
         'Percent_Infected' = cumsum(new_cases_per_million/10000)) %>%
  arrange(date)

# USA_and_China_data = China_data + USA_data


# New cases by day in China and USA
library(ggplot2)
plot1 = ggplot() + 
  ggtitle('New Cases in China vs. USA') +
  geom_line(data = USA_data, aes(x = date, y = new_cases, color='a'), color = "blue") + 
  geom_line(data = China_data, aes(x = date, y = new_cases), color = "red") + 
  labs(x = 'Date',
       y = 'Cumulative Cases',
       color = 'Legend')
  
  #xlab('Date') + 
  #ylab('Cases by Day')
plot1

#Cumulative cases in China and  USA
plot2 = ggplot() +
  geom_line(data = USA_data, aes(x = date, y = Cumulative_New_Cases), color = "blue") +
  geom_line(data = China_data, aes(x= date, y = Cumulative_New_Cases), color = "red") + 
  xlab('Date') + 
  ylab('Cumulative Cases')
plot2


# Let's check the percantage infected in each country
June3data = data %>%
  select(iso_code, location, date, total_cases, population) %>%
  filter(date == '2020-06-03') %>%
  mutate('Percent_of_Country_Infected' = (total_cases/population)*100)

# Percent infected by population
# It looks as though Qatar and San Marino have the highest infection rate:population ratio, followed by Vatican and Andorra
# India and China appear to have the lowest Infection:population rate
plot3 = ggplot(data = June3data, aes(population, Percent_of_Country_Infected)) +
  geom_point() + 
  geom_text(size = 3, angle=-45, data=subset(June3data, Percent_of_Country_Infected > 1 | population > 1e+09),
            aes(population, Percent_of_Country_Infected, label=location)) +
  theme(text = element_text(size=12)) +
  xlab('Population') + ylab('Percent Infected')+
  xlim(0, 1.75e+09)
plot3


  
