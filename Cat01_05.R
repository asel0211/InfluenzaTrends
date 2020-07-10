library(fpp3)
library(readr)
library(tsibble)
library(lubridate)
library(tidyverse)

## Import the file ####
# Note: 1 - resuscitation, 2 - emergency, 3 - urgent, 4 - semi-urgent, 5 - non-urgent
Cat01_05 <- read_csv("C:/Users/Andy/Dropbox/Research Ouput/Time series analysis of respiratory presentations to the Emergency Department/Data Update March 2020/Cat01-05.csv")
Cat01_05$Date <- as.Date(Cat01_05$Date, "%d-%m-%y")
Cat01_05$Date <- yearmonth(Cat01_05$Date)
Cat01_05$Category <- factor(Cat01_05$Category, levels=c("1","2","3","4","5"))
Cat01_05$Diagnosis <- factor(Cat01_05$Diagnosis, levels=c("B349","R509","J22","J069","R05","J111"))
Cat01_05

# Count numbers
Cat01_05 %>% group_by(Category) %>%
  summarize(Count = sum(Count))
Cat01_05 %>% group_by(Diagnosis) %>%
  summarize(Count = sum(Count))

# Prepare subgroups
    # Total count
    Cat01_05Total <- Cat01_05 %>% group_by(Date) %>%
      summarize(Count = sum(Count))
      Cat01_05Total
    
    # Total count by category
    Cat01_05Cat <- Cat01_05 %>% group_by(Date, Category) %>%
      summarize(Count = sum(Count))
      Cat01_05Cat
      
    # Total count by diagnosis
      Cat01_05Dis <- Cat01_05 %>% group_by(Date, Diagnosis) %>%
        summarize(Count = sum(Count))
      Cat01_05Dis
  
      
      
# Bar plots ####
      plot(Cat01_05$Category)
      plot(Cat01_05$Diagnosis)
      plot(Cat01_05$Diagnosis ~ Cat01_05$Category, xlab="Triage category", ylab="ICD-10 Code") # Favourite
      plot(Cat01_05$Category ~ Cat01_05$Diagnosis)
   
      
         
## Total count ####
      # Coerce into a tsibble
      Total <- as_tsibble(Cat01_05Total)
      Total <- fill_gaps(Total, .full=TRUE)
      Total
      
      # Check accuracy
      train1 <- Total %>%
        filter_index(~"2019-3")
      
      fit1 <- train1 %>%
        model(
          snaive = SNAIVE(Count ~ lag("year")), # Random walk model
          auto_ets = ETS(Count), # Exponential smoothing model
          arima = ARIMA(Count), # ARIMA model
          neural = NNETAR(Count) # Neural network
          
       #  ) %>% 
       #  mutate(
       #   average = (snaive + auto_ets + arima) / 3
        )
      fit1
      
      fc1 <- fit1 %>% forecast(h = 12)
      
      fc_accuracy1 <- accuracy(fc1, Total)
      fc_accuracy1
      
      # Forecast by total count
      Total %>% autoplot(Count)
      
      fit1 <- Total %>%
        model(
          snaive = SNAIVE(Count ~ lag("year")),
          auto_ets = ETS(Count),
          arima = ARIMA(Count),
        #  neural = NNETAR(Count),
        # ) %>% 
        # mutate(
        #   average = (snaive + auto_ets + arima) / 3
        )
      fit1
      
      fc1 <- fit1 %>%
        forecast(h = 24)
      print(hilo(fc1, level = 95), n=96)
      
      fc1 %>%
        autoplot(Total, level = NULL) +
        ggtitle("Forecasts by total count") +
        xlab("Year") +
        guides(colour = guide_legend(title = "Forecast"))
      
      
      
## By category ####
# Coerce into a tsibble and fill gaps
Cat <- Cat01_05Cat %>%
    select(-Date) %>%
    as_tsibble(key = Category, index = Date)
    Cat <- fill_gaps(Cat, .full=TRUE)
Cat <- ungroup(Cat)

# Check accuracy
train2 <- Cat %>%
  filter_index(~"2019-3")

fit2 <- train2 %>%
  model(
    snaive = SNAIVE(Count ~ lag("year")), # Random walk model
    auto_ets = ETS(Count), # Exponential smoothing model
    arima = ARIMA(Count), # ARIMA model
    neural = NNETAR(Count), # Neural network
  # ) %>% 
  # mutate(
  #  average = (snaive + auto_ets + arima) / 3
   )
fit2

fc2 <- fit2 %>% forecast(h = 12)

fc_accuracy2 <- accuracy(fc2, Cat)
filter(fc_accuracy2, Category == 1) # SNaive
filter(fc_accuracy2, Category == 2) # ARIMA
filter(fc_accuracy2, Category == 3) # ETS
filter(fc_accuracy2, Category == 4) # ETS
filter(fc_accuracy2, Category == 5) # ETS


# Forecast by category
Cat %>% autoplot(Count) +
  ggtitle("Presentations by category") +
  xlab("Year")

fit2 <- Cat %>%
  model(
    snaive = SNAIVE(Count ~ lag("year")),
    auto_ets = ETS(Count),
    arima = ARIMA(Count),
  #  neural = NNETAR(Count),
  # ) %>% 
  # mutate(
  #  average = (snaive + auto_ets + arima) / 3
  )
fit2

fc2 <- fit2 %>%
  forecast(h = 24)
print(hilo(fc, level = 95), n=480)

fc2 %>%
  autoplot(Cat, level = NULL) +
  ggtitle("Forecasts by category") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))


## By diagnosis ####
# Coerce into a tsibble and fill gaps
Dis <- Cat01_05Dis %>%
  select(-Date) %>%
  as_tsibble(key = Diagnosis, index = Date)
Dis <- fill_gaps(Dis, .full=TRUE)
Dis <- ungroup(Dis)

# Check accuracy
train3 <- Dis %>%
  filter_index(~"2019-3")

fit3 <- train3 %>%
  model(
    snaive = SNAIVE(Count ~ lag("year")), # Random walk model
    auto_ets = ETS(Count), # Exponential smoothing model
    arima = ARIMA(Count), # ARIMA model
    neural = NNETAR(Count), # Neural network
    # ) %>% 
    # mutate(
    #  average = (snaive + auto_ets + arima) / 3
  )
fit3

fc3 <- fit3 %>% forecast(h = 12)

fc_accuracy3 <- accuracy(fc3, Dis)
filter(fc_accuracy3, Diagnosis == 'B349') # ARIMA
filter(fc_accuracy3, Diagnosis == 'J069') # ETS
filter(fc_accuracy3, Diagnosis == 'J111') # ARIMA
filter(fc_accuracy3, Diagnosis == 'J22') # ETS
filter(fc_accuracy3, Diagnosis == 'R05') # ARIMA
filter(fc_accuracy3, Diagnosis == 'R509') # ETS


# Forecast by diagnosis
Dis %>% autoplot(Count) +
  ggtitle("Presentations by diagnosis") +
  xlab("Year")


fit3 <- Dis %>%
  model(
    # snaive = SNAIVE(Count ~ lag("year")),
    auto_ets = ETS(Count),
    arima = ARIMA(Count),
    # neural = NNETAR(Count),
    # ) %>% 
    # mutate(
    #  average = (snaive + auto_ets + arima) / 3
  )
fit3

fc3 <- fit3 %>%
  forecast(h = 24)
print(hilo(fc3, level = 95), n=288)
print(filter(fc3, Diagnosis == 'B349', .model == 'arima'), n=24)


fc3 %>%
  autoplot(Dis, level = NULL) +
  ggtitle("Forecasts by diagnosis") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))



## By Age ####
AdultPaed <- read_csv("C:/Users/Andy/Dropbox/Research Ouput/Time series analysis of respiratory presentations to the Emergency Department/Data Update March 2020/AdultPaed.csv")
AdultPaed$Date <- as.Date(AdultPaed$Date, "%d-%m-%y")
AdultPaed$Date <- yearmonth(AdultPaed$Date)
AdultPaed$Age <- factor(AdultPaed$Age, levels=c("A","P"))
AdultPaed

# Count numbers
AdultPaed %>% group_by(Age) %>%
  summarize(Count = sum(Count))
AdultPaed %>% group_by(Diagnosis) %>%
  summarize(Count = sum(Count))

# Prepare subgroups
# Total count by age
AdultPaedAge <- AdultPaed %>% group_by(Date, Age) %>%
  summarize(Count = sum(Count))
AdultPaedAge


# Plots ####
plot(AdultPaed$Count ~ AdultPaed$Age)


## Total count ####
# Coerce into a tsibble
Age <- AdultPaedAge %>%
  select(-Date) %>%
  as_tsibble(key = Age, index = Date)
Age <- fill_gaps(Age, .full=TRUE)
Age <- ungroup(Age)

# Check accuracy
train3 <- Age %>%
  filter_index(~"2019-3")

fit3 <- train3 %>%
  model(
    snaive = SNAIVE(Count ~ lag("year")), # Random walk model
    auto_ets = ETS(Count), # Exponential smoothing model
    arima = ARIMA(Count), # ARIMA model
    neural= NNETAR(Count)
  # ) %>% 
  # mutate(
  #   average = (snaive + auto_ets + arima) / 3
  )
fit3

fc3 <- fit3 %>% forecast(h = 12)

fc_accuracy3 <- accuracy(fc3, Age)
filter(fc_accuracy3, Age=='A')
filter(fc_accuracy3, Age=='P')

# Forecast by age
Age %>% autoplot(Count) +
  ggtitle("Presentations by age") +
  xlab("Year")

fit3 <- Age %>%
  model(
    snaive = SNAIVE(Count ~ lag("year")),
    auto_ets = ETS(Count),
    arima = ARIMA(Count),
    neural = NNETAR(Count),
  # ) %>% 
  # mutate(
  # average = (snaive + auto_ets + arima) / 3
  )
fit3

fc3 <- fit3 %>%
  forecast(h = 24)
print(hilo(fc3, level = 95), n=480)

fc3 %>%
  autoplot(Age, level = NULL) +
  ggtitle("Forecasts by age") +
  xlab("Year") +
  guides(colour = guide_legend(title = "Forecast"))
