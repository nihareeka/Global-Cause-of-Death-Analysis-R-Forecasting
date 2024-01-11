library(data.table)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(tibble)
library(tsibble)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggfortify)
library(caret)
library(tibble)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(ggplot2)
library(forecast)
library(ggfortify)
library(caret)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(tsibbledata)
library(data.table)
# STEP 1: Load the data

# Load deaths due to all causes data
all_causes.df <- fread("all_causes.csv")

# Load non_communicable deaths data
non_com.df <- fread("non_communicable.csv")

# Load communicable deaths data
com.df <- fread("communicable.csv")

# Load the global health expenditure data
global_exp.df <- fread("global_expenditure.csv")


# STEP 2: Basic Statistics of the data
str(all_causes.df)
str(com.df)
str(non_com.df)
str(global_exp.df)

summary(all_causes.df)
summary(com.df)
summary(non_com.df)
summary(global_exp.df)

# STEP 3: Data Cleaning
all_causes.df <- subset(all_causes.df, select = -c(1,10))
non_com.df <- subset(non_com.df, select = -c(1,11))
com.df <- subset(com.df, select = -c(1,11))
names(global_exp.df)[2] <- "country_code"

# STEP 4: Merging the datasets
merged_df <- merge(com.df, non_com.df, 
                   by = c("region_name","country_code",
                          "country_name" ,"year","sex",
                          "age_group_code","age_group" ), all = FALSE)
merged_df <- merge(merged_df,all_causes.df, 
                   by = c("region_name","country_code",
                          "country_name" ,"year","sex",
                          "age_group_code","age_group"), all = FALSE)


summary(merged_df)

null_count <- colSums(is.na(merged_df))
print(null_count)

# Remove Null values

merged_df <- na.omit(merged_df)
null_count <- colSums(is.na(merged_df))
print(null_count)
print(dim(merged_df))

unique(merged_df$sex)
unique(merged_df$age_group_code)

# Filter required data

death_counts_data.df <- merged_df %>% 
  filter(sex %in% c("Male","Female") &
           !age_group_code %in% c( "Age_all"))
death_counts_data.df <- subset(death_counts_data.df, select = -c(7))
names(death_counts_data.df)[3] <- "country"


# Convert required variables to categorical

cols <- c('country','country_code','region_name','sex','age_group_code')
death_counts_data.df <- death_counts_data.df %>% mutate_at(cols, as.factor)
str(death_counts_data.df)
summary(death_counts_data.df)
print(dim(death_counts_data.df))

cols <- c('country','country_code','region','income')
global_exp.df <- global_exp.df %>% mutate_at(cols, as.factor)
global_exp.df <- na.omit(global_exp.df)

# Insights

# Insight 1 : 

total_deaths_by_year <- death_counts_data.df %>% 
  group_by(year, region_name,sex) %>%
  summarize(deaths = sum(total_deaths))

# ggplot(total_deaths_by_year, aes(x = year, y = deaths, color = sex)) +
#   geom_line() +
#   facet_wrap(~ region_name, scales = "free_x") +
#   labs(title = "Total Deaths by Year and Region", x = "Year and Region", y = "Total Deaths", color = "Sex")

# ggplot(total_deaths_by_year, aes(x = year, y = deaths, fill = sex)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~ region_name, scales = "free_x") +
#   labs(title = "Total Deaths by Year and Region", x = "Year and Region", y = "Total Deaths", fill = "Sex")


# Insight 2 :
europe.df <- death_counts_data.df %>%
  filter(region_name == "Europe") %>%
  group_by(year, region_name) %>%
  summarize(tot_com_deaths = sum(com_deaths),
            tot_non_com_deaths =sum(non_com_deaths))


europe_long <- europe.df %>%
  pivot_longer(cols = c("tot_com_deaths", "tot_non_com_deaths"), names_to = "cause_of_death", values_to = "total_deaths")

# Create stacked area chart
# ggplot(europe_long, aes(x = year, y = total_deaths, fill = cause_of_death, group = cause_of_death)) +
#   geom_area(alpha = 0.6) +
#   labs(title = "Deaths in Europe", x = "Year", y = "Total Deaths in Millions", fill = "Cause of Death") +
#   theme(legend.position = "bottom")


# Insight 3 :

age_death_count.df <- death_counts_data.df %>%
  group_by(age_group_code) %>%
  summarize(tot_deaths = sum(total_deaths))

# plot_ly(age_death_count.df, labels = ~age_group_code,
#         values = ~tot_deaths, 
#         type = "pie") %>%
#   layout(title = "Total Death Count by Age Group")


# Insight 4 :

unique(global_exp.df$country)

avg_health_budget.df <- global_exp.df %>%
  filter(region == "EUR") %>%
  select(year, region, country, che_gdp) %>%
  group_by(country) %>%
  summarize(che_gdp_avg = mean(che_gdp)) %>%
  filter(!che_gdp_avg == 0)  

# ggplot(avg_health_budget.df, aes(x = country, y = che_gdp_avg)) +
#   geom_col() +
#   labs(x = "Country", y = "Avg Health Exp as % of GDP") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   geom_hline(yintercept = 6.5, color = "red")


#########################################################################
# Subset the required data
# we choose USA and Brazil for our further analysis
# we only consider 1990 - 2019 data for our analysis

death_counts_data.df <- subset(death_counts_data.df, select = -c(2))
print(names(death_counts_data.df))
names(death_counts_data.df) <- c("region_name",  "country", "year",  "sex", "age_group_code", "com_deaths", "%_of_com_deaths", "com_death_rate", 
                                 "non_com_deaths", "%_of_non_com_deaths", "non_com_death_rate", 
                                 "total_deaths",  "total_death_rate")

# Label encode the gender
death_counts_data.df$sex <- ifelse(death_counts_data.df$sex == "Male", 0,1)

# Encode the age group data
age_levels <- c("Age00","Age01_04","Age05_09","Age10_14","Age15_19",  "Age20_24","Age25_29","Age30_34","Age35_39","Age40_44","Age45_49",
                "Age50_54","Age55_59","Age60_64","Age65_69","Age70_74","Age75_79",
                "Age80_84","Age85_over")

age_nums <- c(0,1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)
lookup_table <- setNames(age_nums, age_levels)
death_counts_data.df$age_nums <- 
  lookup_table[match(death_counts_data.df$age_group_code, age_levels)]

death_counts_data.df <- subset(death_counts_data.df, select = -c(5))

death_counts_data.df <- death_counts_data.df[, c(1,2,3,4,13,5:12)]


usa_ts <- death_counts_data.df %>%
  filter(country == "United States of America") %>%
  filter(year >1989 & year < 2020) %>%
  select(year, com_deaths,com_death_rate,non_com_deaths, non_com_death_rate, total_deaths, total_death_rate) %>%
  group_by(year) %>%
  summarise(total_non_com_deaths = sum(non_com_deaths),
            total_com_deaths = sum(com_deaths),
            total_com_death_rate = mean(com_death_rate),
            total_non_com_death_rate = mean(non_com_death_rate),
            total_deaths = sum(total_deaths),
            total_death_rate = mean(total_death_rate))


usa_ts %>% select(year, total_non_com_death_rate) %>%
    tsibble(index=year) ->
    usa_ts

autoplot(usa_ts,total_non_com_death_rate) + xlab("Year")+
  ylab("Death Rate") + ggtitle("Time Series of Death Rate")

# Split the data into train and test
usa_train_df <- usa_ts %>% filter(year<2016) 
usa_test_df <- usa_ts %>% filter(year>=2016)

#----------------------------------------------
# Naive, Drift, Mean Models
#----------------------------------------------
# Fit the Naive, Drift, and Mean models on the training data
model_fit <-  usa_train_df %>%
  model(
    Naive = NAIVE(total_non_com_death_rate),
    Drift = RW(total_non_com_death_rate ~ drift()),
    Mean = MEAN(total_non_com_death_rate)  
  )

# Forecast the death rate for 2016-2019
forecast <- model_fit %>%
  forecast(h = 4)

# Plot the forecasted data
forecast %>%  autoplot(usa_ts, level = NULL) +
  labs(title = "Total Non Com Death rate",
       y = "Death rate") +
  guides(colour = guide_legend(title = "Forecast"))

# Compute the accuracy metrics
accuracy_metrics_naive <- forecast %>%
  accuracy(usa_ts)
accuracy_metrics_naive

#----------------------------------------------
# ETS Model
#----------------------------------------------
# Fit the ets model
ets_fit <- usa_train_df %>% 
  model(AN = ETS(total_non_com_death_rate ~ error("A") + trend("N")))
report(ets_fit)

#Forecast
forecast_ets <- ets_fit %>% 
  forecast(h = 4)


autoplot(forecast_ets,color="blue") +
  autolayer(usa_ts, color="black") +
  labs(title = "Forecasting total non com death rate in USA using ETS model",
       y = "Total death rate") +
  guides(colour = guide_legend(title = "Forecast"))

tidy(ets_fit)


accuracy_metrics_ets <- forecast_ets %>%
  accuracy(usa_ts)
accuracy_metrics_ets

#----------------------------------------------
# ARIMA Model
#----------------------------------------------
# 
model1 <- arima(usa_train_df$total_non_com_death_rate, order = c(0,1,2))
model2 <- arima(usa_train_df$total_non_com_death_rate, order = c(2,1,0))
model3 <- arima(usa_train_df$total_non_com_death_rate, order = c(1,0,0))
model4 <- auto.arima(usa_train_df$total_non_com_death_rate, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, lambda = "auto")
# 
AIC(model1, model2, model3, model4)

model4

summary(model4)

forecast_auto_arima <- model4 %>%
  forecast(h = 4)

plot(forecast_auto_arima, ylab = "total net generation of electricity by the U.S.")
lines(ma(usa_ts, 4), col = "red")


#----------------------------------------------
# Linear Regression - 1
#----------------------------------------------
# Given the year, sex and population - we're trying to predict non_com_death_rate

death_counts_data.df <- death_counts_data.df %>% mutate(total_population = (total_deaths*100000)/total_death_rate)

usa_new <- death_counts_data.df %>%
  filter(country == "United States of America") %>%
  filter(year<2020) %>%
  select(year, sex, age_nums,com_deaths,com_death_rate,non_com_deaths, non_com_death_rate, total_deaths, total_death_rate, total_population) %>%
  group_by(year,sex) %>%
  summarise(total_non_com_deaths = sum(non_com_deaths),
            total_com_deaths = sum(com_deaths),
            total_com_death_rate = mean(com_death_rate),
            total_non_com_death_rate = mean(non_com_death_rate),
            total_deaths = sum(total_deaths),
            total_death_rate = mean(total_death_rate),
            population = sum(total_population))

selected.var = c(1,2,6,9)

set.seed(1)
train.index <- sample(c(1:dim(usa_new)[1]),dim(usa_new)[1]*0.8)  # 80% of dataset
train <- usa_new[train.index,selected.var]
valid <- usa_new[-train.index,selected.var]

linear.fit.usa <- lm(total_non_com_death_rate ~ ., data = train)
summary(linear.fit.usa)
par(mfrow = c(2, 2))
plot(linear.fit.usa)

usa.death.pred <- predict(linear.fit.usa, valid)
residuals <- valid$total_non_com_death_rate - usa.death.pred
lr_results_usa <- data.frame('Year' = valid$year,'Predicted' = usa.death.pred, 'Actual' = valid$total_non_com_death_rate,
                             'Residual' = residuals)

lr_results_usa


lr_accuracy_usa <- data.frame(Model = "Linear Regression",
                              MAPE = mean(abs((valid$total_non_com_death_rate - usa.death.pred)/valid$total_non_com_death_rate))*100,
                              RMSE = sqrt(mean((valid$total_non_com_death_rate - usa.death.pred)^2))
)
lr_accuracy_usa
lr_metrics <- postResample(pred = usa.death.pred, obs = valid$total_non_com_death_rate)

lr_metrics

ggplot(lr_results_usa, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "LR Non Com Death Rate - USA", color = "Line")

#----------------------------------------------
# Ridge Regression - 1
#----------------------------------------------
ridge.fit.usa <- train(total_non_com_death_rate ~ ., data = train, method = "ridge", trControl = trainControl(method = "cv",number=10))
ridge.fit.usa

usa.death.pred <- predict(ridge.fit.usa, newdata = valid)
residuals <- valid$total_non_com_death_rate - usa.death.pred
ridge_results_usa <- data.frame('Year' = valid$year,'Predicted' = usa.death.pred, 'Actual' = valid$total_non_com_death_rate,
                                'Residual' = residuals)

ridge_results_usa


ridge_accuracy_usa <- data.frame(Model = "Ridge Regression",
                                 MAPE = mean(abs((valid$total_non_com_death_rate - usa.death.pred)/valid$total_non_com_death_rate))*100,
                                 RMSE = sqrt(mean((valid$total_non_com_death_rate - usa.death.pred)^2))
)
ridge_accuracy_usa
ridge_metrics <- postResample(pred = usa.death.pred, obs = valid$total_non_com_death_rate)

ridge_metrics
ggplot(ridge_results_usa, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "Ridge Non Com Death Rate - USA", color = "Line")

#----------------------------------------------
# Lasso Regression - 1
#----------------------------------------------
lasso.fit.usa <- train(total_non_com_death_rate ~ ., data = train, method = "glmnet", trControl = trainControl(method = "cv",number=10))
lasso.fit.usa

usa.death.pred <- predict(lasso.fit.usa, newdata = valid)

residuals <- valid$total_non_com_death_rate - usa.death.pred
lasso_results_usa <- data.frame('Year' = valid$year,'Predicted' = usa.death.pred, 'Actual' = valid$total_non_com_death_rate,
                                'Residual' = residuals)

lasso_results_usa


lasso_accuracy_usa <- data.frame(Model = "Lasso Regression",
                                 MAPE = mean(abs((valid$total_non_com_death_rate - usa.death.pred)/valid$total_non_com_death_rate))*100,
                                 RMSE = sqrt(mean((valid$total_non_com_death_rate - usa.death.pred)^2))
)
lasso_accuracy_usa
lasso_metrics <- postResample(pred = usa.death.pred, obs = valid$total_non_com_death_rate)

lasso_metrics
ggplot(lasso_results_usa, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "Lasso Non Com Death Rate - USA", color = "Line")

#----------------------------------------------
# PCR - 1
#----------------------------------------------

pcr.fit.usa <- train(total_non_com_death_rate ~ ., data = train, method = "pcr", preProcess = c("center", "scale"), tuneLength = 10, trControl = trainControl(method = "cv", number = 10))
pcr.fit.usa

usa.death.pred <- predict(pcr.fit.usa, newdata = valid)
residuals <- valid$total_non_com_death_rate - usa.death.pred
pcr_results_usa <- data.frame('Year' = valid$year,'Predicted' = usa.death.pred, 'Actual' = valid$total_non_com_death_rate,
                              'Residual' = residuals)

pcr_results_usa


pcr_accuracy_usa <- data.frame(Model = "PCR",
                               MAPE = mean(abs((valid$total_non_com_death_rate - usa.death.pred)/valid$total_non_com_death_rate))*100,
                               RMSE = sqrt(mean((valid$total_non_com_death_rate - usa.death.pred)^2))
)
pcr_accuracy_usa
pcr_metrics <- postResample(pred = usa.death.pred, obs = valid$total_non_com_death_rate)

pcr_metrics
ggplot(pcr_results_usa, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "PCR Non Com Death Rate - USA", color = "Line")


#######################
# Brazil
#######################

brazil_ts <- death_counts_data.df %>%
  filter(country == "Brazil") %>%
  filter(year >1989 & year < 2020) %>%
  select(year, com_deaths,com_death_rate,non_com_deaths, non_com_death_rate, total_deaths, total_death_rate) %>%
  group_by(year) %>%
  summarise(total_non_com_deaths = sum(non_com_deaths),
            total_com_deaths = sum(com_deaths),
            total_com_death_rate = mean(com_death_rate),
            total_non_com_death_rate = mean(non_com_death_rate),
            total_deaths = sum(total_deaths),
            total_death_rate = mean(total_death_rate))


brazil_ts %>% select(year, total_non_com_death_rate) %>%
  tsibble(index=year) ->
  brazil_ts

autoplot(brazil_ts,total_non_com_death_rate) + xlab("Year")+
  ylab("Death Rate") + ggtitle("Time Series of Death Rate")

# Split the data into train and test
brazil_train_df <- brazil_ts %>% filter(year<2016) 
brazil_test_df <- brazil_ts %>% filter(year>=2016)

#----------------------------------------------
# Naive, Drift, Mean Models - 2
#----------------------------------------------
# Fit the Naive, Drift, and Mean models on the training data
model_fit_brazil <-  brazil_train_df %>%
  model(
    Naive = NAIVE(total_non_com_death_rate),
    Drift = RW(total_non_com_death_rate ~ drift()),
    Mean = MEAN(total_non_com_death_rate)  
  )

# Forecast the death rate for 2016-2019
forecast_brazil <- model_fit_brazil %>%
  forecast(h = 4)

# Plot the forecasted data
forecast_brazil %>%  autoplot(brazil_ts, level = NULL) +
  labs(title = "Total Non Com Death rate - Brazil",
       y = "Non Com Death rate") +
  guides(colour = guide_legend(title = "Forecast"))

# Compute the accuracy metrics
accuracy_metrics_naive_brazil <- forecast_brazil %>%
  accuracy(brazil_ts)
accuracy_metrics_naive_brazil

#----------------------------------------------
# ETS Model - 2
#----------------------------------------------
# Fit the ets model
ets_fit_brazil <- brazil_train_df %>% 
  model(AN = ETS(total_non_com_death_rate ~ error("A") + trend("N")))
report(ets_fit_brazil)

#Forecast
forecast_ets_brazil <- ets_fit_brazil %>% 
  forecast(h = 4)


autoplot(forecast_ets_brazil,color="blue") +
  autolayer(brazil_ts, color="black") +
  labs(title = "ETS Total Non Com Death Rate - Brazil",
       y = "Non Com Death Rate") +
  guides(colour = guide_legend(title = "Forecast"))

tidy(ets_fit_brazil)


accuracy_metrics_ets_brazil <- forecast_ets_brazil %>%
  accuracy(brazil_ts)
accuracy_metrics_ets_brazil

#----------------------------------------------
# ARIMA Model - 2
#----------------------------------------------
# 
model_1 <- arima(brazil_train_df$total_non_com_death_rate, order = c(0,1,2))
model_2 <- arima(brazil_train_df$total_non_com_death_rate, order = c(2,1,0))
model_3 <- arima(brazil_train_df$total_non_com_death_rate, order = c(1,0,0))
model_4 <- auto.arima(brazil_train_df$total_non_com_death_rate, seasonal = TRUE, stepwise = FALSE, approximation = FALSE, lambda = "auto")
# 
AIC(model_1, model_2, model_3, model_4)

model_4

summary(model_4)

forecast_auto_arima_brazil <- model_4 %>%
  forecast(h = 4)

plot(forecast_auto_arima_brazil, ylab = "ARIMA Total Non Com Death Rate - Brazil")
lines(ma(brazil_ts, 4), col = "red")

#----------------------------------------------
# Linear Regression - 2
#----------------------------------------------
# Linear Regression Brazil
brazil_new <- death_counts_data.df %>%
  filter(country == "Brazil") %>%
  filter(year<2020) %>%
  select(year, sex, age_nums,com_deaths,com_death_rate,non_com_deaths, non_com_death_rate, total_deaths, total_death_rate, total_population) %>%
  group_by(year,sex) %>%
  summarise(total_non_com_deaths = sum(non_com_deaths),
            total_com_deaths = sum(com_deaths),
            total_com_death_rate = mean(com_death_rate),
            total_non_com_death_rate = mean(non_com_death_rate),
            total_deaths = sum(total_deaths),
            total_death_rate = mean(total_death_rate),
            population = sum(total_population))

selected.var = c(1,2,6,9)
set.seed(1)
train.index <- sample(c(1:dim(brazil_new)[1]),dim(brazil_new)[1]*0.8)  # 80% of dataset
train_brazil <- brazil_new[train.index,selected.var]
valid_brazil <- brazil_new[-train.index,selected.var]

linear.fit.brazil <- lm(total_non_com_death_rate ~ ., data = train_brazil)
summary(linear.fit.brazil)
par(mfrow = c(2, 2))
plot(linear.fit.brazil)

brazil.death.pred <- predict(linear.fit.brazil, valid_brazil)
residuals <- valid_brazil$total_non_com_death_rate - brazil.death.pred
lr_results_brazil <- data.frame('Year' = valid_brazil$year,'Predicted' = brazil.death.pred, 'Actual' = valid_brazil$total_non_com_death_rate,
                         'Residual' = residuals)

lr_results_brazil


lr_accuracy_brazil <- data.frame(Model = "Linear Regression",
                          MAPE = mean(abs((valid_brazil$total_non_com_death_rate - brazil.death.pred)/valid_brazil$total_non_com_death_rate))*100,
                          RMSE = sqrt(mean((valid_brazil$total_non_com_death_rate - brazil.death.pred)^2))
)
lr_accuracy_brazil

lr_metrics_brazil <- postResample(pred = brazil.death.pred, obs = valid_brazil$total_non_com_death_rate)

lr_metrics_brazil
ggplot(lr_results_brazil, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "LR Non Com Death Rate - Brazil", color = "Line")

#----------------------------------------------
# Ridge Regression - 2
#----------------------------------------------
# Ridge for  Brazil
ridge.fit.brazil <- train(total_non_com_death_rate ~ ., data = train_brazil, method = "ridge", trControl = trainControl(method = "cv",number=10))
ridge.fit.brazil

brazil.death.pred <- predict(ridge.fit.brazil, newdata = valid_brazil)
residuals <- valid_brazil$total_non_com_death_rate - brazil.death.pred
ridge_results_brazil <- data.frame('Year' = valid_brazil$year,'Predicted' = brazil.death.pred, 'Actual' = valid_brazil$total_non_com_death_rate,
                                'Residual' = residuals)

ridge_results_brazil


ridge_accuracy_brazil <- data.frame(Model = "Ridge Regression",
                                 MAPE = mean(abs((valid_brazil$total_non_com_death_rate - brazil.death.pred)/valid_brazil$total_non_com_death_rate))*100,
                                 RMSE = sqrt(mean((valid_brazil$total_non_com_death_rate - brazil.death.pred)^2))
)
ridge_accuracy_brazil
ridge_metrics_brazil <- postResample(pred = brazil.death.pred, obs = valid_brazil$total_non_com_death_rate)

ridge_metrics_brazil
ggplot(ridge_results_brazil, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "Ridge Non Com Death Rate - Brazil", color = "Line")

#----------------------------------------------
# Lasso Regression - 2
#----------------------------------------------

# Lasso Regression with Brazil
lasso.fit.brazil <- train(total_non_com_death_rate ~ ., data = train_brazil, method = "glmnet", trControl = trainControl(method = "cv",number=10))
lasso.fit.brazil

brazil.death.pred <- predict(lasso.fit.brazil, newdata = valid_brazil)

residuals <- valid_brazil$total_non_com_death_rate - brazil.death.pred
lasso_results_brazil <- data.frame('Year' = valid_brazil$year,'Predicted' = brazil.death.pred, 'Actual' = valid_brazil$total_non_com_death_rate,
                                'Residual' = residuals)

lasso_results_brazil


lasso_accuracy_brazil <- data.frame(Model = "Lasso Regression",
                                 MAPE = mean(abs((valid_brazil$total_non_com_death_rate - brazil.death.pred)/valid_brazil$total_non_com_death_rate))*100,
                                 RMSE = sqrt(mean((valid_brazil$total_non_com_death_rate - brazil.death.pred)^2))
)
lasso_accuracy_brazil
lasso_metrics_brazil <- postResample(pred = brazil.death.pred, obs = valid_brazil$total_non_com_death_rate)

lasso_metrics_brazil
ggplot(lasso_results_brazil, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "Lasso Non Com Death Rate - Brazil", color = "Line")

#----------------------------------------------
# PCR - 2
#----------------------------------------------

# PCR for Brazil

pcr.fit.brazil <- train(total_non_com_death_rate ~ ., data = train_brazil, method = "pcr", preProcess = c("center", "scale"), tuneLength = 10, trControl = trainControl(method = "cv", number = 10))
pcr.fit.brazil

brazil.death.pred <- predict(pcr.fit.brazil, newdata = valid_brazil)
residuals <- valid_brazil$total_non_com_death_rate - brazil.death.pred
pcr_results_brazil <- data.frame('Year' = valid_brazil$year,'Predicted' = brazil.death.pred, 'Actual' = valid_brazil$total_non_com_death_rate,
                                 'Residual' = residuals)

pcr_results_brazil


pcr_accuracy_brazil <- data.frame(Model = "PCR",
                                  MAPE = mean(abs((valid_brazil$total_non_com_death_rate - brazil.death.pred)/valid_brazil$total_non_com_death_rate))*100,
                                  RMSE = sqrt(mean((valid_brazil$total_non_com_death_rate - brazil.death.pred)^2))
)
pcr_accuracy_brazil
pcr_metrics_brazil <- postResample(pred = brazil.death.pred, obs = valid_brazil$total_non_com_death_rate)

pcr_metrics_brazil
ggplot(pcr_results_brazil, aes(x = Year)) +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  geom_line(aes(y = Actual, color = "Actual")) +
  labs(x = "Year", y = "Non Com Death Rate",title = "PCR Non Com Death Rate - Brazil", color = "Line")

# #----------------------------------------------
# # Time series
# #----------------------------------------------
# 
# 
# fit_arima <- usa_train_df %>%
#   model(ARIMA = ARIMA(total_death_rate))
# 
# forecast_arima <- fit_arima %>% 
#   forecast(h = 4)
# autoplot(forecast_arima) +
#   autolayer(usa, series = "Actual") +
#   labs(title = "Total Death rate",
#        y = "Total Death rate") +
#   guides(colour = guide_legend(title = "Forecast"))
# tidy(fit_arima)
# accuracy_metrics_arima <- accuracy(fit_arima)
# # training metrics
# accuracy_metrics_arima
