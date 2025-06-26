library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(CausalImpact)
library(table1)

covid_data <- read.csv("owid-covid-data.csv")
summary(covid_data)

df <- data.frame(select(covid_data, location, iso_code, date, population, median_age, new_cases, total_cases, total_deaths, total_cases_per_million, total_deaths_per_million, gdp_per_capita), check.names = FALSE)

df_filtered <- df %>% 
  filter(!grepl('^OWID_', iso_code)) %>% 
  filter(!stringr::str_detect(total_deaths, 'NA')) %>% 
  filter(!stringr::str_detect(gdp_per_capita, 'NA'))

df_filtered$date <- lubridate::ymd(df_filtered$date)
df_filtered$month <- lubridate::floor_date(df_filtered$date, 'month')

df_filtered <- df_filtered %>% 
  group_by(iso_code, month) %>% 
  summarize(new_cases = sum(new_cases), .groups = 'drop')

df_filtered <- df_total_cases %>% 
  filter(!grepl('OWID_', df_total_cases$iso_code))

# Highest total cases
result <- df_filtered[order(df_filtered$total_cases, decreasing = TRUE),]
head(result, 1)

# Highest total deaths
result <- df_filtered[order(df_filtered$total_deaths, decreasing = TRUE),]
head(result, 1)

# Highest total cases per million
result <- df_filtered[order(df_filtered$total_cases_per_million, decreasing = TRUE),]
head(result, 1)

# Highest total deaths per million
result <- df_filtered[order(df_filtered$total_deaths_per_million, decreasing = TRUE),]
head(result, 1)

df_total_cases <- data.frame(select(covid_data, location, iso_code, date, gdp_per_capita, new_cases, total_deaths), check.names = FALSE)
df_canada <- df_total_cases %>% filter(grepl('CAN', df_total_cases$iso_code))
df_canada$date <- lubridate::ymd(df_canada$date)

p1 <- ggplot(data = df_canada, aes(x = date, y = new_cases)) +
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") +
  geom_bar(stat = "identity", fill = "navy") + 
  ylab("New cases") + 
  xlab("Date") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
p1

p2 <- ggplot(data = df_canada, aes(x = date, y = total_deaths)) + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
  geom_bar(stat = "identity", fill = "navy") + 
  ylab("Total Deaths") + 
  xlab("Date") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
p2

df_gdp <- data.frame(select(covid_data, location, iso_code, date, gdp_per_capita, total_deaths, new_cases), check.names = FALSE)
df_filtered <- df_gdp %>% 
  filter(!grepl('OWID_', iso_code)) %>% 
  filter(!stringr::str_detect(gdp_per_capita, 'NA')) %>% 
  filter(!stringr::str_detect(total_deaths, 'NA')) %>% 
  distinct() %>% 
  mutate(gdp = ifelse(gdp_per_capita > 2000, "High GDP", "Low GDP"))

df_high <- filter(df_filtered, gdp == "High GDP")
df_low <- filter(df_filtered, gdp == "Low GDP")

result <- t.test(x = df_high$total_deaths, y = df_low$total_deaths)
result

result <- t.test(x = df_high$new_cases, y = df_low$new_cases)
result

df_data <- data.frame(select(covid_data, location, total_deaths_per_million, median_age), check.names = FALSE)
df_data$log_deaths <- log(as.numeric(df_data$total_deaths_per_million))
df_data <- df_data %>% filter(!stringr::str_detect(log_deaths, '-Inf'))
df_data$log_median_age <- log(as.numeric(df_data$median_age))
df_data <- df_data %>% filter(!stringr::str_detect(log_median_age, '-Inf'))

result <- cor.test(df_data$log_deaths, df_data$log_median_age, method = "pearson")
result

fit1 <- lm(total_deaths_per_million ~ median_age, data = df_data)
summary(fit1)$r.squared
summary(fit1)$adj.r.squared

fit2 <- lm(log_deaths ~ log_median_age, data = df_data)
summary(fit2)$r.squared
summary(fit2)$adj.r.squared

p2 <- ggplot(data = df_data, aes(x = log_median_age, y = log_deaths)) + 
  geom_point()
p2

calculate_sample_size <- function(prevalence, margin_of_error, confidence_level = 0.95) {
  sample_size <- ((1.96^2) * (prevalence * (1 - prevalence))) / (margin_of_error^2)
  return(sample_size)
}

sample_size <- calculate_sample_size(0.10, 0.05)
print(sample_size)

setwd("/Users/admin/Desktop")
df <- data.frame(select(covid_data, positive_rate, date), check.names = FALSE)
df$date <- lubridate::ymd(df$date)
p2 <- ggplot(data = df, aes(x = date, y = positive_rate)) + 
  geom_point() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
p2

df <- data.frame(select(covid_data, total_vaccinations, date), check.names = FALSE)
df$date <- lubridate::ymd(df$date)
p3 <- ggplot(data = df, aes(x = date, y = total_vaccinations)) + 
  geom_point() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
p3

df <- data.frame(select(covid_data, date, positive_rate, total_vaccinations), check.names = FALSE)
df$date <- lubridate::ymd(df$date)
df$total_vaccinations <- as.numeric(df$total_vaccinations)

df_filtered <- df %>% filter(!stringr::str_detect(positive_rate, 'NA')) %>% filter(!stringr::str_detect(total_vaccinations, 'NA'))

pre.period <- as.Date(c("2020-01-07", "2020-12-03"))
post.period <- as.Date(c("2020-12-04", "2022-06-23"))

time.points <- seq.Date(as.Date("2020-01-07"), as.Date("2022-06-23"), "days")
info <- df_filtered[c("positive_rate", "total_vaccinations")]
data <- zoo(info, time.points)

impact <- CausalImpact(data, pre.period, post.period)

plot(impact)
summary(impact)

p4 <- ggplot(data = df_filtered, aes(x = date, y = total_vaccinations)) + 
  geom_point() + 
  scale_x_date(date_breaks = "1 month", date_labels = "%m-%Y") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 5))
p4
