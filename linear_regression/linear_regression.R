library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(zoo)

toronto_file <- 'SARS-CoV-2_data-Toronto_raw.csv'
toronto_data <- read.csv(toronto_file, sep=",", check.names=FALSE)
head(toronto_data)

site_file <- 'toronto_siteID.csv'
site_data <- read.csv(site_file, sep=",", check.names=FALSE)

merged_data <- merge(toronto_data, site_data, by.x="sampleID", by.y="sampleID", all=FALSE)
toronto_df <- merged_data %>% filter(grepl('^TOR', merged_data$siteID))
toronto_df$date <- as.Date(toronto_df$dateTimeEnd)

saveRDS(toronto_df, file = "toronto_raw.rds")

# Read in cases by health region
covid_file <- 'CovidTimelineCanada/cases_hr.csv'
covid_data <- read.csv(covid_file, sep=",", check.names=FALSE)

hr_file <- 'CovidTimelineCanada/geo/hr.csv'
hr_data <- read.csv(hr_file, sep=",", check.names=FALSE)

metadata_file <- '/Users/admin/Desktop/Ct_cases_Papers/metadata/metadata_all.csv'
meta_data <- read.csv(metadata_file, sep=",", check.names=FALSE)

df <- meta_data %>% select("sample collection date", "specimen collector sample ID", "CT value")
colnames(df) <- c("date", "site", "ct")
df$date <- lubridate::ymd(df$date)

saveRDS(df, file = "toronto_ct.rds")


ontario_data <- covid_data %>% filter(grepl('ON', covid_data$region))
colnames(ontario_data) <- c("name", "region", "hruid", "date", "value", "value_daily")
final_data = ontario_data %>% left_join(hr_data, by = "hruid")

toronto_hr <- final_data %>% filter(grepl('3595', final_data$hruid))
saveRDS(toronto_hr, file = "toronto_hr_case_data.rds")

df$ct <- as.numeric(df$ct)
df$date <- as.Date(df$date)
toronto_hr$date <- as.Date(toronto_hr$date)

# Calculate 3-day and 7-day rolling averages
toronto_hr <- toronto_hr %>%
  arrange(date) %>%
  mutate(rolling_avg_3day = rollmean(value_daily, 3, fill = NA, align = "right"),
         rolling_avg_7day = rollmean(value_daily, 7, fill = NA, align = "right"))


pdf("toronto_daily_cases_rolling_avg.pdf", width=10)
p <- ggplot(data=toronto_hr, aes(x=date)) +
  scale_x_date(date_breaks = "1 year", date_labels =  "%m-%Y") +
  geom_bar(aes(y=value_daily), stat="identity", fill="orange") +
  geom_line(aes(y=rolling_avg_3day), color="blue", size=1) +
  geom_line(aes(y=rolling_avg_7day), color="green", size=1) +
  geom_smooth(aes(y=value_daily), method="loess", se=FALSE) +
  theme_linedraw() +
  ylab("Daily cases") +
  xlab("Date") +
  theme_pubr()
p
dev.off()


df_rolling <- df %>%
  arrange(date) %>%
  mutate(rolling_avg_3day_ct = rollmean(ct, 3, fill = NA, align = "right"),
         rolling_avg_7day_ct = rollmean(ct, 7, fill = NA, align = "right"))

p2 <- ggplot(data=df_rolling, aes(x=date)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y-%m", limits = as.Date(c("2021-11-01", "2024-01-01")), expand = c(0.07, 0)) +
  geom_point(aes(y=ct), size = 2, shape = 21) +
  geom_line(aes(y=rolling_avg_3day_ct), color="blue", size=1) +
  geom_line(aes(y=rolling_avg_7day_ct), color="green", size=1) +
  scale_y_continuous(breaks = seq(10, 40, 5), limits = c(10, 40)) +
  ylab("Ct value") +
  theme(axis.title.x=element_blank()) +
  theme_pubr()



regression_file <- '/1_qPCR_Results_NEW_2023.csv'
regression_data <- read.csv(regression_file, sep=",", check.names=FALSE)
head(regression_data)

regression_df = data.frame(select(regression_data, as.name("Site")), 
                           select(regression_data, as.name("date")), 
                           select(regression_data, as.name("N1_AVG_gc_per_g")), 
                           select(regression_data, as.name("N2_AVG_gc_per_g")),
                           check.names=FALSE)

regression_df$Date <- as.Date(regression_df$date, format = "%Y-%m-%d")
colnames(regression_df) <- c("SiteID", "Date", "N1_avg", "N2_avg", "date")
regression_df$log_N1 <- log(as.numeric(regression_df$N1_avg))
regression_df$log_N2 <- log(as.numeric(regression_df$N2_avg))

saveRDS(toronto_hr, file = "toronto_hr_case_data.rds")
