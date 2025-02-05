library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(lubridate)
library(forecast)
library(cluster)
library(factoextra)

# Load and preprocess data
updated_data <- read_csv("Case Study Data - Read Only - case_study_data_2025-01-16T06_49_12.19881Z.csv")
updated_data$DATE <- mdy_hm(updated_data$DATE, tz = "Africa/Nairobi")
updated_data <- na.exclude(updated_data)
updated_data$value <- updated_data$QUANTITY * updated_data$`UNIT PRICE`

# K-means Segmentation
business_summary <- updated_data %>% 
  group_by(`ANONYMIZED BUSINESS`) %>% 
  summarise(Total_Quantity = mean(QUANTITY, na.rm = TRUE),
            Total_Value = mean(value, na.rm = TRUE),
            Transactions = n(),
            .groups = 'drop')

scaled_data <- scale(business_summary[, -1])
set.seed(123)
kmeans_result <- kmeans(scaled_data, centers = 3, nstart = 25)
business_summary$cluster <- as.factor(kmeans_result$cluster)
