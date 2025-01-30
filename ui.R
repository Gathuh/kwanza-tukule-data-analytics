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

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = "Kwanza Tukule Sales Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Sales Analysis", tabName = "sales_analysis", icon = icon("chart-bar")),
      menuItem("Trends", tabName = "trends", icon = icon("line-chart")),
      menuItem("Segmentation", tabName = "segmentation", icon = icon("users")),
      menuItem("Forecasting", tabName = "forecasting", icon = icon("chart-line"))
    ),
    selectInput("category", "Select Category:", choices = c("All", unique(updated_data$`ANONYMIZED CATEGORY`)), selected = "All", multiple = TRUE),
    selectizeInput("business", "Select Business:", 
                   choices = c("All", unique(updated_data$`ANONYMIZED BUSINESS`)), 
                   selected = "All", 
                   multiple = TRUE),
    selectizeInput("product", "Select Product:", choices = c("All", unique(updated_data$`ANONYMIZED PRODUCT`)), selected = "All", multiple = F)
  ),
  dashboardBody(
    tabItems(
      # Overview Tab
      # Overview Tab
      tabItem(tabName = "overview",
              fluidRow(
                valueBoxOutput("total_sales"),
                valueBoxOutput("total_quantity"),
                valueBoxOutput("unique_businesses")
              ),
              fluidRow(
                # Bar chart for Total Quantity by Category
                box(
                  plotlyOutput("quantity_by_category_plot"), width = 6
                ),
                # Bar chart for Total Value by Category
                box(
                  plotlyOutput("value_by_category_plot"), width = 6
                )
              )
      ),
      
      # Sales Analysis Tab
      tabItem(tabName = "sales_analysis",
              fluidRow(
                box(plotlyOutput("sales_category_plot"), width = 6),
                box(plotlyOutput("sales_business_plot"), width = 6)
              ),
              DTOutput("sales_table")
      ),
      
      # Trends Tab
      tabItem(tabName = "trends",
              fluidRow(
                box(plotlyOutput("time_series_plot"), width = 12)
              )
      ),
      
      # Segmentation Tab
      tabItem(tabName = "segmentation",
              plotlyOutput("kmeans_plot"),
              DTOutput("segmentation_table")
      ),
      
      # Forecasting Tab
      tabItem(tabName = "forecasting",
              numericInput("forecast_horizon", "Forecast Horizon (Months):", value = 3, min = 1, max = 12),
              plotlyOutput("forecast_plot")
      )
    )
  )
)
