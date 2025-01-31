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
updated_data<-sample_n(updated_data,1000)
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




# Define Server
server <- function(input, output) {
  # Reactive function to filter data based on user inputs
  filtered_data <- reactive({
    data <- updated_data
    if (!"All" %in% input$category) {
      data <- data %>% filter(`ANONYMIZED CATEGORY` %in% input$category)
    }
    if (!"All" %in% input$business) {
      data <- data %>% filter(`ANONYMIZED BUSINESS` %in% input$business)
    }
    if (!"All" %in% input$product) {
      data <- data %>% filter(`ANONYMIZED PRODUCT` %in% input$product)
    }
    data
  })
  
  # Value Boxes
  output$total_sales <- renderValueBox({
    valueBox(sum(filtered_data()$value, na.rm = TRUE), "Total Sales", icon = icon("dollar"))
  })
  
  output$total_quantity <- renderValueBox({
    valueBox(sum(filtered_data()$QUANTITY, na.rm = TRUE), "Total Quantity", icon = icon("boxes"))
  })
  
  output$unique_businesses <- renderValueBox({
    valueBox(n_distinct(filtered_data()$`ANONYMIZED BUSINESS`), "Unique Businesses", icon = icon("building"))
  })
  # Total Quantity by Category Bar Plot
  output$quantity_by_category_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(`ANONYMIZED CATEGORY`) %>%
      summarise(Total_Quantity = sum(QUANTITY, na.rm = TRUE)) %>%
      arrange(desc(Total_Quantity))
    
    plot_ly(data, x = ~`ANONYMIZED CATEGORY`, y = ~Total_Quantity, type = "bar", 
            color = ~`ANONYMIZED CATEGORY`, colors = RColorBrewer::brewer.pal(n = 8, name = "Set2")) %>%
      layout(title = "Total Quantity by Category", xaxis = list(title = "Category"), yaxis = list(title = "Total Quantity"))
  })
  
  # Total Value by Category Bar Plot
  output$value_by_category_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(`ANONYMIZED CATEGORY`) %>%
      summarise(Total_Value = sum(value, na.rm = TRUE)) %>%
      arrange(desc(Total_Value))
    
    plot_ly(data, x = ~`ANONYMIZED CATEGORY`, y = ~Total_Value, type = "bar", 
            color = ~`ANONYMIZED CATEGORY`, colors = RColorBrewer::brewer.pal(n = 8, name = "Set3")) %>%
      layout(title = "Total Value by Category", xaxis = list(title = "Category"), yaxis = list(title = "Total Value"))
  })
  
  # Sales by Category Plot
  output$sales_category_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(`ANONYMIZED CATEGORY`) %>%
      summarise(Total_Sales = sum(value, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales))
    
    plot_ly(data, x = ~`ANONYMIZED CATEGORY`, y = ~Total_Sales, type = "bar", 
            marker = list(color = RColorBrewer::brewer.pal(n = 8, name = "Set2"))) %>%
      layout(title = "Sales by Category", xaxis = list(title = "Category"), yaxis = list(title = "Total Sales"))
  })
  
  # Sales by Business Plot
  output$sales_business_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(`ANONYMIZED BUSINESS`) %>%
      summarise(Total_Sales = sum(value, na.rm = TRUE)) %>%
      arrange(desc(Total_Sales)) %>%
      head(10)
    
    plot_ly(data, x = ~`ANONYMIZED BUSINESS`, y = ~Total_Sales, type = "bar", 
            marker = list(color = RColorBrewer::brewer.pal(n = 8, name = "Set1"))) %>%
      layout(title = "Top 10 Businesses by Sales", xaxis = list(title = "Business"), yaxis = list(title = "Total Sales"))
  })
  
  # Time Series Plot
  output$time_series_plot <- renderPlotly({
    data <- filtered_data() %>%
      group_by(DATE = floor_date(DATE, "day")) %>%
      summarise(Daily_Value = sum(value, na.rm = TRUE)) %>%
      arrange(DATE)  # Ensure chronological order
    
    plot_ly(data, x = ~DATE, y = ~Daily_Value, type = "scatter", mode = "lines+markers", 
            line = list(color = 'blue')) %>%
      layout(title = "Sales Trends Over Time", xaxis = list(title = "Date"), yaxis = list(title = "Daily Sales Value"))
  })
  
  # K-means Plot
  output$kmeans_plot <- renderPlotly({
    plot_ly(business_summary, x = ~Total_Quantity, y = ~Total_Value, color = ~cluster, type = "scatter", mode = "markers") %>%
      layout(title = "Customer Segmentation", xaxis = list(title = "Total Quantity"), yaxis = list(title = "Total Value"))
  })
  
  # Segmentation Table
  output$segmentation_table <- renderDT({
    datatable(business_summary)
  })
  
  
  # Forecast Plot
  output$forecast_plot <- renderPlotly({
    ts_data <- ts(filtered_data()$value, frequency = 12)
    fit <- auto.arima(ts_data)
    forecast_data <- forecast(fit, h = input$forecast_horizon)
    
    # Create future dates for x-axis
    future_dates <- seq(max(filtered_data()$DATE), by = "month", length.out = input$forecast_horizon + 1)[-1]
    
    # Plot forecast
    plot_ly() %>%
      add_lines(x = future_dates, y = forecast_data$mean, name = "Forecast", line = list(color = 'red')) %>%
      layout(title = "Sales Forecast", xaxis = list(title = "Date"), yaxis = list(title = "Sales Value"))
  })
}

# Run the app
shinyApp(ui, server)
