
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
