
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
