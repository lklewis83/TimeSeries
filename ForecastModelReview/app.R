library(shiny)
library(ggplot2)
library(dygraphs)
library(plotly)
library(dplyr)
library(DT)
# SMU
library(tswge)
library(nnfor)  # For MLP functions
# For Prophet and TimeGen
library(prophet)
library(nixtlar)

# Set memory options
options(shiny.maxRequestSize = 100*1024^2)  # Increase to 100MB

# UI - Simplified version without file uploads
ui <- fluidPage(
  # Custom CSS styling
  tags$head(tags$style(HTML("
    body { background-color: #ffffff; }
    h3, h4 { color: #0071ce; }
    .well {
      border-left: 5px solid #ffc220;
      background-color: #f8f9fa;
    }
    .model-section {
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 15px;
      margin-bottom: 20px;
    }
    .metrics-box {
      background-color: #f8f9fa;
      border-left: 4px solid #0071ce;
      padding: 10px;
      margin-bottom: 15px;
    }
    .model-title {
      border-bottom: 2px solid #ffc220;
      padding-bottom: 5px;
      margin-bottom: 15px;
    }
  "))),
  
  # App title with Walmart logo - Fixed to use www folder correctly
  titlePanel(div(style = "display: flex; align-items: center;", 
                 tags$img(src = "walmart_logo.png", height = "20px", style = "margin-right: 10px;"),
                 "Walmart Forecast Model Dashboard")),
  
  # App description
  wellPanel(
    HTML("<p><strong>Data Source:</strong> <a href='https://www.kaggle.com/datasets/aslanahmedov/walmart-sales-forecast' target='_blank'>
          Walmart Sales Forecast</a> – Weekly dataset with Monthly holiday trends. This version is filtered for <strong><b>Store A</b></strong> only.<br>
          <strong>Forecast period:</strong> 8 weeks.</p>")
  ),
  
  # Tabs for different views
  tabsetPanel(
    # Model Metrics tab (moved to first tab from being a panel above tabs)
    tabPanel("Model Performance Metrics",
             wellPanel(
               h4("Model Performance Metrics", style = "text-align: left;"),
               div(style = "overflow-x: auto;",
                   tableOutput("metrics_table")
               )
             ),
             # Added new model descriptions as requested
             div(class = "model-section",
                 h4("Model Information", class = "model-title"),
                 HTML("<p><strong>Prophet</strong> <a href='https://facebook.github.io/prophet/' target='_blank'>https://facebook.github.io/prophet/</a> Prophet is an open-source forecasting tool available in R and Python, built on an additive model that captures non-linear trends with yearly, weekly, and daily seasonality—including holiday effects. It performs best with time series data exhibiting strong seasonal patterns and ample historical records. Prophet is known for its ease of use and intuitive, informative plots.</p>
                      
                      <p><strong>TimeGPT</strong> <a href='https://cran.r-project.org/web/packages/nixtlar/vignettes/azure-quickstart.html' target='_blank'>https://cran.r-project.org/web/packages/nixtlar/vignettes/azure-quickstart.html</a> <strong>|</strong> <a href='https://azuremarketplace.microsoft.com/en/marketplace/apps/nixtla.nixtlatimegen1?tab=Overview' target='_blank'>https://azuremarketplace.microsoft.com/en/marketplace/apps/nixtla.nixtlatimegen1?tab=Overview</a> <strong>|</strong> <a href='https://www.nixtla.io/docs/getting-started-about_timegpt' target='_blank'>https://www.nixtla.io/docs/getting-started-about_timegpt</a> TimeGPT is a generative pre-trained transformer model tailored for time series forecasting across domains like retail, energy, finance, and IoT. With minimal coding, it delivers high-accuracy forecasts and benchmarks well against classical models (ARIMA), machine learning (LightGBM), and deep learning (N-HiTS). While powerful, its output plots rely on unique IDs, making them less visually intuitive than traditional charts.</p>
                      
                      <p><strong>TSWGE</strong> <a href='https://github.com/BivinSadler/tswge/blob/master/DESCRIPTION' target='_blank'>https://github.com/BivinSadler/tswge/blob/master/DESCRIPTION</a> <strong>|</strong> <a href='https://r-packages.io/' target='_blank'>https://r-packages.io/</a> <strong>|</strong> <a href='https://cran.r-project.org/web/packages/tswge/tswge.pdf' target='_blank'>https://cran.r-project.org/web/packages/tswge/tswge.pdf</a> The TSWGE package (Time Series for Data Science) supports both teaching and applied analysis of time series in R. Developed to accompany textbooks by Woodward, Sadler, and others, it provides a comprehensive suite of tools for classical time series modeling and diagnostics, making it ideal for educational and practical forecasting applications.</p>")
             )
    ),
    
    # TSWGE Model tab
    tabPanel("TSWGE (MLP)",
             div(class = "model-section",
                 h3("📈 TSWGE (MLP) Forecast", class = "model-title"),
                 plotOutput("tswge_plot", height = "300px"),
                 div(class = "metrics-box",
                     HTML("<p><strong>ASE:</strong> 0.10 | <strong>WMAE:</strong> 0.29</p>
                          <p>The Multilayer Perceptron (MLP) neural network model from the TSWGE package delivers 
                          strong forecasting performance by capturing both trend and seasonality within weekly sales 
                          data (frequency = 52). The model is trained on a time series dataset aggregated by date, 
                          with scaled features such as temperature, fuel price, CPI, and unemployment. 
                          These serve as exogenous variables along with holiday indicators that enhance predictive 
                          accuracy and allow the MLP to account for external economic and seasonal 
                          influences on weekly & monthly sales. While this is the best performing model, this model 
                          is the hardest to configure as well as learn to use.</p>")
                 ),
                 h4("Residuals"),
                 DTOutput("tswge_residuals")
             )
    ),
    
    # Prophet Model tab
    tabPanel("Prophet",
             div(class = "model-section",
                 h3("📈 Prophet Forecast", class = "model-title"),
                 plotOutput("prophet_plot", height = "300px"),
                 div(class = "metrics-box",
                     HTML("<p><strong>ASE:</strong> 0.34 | <strong>WMAE:</strong> 0.33</p>
                          <p>Facebook's Prophet model decomposes the time series into trend, seasonality, 
                          and holiday effects, offering interpretable forecasts that clearly reflect external influences. 
                          The data is aggregated by date, with scaled columns for features like temperature, fuel price, 
                          CPI, unemployment, and weekly sales. Prophet uses a DataFrame with a required structured of a target 
                          column (“y”) and timestamp (“ds”), alongside a custom holiday DataFrame that includes 
                          events such as Thanksgiving, Black Friday, Cyber Monday, and Christmas from 2010 to 2012. 
                          Although it’s not as precise as TSWGE for this dataset, Prophet excels at modeling holiday effects 
                          and generating future dates using its built-in 'make_future_dataframe()' function. It is also very easy to use!</p>")
                 ),
                 h4("Residuals"),
                 DTOutput("prophet_residuals")
             )
    ),
    
    # TimeGen Model tab with updated plot function
    tabPanel("TimeGen",
             div(class = "model-section",
                 h3("📈 TimeGen Forecast", class = "model-title"),
                 plotOutput("timegen_plot", height = "300px"),
                 div(class = "metrics-box",
                     HTML("<p><strong>ASE:</strong> 0.88 | <strong>WMAE:</strong> 0.64</p>
                          <p>The TimeGen model from the Nixtla API provides automated forecasting with minimal configuration, making it 
                          ideal for quick experimentation or prototyping. I used a DataFrame with scaled features including temperature, 
                          fuel price, CPI, unemployment, and weekly sales alongside required columns for date (“ds”), target (“y”), and a unique identifier 
                          (“unique_id”).</p> 
                          <p>In this implementation, the unique_id was constructed by combining StoreNumber and Is Holiday Flag (e.g., IsHoliday-TRUE), 
                          which differs from the structure used in the TSWGE and Prophet models. As a standalone forecasting model, a more strategic 
                          unique_id such as store type (A, B, C) could be more effective. Additionally, TimeGen requires more data than the other 
                          models to generate forecasts, which is why it was only applied to the scaled dataset rather than the aggregated version. 
                          Despite having the highest error metrics in this study, it remains a useful tool for automated, low-effort forecasting on large datasets.</p>")
                 ),
                 h4("Residuals"),
                 DTOutput("timegen_residuals")
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load data files directly rather than waiting for upload
  trained_filtered <- reactive({
    # Attempt to read from current directory
    train_file_path <- "walmart_train.csv"
    
    # Check if file exists
    if (!file.exists(train_file_path)) {
      # Create a notification for missing file
      showNotification("Train file not found in current directory. Using placeholder data.", 
                       type = "warning", duration = 10)
      
      # Return placeholder data if file not found
      return(data.frame(
        Date = seq.Date(from = as.Date("2010-02-05"), by = "week", length.out = 143),
        Weekly_Sales = rnorm(143, 1000, 200),
        Store = rep(1:3, length.out = 143),
        IsHoliday = sample(c(TRUE, FALSE), 143, replace = TRUE, prob = c(0.2, 0.8)),
        Temperature = rnorm(143, 60, 15),
        Fuel_Price = rnorm(143, 3, 0.5),
        CPI = rnorm(143, 200, 10),
        Unemployment = rnorm(143, 6, 1)
      ))
    }
    
    # Read actual file
    read.csv(train_file_path)
  })
  
  validation_filtered <- reactive({
    # Attempt to read from current directory
    test_file_path <- "walmart_test.csv"
    
    # Check if file exists
    if (!file.exists(test_file_path)) {
      # Create a notification for missing file
      showNotification("Test file not found in current directory. Using placeholder data.", 
                       type = "warning", duration = 10)
      
      # Return placeholder data if file not found
      return(data.frame(
        Date = seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8),
        Weekly_Sales = rnorm(8, 1000, 200),
        Store = rep(1:3, length.out = 8),
        IsHoliday = sample(c(TRUE, FALSE), 8, replace = TRUE, prob = c(0.2, 0.8)),
        Temperature = rnorm(8, 60, 15),
        Fuel_Price = rnorm(8, 3, 0.5),
        CPI = rnorm(8, 200, 10),
        Unemployment = rnorm(8, 6, 1)
      ))
    }
    
    # Read actual file
    read.csv(test_file_path)
  })
  
  # Process input files immediately
  scaled_a <- reactive({
    # Get training data
    trained_data <- trained_filtered()
    
    # Process for TSWGE and Prophet (aggregated data)
    aggregated_data <- trained_data %>%
      group_by(Date) %>%
      summarize(
        weekly_sales = sum(Weekly_Sales, na.rm = TRUE),
        IsHoliday = first(IsHoliday),
        Temperature = mean(Temperature, na.rm = TRUE),
        Fuel_Price = mean(Fuel_Price, na.rm = TRUE),
        CPI = mean(CPI, na.rm = TRUE),
        Unemployment = mean(Unemployment, na.rm = TRUE),
        Store = n_distinct(Store),
        .groups = "drop"
      ) %>%
      arrange(Date)
    
    # Scale the data
    columns_to_scale <- c("Temperature", "Fuel_Price", "CPI", "Unemployment", "weekly_sales")
    scaled_data <- aggregated_data
    scaled_data[columns_to_scale] <- scale(scaled_data[columns_to_scale])
    
    # Make sure Date is in correct format
    scaled_data$Date <- as.Date(scaled_data$Date)
    
    return(scaled_data)
  })
  
  timegen_data <- reactive({
    # Get training data
    trained_data <- trained_filtered()
    
    # Process for TimeGen (non-aggregated)
    result <- trained_data %>%
      select(Date, Weekly_Sales, Store, IsHoliday) %>%
      rename(ds = Date, y = Weekly_Sales) %>%
      mutate(unique_id = paste(Store, IsHoliday, sep = "-"))
    
    # Make sure ds is in Date format
    result$ds <- as.Date(result$ds)
    
    return(result)
  })
  
  # TSWGE Model ----------------------------------------------------------------
  tswge_model <- reactive({
    # First try to load MLP model forecast with dates
    if (file.exists("tswge_forecast_with_dates.csv")) {
      # Load the forecast with dates CSV file
      forecast_with_dates <- read.csv("tswge_forecast_with_dates.csv")
      
      # Ensure Date column is properly formatted
      forecast_with_dates$Date <- as.Date(forecast_with_dates$Date)
      
      # Convert to the format expected by the plot function
      mlp52_for <- list(
        mean = forecast_with_dates$Forecast,
        dates = forecast_with_dates$Date
      )
      
      # Try to load residuals and add them to the model object
      if (file.exists("residuals_tswge.csv")) {
        tswge_residuals_df <- read.csv("residuals_tswge.csv")
        mlp52_for$residuals <- tswge_residuals_df$residual
      }
      
      return(mlp52_for)
    } 
    # If date file not found, try the RDS file
    else if (file.exists("mlp52_for.rds")) {
      mlp52_for <- readRDS("mlp52_for.rds")
      return(mlp52_for)
    } 
    # If no files found, create a placeholder
    else {
      # Create placeholder if file not found
      mlp52_for <- list(
        mean = runif(8, -0.5, 0.5),
        residuals = runif(8, -0.3, 0.3)
      )
      return(mlp52_for)
    }
  })
  
  # TSWGE plot using dates directly from the forecast file
  output$tswge_plot <- renderPlot({
    # Get data
    scaled_data <- scaled_a()
    mlp52_for <- tswge_model()
    
    # Check if we have the dates from the CSV file
    if ("dates" %in% names(mlp52_for)) {
      forecast_dates <- mlp52_for$dates
    } else {
      # If not, calculate them based on the data
      forecast_start_date <- max(scaled_data$Date) + 7
      forecast_dates <- seq.Date(from = forecast_start_date, by = "week", length.out = length(mlp52_for$mean))
    }
    
    # Create a data frame for original data
    original_df <- data.frame(
      Date = scaled_data$Date,
      Value = scaled_data$weekly_sales,
      Type = "Historical"
    )
    
    # Create a data frame for forecast data
    forecast_df <- data.frame(
      Date = forecast_dates,
      Value = mlp52_for$mean,
      Type = "Forecast"
    )
    
    # Combine the data frames
    combined_df <- rbind(original_df, forecast_df)
    
    # Create confidence intervals if residuals are available
    if (sum(!is.na(mlp52_for$residuals)) > 0) {
      sd_val <- sd(mlp52_for$residuals, na.rm = TRUE)
      
      ci_lower <- forecast_df$Value - 1.96 * sd_val
      ci_upper <- forecast_df$Value + 1.96 * sd_val
      
      # Add confidence intervals to the forecast data frame
      forecast_df$Lower <- ci_lower
      forecast_df$Upper <- ci_upper
    }
    
    # Plot with ggplot2
    p <- ggplot() +
      # Historical data
      geom_line(data = original_df, aes(x = Date, y = Value), color = "black", size = 1) +
      # Forecast
      geom_line(data = forecast_df, aes(x = Date, y = Value), color = "blue", size = 1.2) +
      # Add points to the forecast
      geom_point(data = forecast_df, aes(x = Date, y = Value), color = "blue", size = 3)
    
    # Add confidence interval if available
    if ("Lower" %in% names(forecast_df)) {
      p <- p + geom_ribbon(data = forecast_df, 
                           aes(x = Date, ymin = Lower, ymax = Upper), 
                           fill = "blue", alpha = 0.2)
    }
    
    # Add labels and styling
    p <- p + 
      labs(title = "TSWGE MLP Forecast with Dates",
           x = "Date",
           y = "Weekly Sales (Scaled)") +
      theme_minimal() +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      # Add vertical line at the forecasting start point
      geom_vline(xintercept = as.numeric(min(forecast_dates)), 
                 linetype = "dashed", color = "gray50")
    
    return(p)
  })
  
  # TSWGE residuals table
  output$tswge_residuals <- renderDT({
    # First try to load residuals with dates CSV
    if (file.exists("tswge_residuals_with_dates.csv")) {
      tswge_residuals_df <- read.csv("tswge_residuals_with_dates.csv")
      # Rename columns if needed for consistency 
      if ("Date" %in% names(tswge_residuals_df) && "Residual" %in% names(tswge_residuals_df)) {
        tswge_residuals_df <- tswge_residuals_df %>%
          rename(date = Date, residual = Residual)
      }
    } 
    # If not found, try the regular residuals file
    else if (file.exists("residuals_tswge.csv")) {
      tswge_residuals_df <- read.csv("residuals_tswge.csv")
    } 
    # If no files found, create a placeholder
    else {
      # Create placeholder if file not found
      forecast_dates <- seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8)
      tswge_residuals_df <- data.frame(
        date = forecast_dates,
        residual = runif(8, -0.3, 0.3)
      )
    }
    
    datatable(
      tswge_residuals_df,
      options = list(pageLength = 8, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 'residual', digits = 4)
  })
  
  # Prophet Model --------------------------------------------------------------
  prophet_model <- reactive({
    # Try to load Prophet model and forecast
    if (file.exists("prophet_forecast_df.csv")) {
      prophet_forecast <- read.csv("prophet_forecast_df.csv")
      # Make sure date column is in Date format
      prophet_forecast$ds <- as.Date(prophet_forecast$ds)
      return(prophet_forecast)
    } else if (file.exists("prophet_for.rds")) {
      prophet_forecast <- readRDS("prophet_for.rds")
      return(prophet_forecast)
    } else {
      # Create placeholder if file not found
      future_dates <- seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8)
      prophet_forecast <- data.frame(
        ds = future_dates,
        yhat = runif(8, -0.5, 0.5),
        yhat_lower = runif(8, -1, -0.5),
        yhat_upper = runif(8, 0.5, 1)
      )
      return(prophet_forecast)
    }
  })
  
  # Prophet model object for plot(m, forecast)
  prophet_model_obj <- reactive({
    # Check if the RDS file exists
    if (file.exists("prophet_model.rds")) {
      m <- readRDS("prophet_model.rds")
      return(m)
    } else {
      return(NULL)
    }
  })
  
  # Prophet plot using plot(m, forecast) as requested
  output$prophet_plot <- renderPlot({
    # Get data
    scaled_data <- scaled_a()
    prophet_forecast <- prophet_model()
    prophet_m <- prophet_model_obj()
    
    # If we have the model object, use plot(m, forecast)
    if (!is.null(prophet_m)) {
      # Use the requested plot(m, forecast) function
      plot(prophet_m, prophet_forecast)
    } else {
      # Fallback to a standard ggplot if we don't have the model object
      # Format for prophet data input
      prophet_df <- scaled_data %>%
        select(Date, weekly_sales) %>%
        rename(ds = Date, y = weekly_sales)
      
      # Plot with ggplot2
      ggplot() +
        # Historical data
        geom_line(data = prophet_df, aes(x = ds, y = y), color = "black", size = 1) +
        # Forecast
        geom_line(data = prophet_forecast, aes(x = ds, y = yhat), color = "blue", size = 1.2) +
        # Confidence interval
        geom_ribbon(data = prophet_forecast, 
                    aes(x = ds, ymin = yhat_lower, ymax = yhat_upper), 
                    fill = "blue", alpha = 0.2) +
        labs(title = "Prophet Forecast (Standard View)",
             subtitle = "Note: Model object not available for plot(m, forecast)",
             x = "Date",
             y = "Weekly Sales (Scaled)") +
        theme_minimal() +
        scale_x_date(date_breaks = "3 months", date_labels = "%b %Y")
    }
  })
  
  # Prophet residuals table
  output$prophet_residuals <- renderDT({
    # Try to load residuals file
    if (file.exists("residuals_prophet.csv")) {
      prophet_residuals_df <- read.csv("residuals_prophet.csv")
    } else {
      # Create placeholder if file not found
      forecast_dates <- seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8)
      prophet_residuals_df <- data.frame(
        date = forecast_dates,
        residual = runif(8, -0.5, 0.5)
      )
    }
    
    datatable(
      prophet_residuals_df,
      options = list(pageLength = 8, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 'residual', digits = 4)
  })
  
  # TimeGen Model --------------------------------------------------------------
  timegen_model <- reactive({
    # Try to load TimeGen model and forecast
    if (file.exists("timeG_forecast_df.csv")) {
      timegen_forecast <- read.csv("timeG_forecast_df.csv")
      # Make sure date column is in Date format
      if ("ds" %in% names(timegen_forecast)) {
        timegen_forecast$ds <- as.Date(timegen_forecast$ds)
      }
      return(timegen_forecast)
    } else if (file.exists("timeG_for.rds")) {
      timegen_forecast <- readRDS("timeG_for.rds")
      return(timegen_forecast)
    } else {
      # Create placeholder if file not found
      forecast_dates <- seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8)
      timegen_forecast <- data.frame(
        ds = forecast_dates,
        TimeGPT = runif(8, -0.5, 0.5)
      )
      return(timegen_forecast)
    }
  })
  
  # TimeGen plot - Using nixtla_client_plot as requested
  output$timegen_plot <- renderPlot({
    # We need the scaled data and TimeGen data
    timegen_data_df <- timegen_data()
    
    # Check if we have the necessary objects for nixtla_client_plot
    if (file.exists("nixtla_new_df.rds") && file.exists("nixtla_forecast_w.rds")) {
      # Load the saved objects
      new_df <- readRDS("nixtla_new_df.rds")
      forecast_w <- readRDS("nixtla_forecast_w.rds")
      
      # Ensure dates are properly formatted for nixtla_client_plot
      new_df$ds <- as.POSIXct(new_df$ds)
      forecast_w$ds <- as.POSIXct(forecast_w$ds)
      
      # Use the nixtla_client_plot function as requested
      nixtla_client_plot(new_df, forecast_w, max_insample_length = 200)
    } else {
      # Try to create the objects if they don't exist in RDS format
      # First check if we have the objects in memory already
      
      # Get the forecast
      timegen_forecast <- timegen_model()
      
      # Try to adapt the data to the format expected by nixtla_client_plot
      # If we have TimeGPT column, we might be able to construct forecast_w
      if ("TimeGPT" %in% colnames(timegen_forecast)) {
        # Create a new_df from timegen_data
        new_df <- timegen_data_df
        
        # Ensure dates are properly formatted
        new_df$ds <- as.POSIXct(new_df$ds)
        timegen_forecast$ds <- as.POSIXct(timegen_forecast$ds)
        
        # Try to use nixtla_client_plot
        tryCatch({
          nixtla_client_plot(new_df, timegen_forecast, max_insample_length = 200)
        }, error = function(e) {
          # If it fails, fall back to a standard plot
          create_fallback_plot(timegen_data_df, timegen_forecast)
        })
      } else {
        # If proper format not available, use fallback plot
        create_fallback_plot(timegen_data_df, timegen_forecast)
      }
    }
  })
  
  # Helper function for creating fallback TimeGen plot
  create_fallback_plot <- function(timegen_data_df, timegen_forecast) {
    # Aggregate the data by date to get a single line
    aggregated_timegen <- timegen_data_df %>%
      group_by(ds) %>%
      summarize(y = mean(y, na.rm = TRUE))
    
    # Ensure dates are properly formatted
    aggregated_timegen$ds <- as.Date(aggregated_timegen$ds)
    timegen_forecast$ds <- as.Date(timegen_forecast$ds)
    
    # Create a standard ggplot
    ggplot() +
      # Historical data
      geom_line(data = aggregated_timegen, aes(x = ds, y = y), color = "black", size = 1) +
      # Forecast
      geom_line(data = timegen_forecast, aes(x = ds, y = TimeGPT), color = "green", size = 1.2) +
      # Add points to the forecast
      geom_point(data = timegen_forecast, aes(x = ds, y = TimeGPT), color = "green", size = 3) +
      labs(title = "TimeGen Forecast (Standard View)",
           subtitle = "Note: Not using nixtla_client_plot due to missing data objects",
           x = "Date",
           y = "Weekly Sales (Scaled)") +
      theme_minimal() +
      scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
      # Add vertical line at the forecasting start point
      geom_vline(xintercept = as.numeric(min(timegen_forecast$ds)), 
                 linetype = "dashed", color = "gray50")
  }
  
  # TimeGen residuals table
  output$timegen_residuals <- renderDT({
    # Try to load residuals file
    if (file.exists("residuals_timeG.csv")) {
      timegen_residuals_df <- read.csv("residuals_timeG.csv")
    } else {
      # Create placeholder if file not found
      forecast_dates <- seq.Date(from = as.Date("2012-11-02"), by = "week", length.out = 8)
      timegen_residuals_df <- data.frame(
        date = forecast_dates,
        residual = runif(8, -0.6, 0.6)
      )
    }
    
    datatable(
      timegen_residuals_df,
      options = list(pageLength = 8, scrollX = TRUE, dom = 'tp'),
      rownames = FALSE
    ) %>% 
      formatRound(columns = 'residual', digits = 4)
  })
  
  # Metrics table
  output$metrics_table <- renderTable({
    # Try to load metrics from RDS files with the specified names
    tswge_ase <- if (file.exists("tswge_ase.rds")) readRDS("tswge_ase.rds") else 0.10
    tswge_wmae <- if (file.exists("tswge_wmae.rds")) readRDS("tswge_wmae.rds") else 0.29
    prophet_ase <- if (file.exists("prophet_ase.rds")) readRDS("prophet_ase.rds") else 0.74
    prophet_wmae <- if (file.exists("prophet_wmae.rds")) readRDS("prophet_wmae.rds") else 0.48
    timegen_ase <- if (file.exists("timeG_ase.rds")) readRDS("timeG_ase.rds") else 0.88
    timegen_wmae <- if (file.exists("timeG_wmae.rds")) readRDS("timeG_wmae.rds") else 0.64
    
    data.frame(
      Score = c("ASE", "WMAE"),
      TSWGE = c(tswge_ase, tswge_wmae),
      PROPHET = c(prophet_ase, prophet_wmae),
      'TIMEGEN' = c(timegen_ase, timegen_wmae)
    )
  }, bordered = TRUE, striped = TRUE, hover = TRUE, align = 'c')
}

# Run the application
shinyApp(ui = ui, server = server)