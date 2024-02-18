# Pengda Wang 
# Synthetic data generation

#### Library
library(shinyjs)
library(MASS) # For mvrnorm
library(DT)   # For dataTableOutput
library(MASS)
library(shiny)
library(reticulate)
library(ggplot2)
library(shinycssloaders)
library(moments)

# Normal distribution dataset
# Set seeds to reproduce results
set.seed(0)

# Generate data for independent variable x (normal distribution)
x <- rnorm(1000, mean = 50, sd = 10)
# Generate data for dependent variable y (normal distribution, partially dependent on x)
y <- 2 * x + rnorm(1000, mean = 0, sd = 40)
# Create a dataframe to hold x and y
normal_data <- data.frame(x, y)

##########################################################
# Skew-distributed datasets
# Seeds set to reproduce results
set.seed(0)
# Generate independent variables (uniformly distributed)
x <- rnorm(1000, mean = 50, sd = 10)
# Generate dependent variable (non-linear relationship plus random noise)
y <- exp(runif(1000, min=-3, max=3)) + rnorm(1000, mean=0, sd=1)
# Create a data frame to hold x and y
skewed_data <- data.frame(x, y)

##########################################################
calculate_stats <- function(data, label) {
  stats <- data.frame(
    Method = label,
    Variable = colnames(data),
    Mean = colMeans(data, na.rm = TRUE),
    SD = apply(data, 2, sd, na.rm = TRUE),
    Skewness = apply(data, 2, skewness, na.rm = TRUE),
    Kurtosis = apply(data, 2, kurtosis, na.rm = TRUE)
  )
  return(stats)
}
##########################################################

#### UI
ui <- fluidPage(
  titlePanel("Synthetic Data Generation"),
  tabsetPanel(
    tabPanel("Introduction",
             h1(HTML("Welcome to our <strong>Synthetic Data Generation Shiny App</strong>, your new ally in navigating the exciting world of synthetic data!"), 
               style = "font-size:20px; font-family: 'Open Sans', serif;"),
             p(HTML("<strong>Why synthetic data?</strong> Data sharing promotes the benefits of open science: transparency, reproducibility, and the ability for others to conduct additional statistical analyses of value. However, the important need for individual privacy can completely prevent data sharing. Synthetic data emerges as a solution. Synthetic datasets can be shared in that they mirror the actual dataset as closely as possible without compromising individual-level information."), 
               style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p("One of the traditional hurdles in creating high-quality synthetic data has been the reliance on specific data distribution assumptions, such as multivariate normality. Such assumptions can be too restrictive by failing to reflect complex distributions found in real-world datasets. However, innovative machine learning methods, such as Generative Adversarial Networks (GANs), can be trained on any given dataset, yielding a synthetic dataset that reflects the same intricacies.", style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p(HTML("<strong>What can this app do?</strong> Dive into this synthetic data playground with curiosity!"),style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p(HTML("<em>“Upload Data” tab.</em> Here, we have provided datasets that follow both normal and non-normal distributions that you can create synthetic data with. Or to create synthetic data from your own data for research purposes, you can first upload your own data."), style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p(HTML("<em>“Synthetic Data” tab.</em> Here, you can check and download synthetic data using machine learning (GANs). However, the app also shows how synthetic data look using correlation matrices off the assumption of multivariate normality."), style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p(HTML("<em>“Statistical Difference” tab.</em> Here, you can compare and contrast the original data with its synthetic counterparts (from both GANs and correlations): i.e., descriptive statistics, density plots, and scatter plots."), style = "font-size:20px; font-family: 'Open Sans', serif;"), 
             p("⚠️ A gentle reminder: Our demonstration is a demonstration of the potential for synthetic data. Keep in mind that generating synthetic data, especially with GANs, involves different parameter settings.", style = "font-size:20px; font-family: 'Open Sans', serif;"),
             p("⚠️ Currently not support categorical variables.", style = "font-size:20px; font-family: 'Open Sans', serif;"),
             tags$br()
    ),
    
    tabPanel("Upload data",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("dataset", "Selecting a dataset",
                              choices = list("Upload your data file" = "Upload File", 
                                             "Normal Distribution" = "Normal", 
                                             "Skewness" = "Skewness"),
                              selected = "Upload File"),
                 numericInput("numDataPoints", "Number of Synthetic Data Points:", 
                              value = 1000, min = 1, step = 500),
                 uiOutput("dataEntryUI"),
                 actionButton("loadData", "Load data", class = "btn-primary")
               ),
               mainPanel(
                 uiOutput("data_status"),
                 DTOutput("viewData") %>% withSpinner(color="#0dc5c1")
               )
             )
    ),
    
    tabPanel("Synthetic data",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("synthDisplayMethod", "Displaying Synthetic Data Methods", 
                              choices = c("GAN Method", "Correlation Matrix Method"), 
                              selected = "GAN Method"), 
                 downloadButton("downloadData", "Download data")
               ),
               mainPanel(
                 uiOutput("synthetic_data_status"),
                 DTOutput("viewSyntheticData") %>% withSpinner(color="#0dc5c1")
               )
             )
    ),
    
tabPanel("Statistical differences",
         sidebarLayout(
           sidebarPanel(
             radioButtons("statsOption", "Select Display Content",
                          choices = list("Descriptive statistics" = "stats", 
                                         "Density plot" = "densityPlots", 
                                         "Scatter plot" = "scatterPlots"),
                          selected = "stats"),
             uiOutput("variableSelectionUI")
           ),
           mainPanel(
             uiOutput("statsContent"),
             fluidRow(
               column(6, plotOutput("scatterPlot")),
               column(6, plotOutput("scatterPlotOriginal"))
             ),
             fluidRow(
               column(6, plotOutput("scatterPlotGAN")),
               column(6, plotOutput("scatterPlotCorrMatrix"))
              
             )
             
           )
         )
    )

  )
)

#### Server
server <- function(input, output, session) {
  
  original_data <- reactiveVal()
  synthetic_data_gan <- reactiveVal()
  synthetic_data_corr_matrix <- reactiveVal()
  data_loaded <- reactiveVal(FALSE)
  
  output$dataEntryUI <- renderUI({
    if(input$dataset == "Upload File") {
      fileInput('file1', 'Upload your data files', accept = c('.csv', '.txt'))
    }
  })
  
  observeEvent(input$loadData, {
    if(input$dataset == "Normal") {
      original_data(normal_data)
    } else if(input$dataset == "Skewness") {
      original_data(skewed_data)
    } else if(input$dataset == "Upload File") {
      file <- input$file1
      req(file)
      original_data(read.csv(file$datapath))
    }
    data_loaded(TRUE)
  })
  
  output$data_status <- renderUI({
    if(!data_loaded()) {
      tagList(
        p("Please load the data first.")
      )
    } else {
      NULL
    }
  })
  
  output$synthetic_data_status <- renderUI({
    if(!data_loaded()) {
      tagList(
        p("Please load the data first.")
      )
    } else {
      NULL
    }
  })
  
  output$viewData <- renderDT({
    req(original_data())
    datatable(original_data(), options = list(pageLength = 20))
  })
  
  # Generate synthetic data when original data is available
  observe({
    data <- original_data()
    req(data)
    
    # GAN method
    file_path_gan <- tempfile()
    write.csv(data, file_path_gan, row.names = FALSE)
    num_points <- input$numDataPoints
    
    # set python env
    python_path <- "python" 
    
    command_gan <- sprintf("%s /Users/WangP/Desktop/shiny/gan.py %s %d", python_path, file_path_gan, num_points)
    system(command_gan)
    
    synthetic_data_gan(read.csv("synthetic_data_gan.csv"))
    
    ################### more complex method
    mu <- colMeans(data)
    cor_matrix <- cor(data)
    std_devs <- sapply(data, sd)
    sigma <- matrix(nrow = ncol(data), ncol = ncol(data))
    
    for (i in 1:ncol(data)) {
      for (j in 1:ncol(data)) {
        sigma[i, j] <- cor_matrix[i, j] * std_devs[i] * std_devs[j]
      }
    }
    
    synthetic_data <- mvrnorm(n = num_points, mu = mu, Sigma = sigma)
    synthetic_data_df <- as.data.frame(synthetic_data)
    colnames(synthetic_data_df) <- colnames(data)
    synthetic_data_corr_matrix(synthetic_data_df)
    
  })
  
  output$viewSyntheticData <- renderDT({
    if(input$synthDisplayMethod == "GAN Method") {
      req(synthetic_data_gan())
      datatable(synthetic_data_gan(), options = list(pageLength = 20))
    } else {
      req(synthetic_data_corr_matrix())
      datatable(synthetic_data_corr_matrix(), options = list(pageLength = 20))
    }
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("synthetic_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data_to_download <- if(input$synthDisplayMethod == "GAN Method") {
        synthetic_data_gan()
      } else {
        synthetic_data_corr_matrix()
      }
      write.csv(data_to_download, file, row.names = FALSE)
    }
  )
  
  output$variableSelectionUI <- renderUI({
    if(input$statsOption == "scatterPlots") {
      req(original_data())
      varNames <- names(original_data())
      tagList(
        selectInput("scatterX", "Select X-axis variable", choices = varNames, selected = varNames[1]),
        selectInput("scatterY", "Select Y-axis variable", choices = varNames, selected = varNames[2])
      )
    }else if(input$statsOption == "densityPlots") {
      req(original_data())
      varNames <- names(original_data())
      selectInput("selectedVariable", "Select variable for Density Plot", choices = varNames, selected = varNames[1])
    }
  })
  
  
  # densityPlots
  output$densityPlotOriginal <- renderPlot({
    req(input$statsOption == "densityPlots")
    req(input$selectedVariable, original_data())
    
    ggplot(original_data(), aes(x = .data[[input$selectedVariable]])) +
      geom_density(fill = "blue", alpha = 0.5) +
      ggtitle(paste("Density plot of", input$selectedVariable, "(Original)")) +
      xlab(input$selectedVariable) +
      ylab("Density") +
      theme_minimal()
  })
  
  output$densityPlotGAN <- renderPlot({
    req(input$statsOption == "densityPlots")
    req(input$selectedVariable, synthetic_data_gan())
    
    ggplot(synthetic_data_gan(), aes(x = .data[[input$selectedVariable]])) +
      geom_density(fill = "red", alpha = 0.5) +
      ggtitle(paste("Density plot of", input$selectedVariable, "(GAN Method)")) +
      xlab(input$selectedVariable) +
      ylab("Density") +
      theme_minimal()
  })
  
  output$densityPlotCorrMatrix <- renderPlot({
    req(input$statsOption == "densityPlots")
    req(input$selectedVariable, synthetic_data_corr_matrix())
    
    ggplot(synthetic_data_corr_matrix(), aes(x = .data[[input$selectedVariable]])) +
      geom_density(fill = "green", alpha = 0.5) +
      ggtitle(paste("Density plot of", input$selectedVariable, "(Correlation Matrix Method)")) +
      xlab(input$selectedVariable) +
      ylab("Density") +
      theme_minimal()
  })
  
  output$densityPlot <- renderPlot({
    req(input$statsOption == "densityPlots")
    req(input$selectedVariable, original_data(), synthetic_data_gan(), synthetic_data_corr_matrix())
    
    ggplot() +
      geom_density(data = original_data(), aes(x = .data[[input$selectedVariable]], fill = "Original"), alpha = 0.5) +
      geom_density(data = synthetic_data_gan(), aes(x = .data[[input$selectedVariable]], fill = "GAN"), alpha = 0.5) +
      geom_density(data = synthetic_data_corr_matrix(), aes(x = .data[[input$selectedVariable]], fill = "Correlation Matrix"), alpha = 0.5) +
      ggtitle(paste("Density plot of", input$selectedVariable)) +
      xlab(input$selectedVariable) +
      ylab("Density") +
      theme_minimal() +
      scale_fill_manual(values = c("Original" = "blue", "GAN" = "red", "Correlation Matrix" = "green"),
                        name = "Dataset",
                        breaks = c("Original", "GAN", "Correlation Matrix"),
                        labels = c("Original Data", "GAN Method", "Correlation Matrix Method"))
  })
  
  
  ########################
  # scater plot
  output$scatterPlot <- renderPlot({
    # Ensure that scatterplots are generated only when a scatterplot is selected
    req(input$statsOption == "scatterPlots")
    req(input$scatterX, input$scatterY, original_data(), synthetic_data_gan(), synthetic_data_corr_matrix())
    
    # Plot scatterplots and add labels
    ggplot() +
      geom_point(data = original_data(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]], color = "Original"), alpha = 0.5) +
      geom_point(data = synthetic_data_gan(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]], color = "GAN"), alpha = 0.5) +
      geom_point(data = synthetic_data_corr_matrix(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]], color = "Correlation Matrix"), alpha = 0.5) +
      ggtitle(paste("Scatter plot of", input$scatterX, "vs.", input$scatterY)) +
      xlab(input$scatterX) +
      ylab(input$scatterY) +
      theme_minimal() +
      scale_color_manual(values = c("Original" = "blue", "GAN" = "red", "Correlation Matrix" = "green"), 
                         name = "Dataset", 
                         breaks = c("Original", "GAN", "Correlation Matrix"),
                         labels = c("Original Data", "GAN Method", "Correlation Matrix Method"))
  })
  ###########
  output$scatterPlotOriginal <- renderPlot({
    req(input$statsOption == "scatterPlots")
    req(input$scatterX, input$scatterY, original_data())
    
    ggplot(original_data(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]])) +
      geom_point(color = "blue", alpha = 0.5) +
      ggtitle(paste("Scatter plot of", input$scatterX, "vs.", input$scatterY, "(Original)")) +
      xlab(input$scatterX) +
      ylab(input$scatterY) +
      theme_minimal()
  })
  
  output$scatterPlotGAN <- renderPlot({
    req(input$statsOption == "scatterPlots")
    req(input$scatterX, input$scatterY, synthetic_data_gan())
    
    ggplot(synthetic_data_gan(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]])) +
      geom_point(color = "red", alpha = 0.5) +
      ggtitle(paste("Scatter plot of", input$scatterX, "vs.", input$scatterY, "(GAN Method)")) +
      xlab(input$scatterX) +
      ylab(input$scatterY) +
      theme_minimal()
  })
  
  output$scatterPlotCorrMatrix <- renderPlot({
    req(input$statsOption == "scatterPlots")
    req(input$scatterX, input$scatterY, synthetic_data_corr_matrix())
    
    ggplot(synthetic_data_corr_matrix(), aes(x = .data[[input$scatterX]], y = .data[[input$scatterY]])) +
      geom_point(color = "green", alpha = 0.5) +
      ggtitle(paste("Scatter plot of", input$scatterX, "vs.", input$scatterY, "(Correlation Matrix Method)")) +
      xlab(input$scatterX) +
      ylab(input$scatterY) +
      theme_minimal()
  })
  ###############
  
  output$statsContent <- renderUI({
    req(original_data(), synthetic_data_gan(), synthetic_data_corr_matrix())
    
    if(input$statsOption == "stats") {
      tableOutput("statsTable")
    } else if (input$statsOption == "densityPlots") {
      
      fluidRow(
        column(6, plotOutput("densityPlot")),
        column(6, plotOutput("densityPlotOriginal")),
        column(6, plotOutput("densityPlotGAN")),
        column(6, plotOutput("densityPlotCorrMatrix"))
        
      )
      
    }
      
  })
  
  output$statsTable <- renderTable({
    req(input$statsOption == "stats")
    stats_orig <- calculate_stats(original_data(), "Original")
    stats_gan <- calculate_stats(synthetic_data_gan(), "GAN")
    stats_corr <- calculate_stats(synthetic_data_corr_matrix(), "Correlation Matrix")
    stats_combined <- rbind(stats_orig, stats_gan, stats_corr)
    stats_combined <- stats_combined[order(stats_combined$Variable, stats_combined$Method),]
    return(stats_combined)
    
  })
  
}

#### run shiny
shinyApp(ui = ui, server = server)





