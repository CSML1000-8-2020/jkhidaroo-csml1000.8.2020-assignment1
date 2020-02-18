# Load the required libraries
library('e1071')
library('shiny')

# Concatenate data file subdir and name.
full_file_name <- paste("./input/diagnosis_data.csv",sep="")
show(full_file_name)

# Load full data
diagnosis_data_full = read.csv(full_file_name)
breastcancer_data <- read.csv(full_file_name)

# Remove Lines for the ID and Null X final Columns
diagnosis_data_full <- diagnosis_data_full[2:32]
diagnosis_data_full <- diagnosis_data_full[c(1,9,29,8,24,22)]
diagnosis_data_full$diagnosis = factor(diagnosis_data_full$diagnosis)
names(diagnosis_data_full)

# Scaling the dataset for models that require it
diagnosis_data_scaled <- diagnosis_data_full
diagnosis_data_scaled <- scale(diagnosis_data_scaled[2:6])
names(diagnosis_data_full)

# Split the data into a train set and a test set
diagnosis_data_train <- diagnosis_data_full[1:426,]
diagnosis_data_test <- diagnosis_data_full[427:569,]
# Split the scaled version as well
diag_data_scaled_train <- diagnosis_data_scaled[1:426,]
diag_data_scaled_test <- diagnosis_data_scaled[427:569,]


# 4. D) Data Modeling - kernelSVM
dt <- diagnosis_data_train[,-c(1)]
diag_kSVM_model <- svm(formula = diagnosis_data_train$diagnosis ~.,
                       data = diagnosis_data_train[,-c(1)],
                       type = 'C-classification',
                       kernel = 'radial')

# 5. D) Data Evaluation - kernelSVM
# Test the kernel SVM model.
diag_kSVM_pre <- predict(diag_kSVM_model, diagnosis_data_test[,-c(1)])
diag_kSVM_pre

diag_kSVM_pre[1]

#### Define UI
ui <- fluidPage(
  
                #ui <- fillPage(theme = shinytheme("lumen"),
                titlePanel("Diagnosis Predictor"),
                sidebarLayout(
                  sidebarPanel(
                    # Inputs
                    p("Select the following"),
                    sliderInput("perimeter_worst", label = "Perimeter Worst", min = 1, max = 30, value = 1, step = 0.001),
                    sliderInput("concave.points_worst", label = "Concave Points Worst", min = 5, max = 50, value = 5, step = 0.01),
                    sliderInput("radius_worst", label = "Radius Worst", min = 25, max = 225, value = 25, step = 0.01),
                    sliderInput("concave.points_mean", label = "Concave Points Mean", min = 100, max = 2600, value = 100, step = 0.1),
                    sliderInput("concavity_mean", label = "Concavity Mean", min = 0.01, max = 0.25, value = 0.01, step = 0.00001),
                    # Button to click for generating prediction
                    actionButton("predict", "Predict")
                  ),
                  # Output: Text Giving a Prediction
                  mainPanel(
                    h3(textOutput(outputId = "Pred")),
                    tableOutput("values")
                  ),
                )
)


#### Define server function
server <- function(input, output, session) {
  
  diagnosis <- "NA"
  
  observeEvent(input$predict, {

    data <- reactive({
      req(diagnosis)
      data.frame(diagnosis=NA,
                 perimeter_worst=input$perimeter_worst,
                 concave.points_worst=input$concave.points_worst,
                 radius_worst=input$radius_worst,
                 concave.points_mean=input$concave.points_mean,
                 concavity_mean=input$concavity_mean)
    })

    sliderValues <- reactive({
      
      data.frame(
                Name = c("Perimeter Worst",
                 "Concave Points Worst",
                 "Radius Worst",
                 "Concave Points Mean",
                 "Concavity Mean"),
        Value = as.character(c(input$perimeter_worst,
                               input$concave.points_worst,
                               input$radius_worst,
                               input$concave.points_mean,
                               input$concavity_mean)),
        stringsAsFactors = FALSE)
    })

    str_text <- reactive({
      str(data())
    })
    output$ShowStr <- renderPrint(str_text())
    

    pred <- reactive({
      predict(diag_kSVM_model,data())
    })
  
    
    output$Pred <- renderPrint("pred()")

    output$values <- renderTable({
      sliderValues()
    })
    
  })
}

#### Run Shiny App
shinyApp(ui = ui, server = server)
