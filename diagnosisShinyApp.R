#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library('e1071')

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    #ui <- fillPage(theme = shinytheme("lumen"),
    titlePanel("Diagnosis Predictor"),
    sidebarLayout(
        sidebarPanel(
            # Inputs
            p("Select from the following features the parameter value, then click on the Predict button at the bottom of the table"),
            sliderInput("perimeter_worst", label = "Perimeter Worst", min = 0, max = 300, value = 0, step = 0.1),
            sliderInput("concave.points_worst", label = "Concave Points Worst", min = 0, max = 0.4, value = 0, step = 0.0001),
            sliderInput("radius_worst", label = "Radius Worst", min = 0, max = 50, value = 0, step = 0.01),
            sliderInput("concave.points_mean", label = "Concave Points Mean", min = 0, max = 0.25, value = 0, step = 0.00001),
            sliderInput("concavity_mean", label = "Concavity Mean", min = 0, max = 0.5, value = 0, step = 0.00001),
            # Button to click for generating prediction
            actionButton("predict", "Predict")
        ),
        # Output: Text Giving a Prediction
        mainPanel(
            h1("Diagnosis Result"),
            h3(textOutput(outputId = "Pred")),
            tableOutput("values")
        ),
    )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    diagnosis <- "NA"
    
    observeEvent(input$predict, {
        
        
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
        
        data <- reactive({
            req(diagnosis)
            data.frame(
                perimeter_worst=input$perimeter_worst,
                concave.points_worst=input$concave.points_worst,
                radius_worst=input$radius_worst,
                concave.points_mean=input$concave.points_mean,
                concavity_mean=input$concavity_mean)
        })
        
        str_text <- reactive({
            str(data())
        })
        output$ShowStr <- renderPrint(str_text())
        
        pred <- reactive({
            predict(diag_kSVM_model,data())
        })
        
        DIAG <- toString(pred())
        if (DIAG == "B"){
            DIAG <- "Benign"
            font <- 'green'
        } else {
            DIAG <- "Malignant"
            font <- 'red'
        }
        output$Pred <- renderText(DIAG)
        
        output$values <- renderTable({
            sliderValues()
        })
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
