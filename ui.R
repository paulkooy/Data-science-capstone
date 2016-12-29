#
#   This application provide a user interface for my text prediction model.
#   The application is part of the Coursera Data Specialization Capstone project.
#   
#   Shiny User Interface code
#
#   Author: Paul van der Kooy
#   Date:   December'16
#
#   Load required libraries
#
library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Text prediction application"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            print("Enter your text here and the main window will display the full text and the predicted next word(s)."),
            textInput("inputText", "Your text:")
        ),

        # Show the predicted text plus prediction score
        mainPanel(

            h4(textOutput("predictionScore")),
            br(),
            h4(print(paste("The input text:"))),
            textOutput("outputText"),
            br(),
            h4(print(paste("The predicted words:"))),
            textOutput("prediction1"),
            textOutput("prediction2"),
            textOutput("prediction3"),
            textOutput("prediction4")
        )
    )
))
