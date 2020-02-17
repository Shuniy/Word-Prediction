library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("sandstone"),
        themeSelector(),
    
    titlePanel("Text Prediction"),
    p("This app that takes an input phrase (multiple words) in a text box and outputs a prediction of the next word."),
    
    sidebarLayout(
        sidebarPanel(
            h2("Instructions:"), 
            h5("1. Enter a word or words in the text box."),
            h5("2. The predicted next word prints below it in blue."),
            h5("3. No need of submit button."),
            h5("4. A question mark means no prediction, typically due to mis-spelling or stopwords."),
            h5("5. Documentation and Plots are in Documentation Tab.")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Predict",
                         textInput("user_input", h3("Your Input:"), 
                                   value = "Input Here"),
                         h3("Predicted Word 1:"),
                         h4(em(span(textOutput("ngram_output1"), style = "color:blue"))),
                
                         h3("Predicted Word 2:"),
                         h4(em(span(textOutput("ngram_output2"), style = "color:blue"))),
            
                         h3("Predicted Word 3:"),
                         h4(em(span(textOutput("ngram_output3"), style = "color:blue")))
                ),
                
                tabPanel("Exploratory Analysis", includeHTML("www/Milestone-Report-1.html")),
                
                tabPanel("Documentation", includeHTML("www/Documentation.html"))
                
            )   
        )
    )
)
