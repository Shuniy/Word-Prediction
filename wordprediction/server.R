library(shiny)

source(file = "predictmodelapp.R")

server <- function(input, output) {
    
    output$ngram_output1 <- renderText({
        ngrams1(input$user_input)})
            
     output$ngram_output2 <- renderText({
        ngrams2(input$user_input)})
                    
     output$ngram_output3 <- renderText({
        ngrams3(input$user_input)})

}
