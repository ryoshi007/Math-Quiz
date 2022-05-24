library(shiny)
library(shinyjs)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

result_table <- data.frame(
  Question=character(), Input=numeric(), Answer=numeric()  
)

generateQuestion <- function() {
  numbers <- sample(1:50, 2)
  operation <- sample(c("+", "-", "*"), 1)
  question <- paste(numbers[1], operation, numbers[2], sep=" ")
  answer <- calculator(numbers[1], numbers[2], operation)
  result_table[nrow(result_table) + 1,] <<- c(question, -999, answer)
  return (question)
}

calculator <- function(int1, int2, operation) {
  if (operation == "+") {
    return (int1 + int2)
  } else if (operation == '-') {
    return (int1 - int2)
  } else {
    return (int1 * int2)
  }
}

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Math Quiz"),
  
  sidebarLayout(
    sidebarPanel(
      p("Welcome to Math Quiz! 10 randomly generated questions have to be answered by the user and click 'Submit' button once you finish it.
      You can replay the quiz again by clicking 'Generate New Questions' button. Enjoy!"), 
      br(),
      p("I notice there is a bug that cannot be reproduced when testing the app on the RStudio. Please click 'Generate New Questions' button
        when you enter the website for the first time. Sorry for causing such inconvenience :("),
      br(),
      textInput("q1", generateQuestion()),
      textInput("q2", generateQuestion()),
      textInput("q3", generateQuestion()),
      textInput("q4", generateQuestion()),
      textInput("q5", generateQuestion()),
      textInput("q6", generateQuestion()),
      textInput("q7", generateQuestion()),
      textInput("q8", generateQuestion()),
      textInput("q9", generateQuestion()),
      textInput("q10", generateQuestion()),
      actionButton("submit", "Submit"),
      actionButton("reset_button", "Generate New Questions")
    ),
    
    mainPanel(
      tableOutput("table"),
      h3(textOutput("final_result"))
    )
  )
)

server <- function(input, output, session) {
  count <- 0
  
  hide("table")
  hide("final_result")
  
  observeEvent(input$submit, {
    show("table")
    show("final_result")
    disable("q1")
    disable("q2")
    disable("q3")
    disable("q4")
    disable("q5")
    disable("q6")
    disable("q7")
    disable("q8")
    disable("q9")
    disable("q10")
    disable("submit")
  })
  
  output$table <- renderTable({
    input = list(input$q1,input$q2,input$q3,input$q4,input$q5,input$q6,input$q7,input$q8,input$q9,input$q10)
    for (i in 1:10) {
      result_table$Input[i] <<- input[i]
      if ((result_table$Input[i]) == result_table$Answer[i]) {
        count <<- count + 1
      }
    }
    result_table
  })
  
  output$final_result <- renderText({
    if (input$submit) {
      output <- paste("Your final result is ", count, " /10")
      output
    }
  })
  
  observeEvent(input$reset_button, {
    updateTextInput(session, "q1", label = generateQuestion(), value = "")
    updateTextInput(session, "q2", label = generateQuestion(), value = "")
    updateTextInput(session, "q3", label = generateQuestion(), value = "")
    updateTextInput(session, "q4", label = generateQuestion(), value = "")
    updateTextInput(session, "q5", label = generateQuestion(), value = "")
    updateTextInput(session, "q6", label = generateQuestion(), value = "")
    updateTextInput(session, "q7", label = generateQuestion(), value = "")
    updateTextInput(session, "q8", label = generateQuestion(), value = "")
    updateTextInput(session, "q9", label = generateQuestion(), value = "")
    updateTextInput(session, "q10", label = generateQuestion(), value = "")
    result_table <<- result_table[-c(1:10), ]
    count <<- 0
    
    hide("table")
    hide("final_result")
    
    enable("q1")
    enable("q2")
    enable("q3")
    enable("q4")
    enable("q5")
    enable("q6")
    enable("q7")
    enable("q8")
    enable("q9")
    enable("q10")
    enable("submit")
  })
  
}

shinyApp(ui = ui, server = server)
