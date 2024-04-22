#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Water tank prices"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("min_size", "Minimum capacity", min = 10, max = 100, value = 20),
          sliderInput("max_size", "Maximum capacity", min = 101, max = 250, value = 100),
          numericInput("cap_hat", "hypothetical capacity", value = 50),
          textOutput("predPrice")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  df <- reactive({read_csv('../tank_prices.csv') %>% 
     filter(capacity >= input$min_size, capacity <= input$max_size)})
  model <- reactive({lm(price ~ capacity, data=df())})
  output$predPrice <- renderText({
    paste0(
      "expected price: $", 
      round(
    predict(model(), newdata = data.frame(capacity = input$cap_hat)) , digits = 2
      )
    )
  })
  output$distPlot <- renderPlot({
      #ggplot(df, aes(capacity, price, alpha=gal_per_dol, color=log(floor_space))) +
      ggplot(df(), aes(capacity, price, color=log(floor_space))) +
        geom_point() +
        geom_vline(aes(xintercept = input$cap_hat)) +
        geom_hline(aes(yintercept = predict(model(), newdata = data.frame(capacity = input$cap_hat)))) +
        geom_smooth(method="lm")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
