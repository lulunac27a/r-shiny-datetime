library(shiny)

ui <- fluidPage(
    titlePanel("Date and Time in R Shiny"),
    sidebarLayout(
        sidebarPanel(
            textInput("year", "Year", value = "2022"),
            sliderInput("month", "Month", min = 1, max = 12, value = 1, step = 1),
            sliderInput("day", "Day", min = 1, max = 31, value = 1, step = 1),
            sliderInput("hour", "Hour", min = 0, max = 23, value = 0, step = 1),
            sliderInput("minute", "Minute", min = 0, max = 59, value = 0, step = 1),
            sliderInput("second", "Second", min = 0, max = 59, value = 0, step = 1),
            sliderInput("millisecond", "Millisecond", min = 0, max = 999, value = 0, step = 1)
        ),
        mainPanel(
            textOutput("date_time")
        )
        )
    )

server <- function(input, output) {
    date_time <- reactive({
        as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d.%03d", as.integer(input$year), as.integer(input$month), as.integer(input$day), as.integer(input$hour), as.integer(input$minute), as.integer(input$second), as.integer(input$millisecond)), tz = "UTC")
    })
    output$date_time <- renderText({
        paste("Date and Time:", date_time())
    })
}

shinyApp(ui = ui, server = server)