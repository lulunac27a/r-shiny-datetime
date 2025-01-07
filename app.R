# import Shiny library
library(shiny)
# define UI component
ui <- fluidPage(titlePanel("Date and Time in R Shiny"), sidebarLayout(sidebarPanel(textInput("year",
    "Year", value = "2022")  #year input
, sliderInput("month",
    "Month", min = 1, max = 12, value = 1, step = 1)  #month input
,
    sliderInput("day", "Day", min = 1, max = 31, value = 1, step = 1)  #day of month input
, sliderInput("hour", "Hour", min = 0,
        max = 23, value = 0, step = 1)  #hour input
, sliderInput("minute",
        "Minute", min = 0, max = 59, value = 0, step = 1)  #minute input
, sliderInput("second", "Second", min = 0,
        max = 59, value = 0, step = 1)  #second input
, sliderInput("millisecond",
        "Millisecond", min = 0, max = 999, value = 0, step = 1)  #millisecond input
), mainPanel(textOutput("iso_format")  #ISO format output
, textOutput("us_format")  #US format output
,
    textOutput("eu_format")  #EU format output
, textOutput("date_time")  #date and time output
)))
# define server component
server <- function(input, output) {
    date_time <- reactive({
        # create date and time object from full date and
        # time string
        as.POSIXct(sprintf("%04d-%02d-%02d %02d:%02d:%02d.%03d",
            as.integer(input$year), as.integer(input$month),
            as.integer(input$day), as.integer(input$hour), as.integer(input$minute),
            as.integer(input$second), as.integer(input$millisecond)),
            tz = "UTC")
    })
    output$date_time <- renderText({
        # date and time output
        paste("Date and Time:", date_time())
    })
    output$iso_format <- renderText({
        # ISO format output
        paste("ISO Format:", format(date_time(), "%Y-%m-%dT%H:%M:%OS3Z"))
    })

    output$us_format <- renderText({
        # US format output
        paste("US Format:", format(date_time(), "%m/%d/%Y %I:%M:%S %p"))
    })

    output$eu_format <- renderText({
        # EU format output
        paste("EU Format:", format(date_time(), "%d/%m/%Y %H:%M:%S"))
    })
}
# run the Shiny web app server
shinyApp(ui = ui, server = server)
