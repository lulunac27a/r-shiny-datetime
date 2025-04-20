# import Shiny library
library(shiny)
# import lubridate library
library(lubridate)
# define UI component
ui <- fluidPage(
    titlePanel("Date and Time in R Shiny"),
    sidebarLayout(
        sidebarPanel(
            textInput("year", "Year", value = format(Sys.Date(), "%Y")), # year input
            sliderInput(
                "month",
                "Month",
                min = 1,
                max = 12,
                value = 1,
                step = 1
            ), # month input
            sliderInput("day", "Day", min = 1, max = 31, value = 1, step = 1), # day of month input
            sliderInput("hour", "Hour", min = 0, max = 23, value = 0, step = 1), # hour input
            sliderInput(
                "minute",
                "Minute",
                min = 0,
                max = 59,
                value = 0,
                step = 1
            ), # minute input
            sliderInput(
                "second",
                "Second",
                min = 0,
                max = 59,
                value = 0,
                step = 1
            ), # second input
            sliderInput(
                "millisecond",
                "Millisecond",
                min = 0,
                max = 999,
                value = 0,
                step = 1
            ) # millisecond input
        ),
        mainPanel(
            textOutput("iso_format"), # ISO format output
            textOutput("us_format"), # US format output
            textOutput("eu_format"), # EU format output
            textOutput("date_time"), # date and time output
            textOutput("weekday"), # weekday output
            textOutput("month"), # month output
            textOutput("locale_date_time"), # locale date and time output
            textOutput("day_of_month"), # day of month output
            textOutput("date"), # date output
            textOutput("week_based_year"), # week based year output
            textOutput("hour"), # hour output
            textOutput("day_of_year"), # day of year output
            textOutput("month_number"), # month output
            textOutput("minute"), # minute output
            textOutput("hour_12_clock"), # 12-hour clock output
            textOutput("second"), # second output
            textOutput("weekday_format"), # weekday format output
            textOutput("week_of_year"), # week of year output
            textOutput("year"), # year output
            textOutput("timezone"), # timezone output
            textOutput("millisecond"), # millisecond output
            textOutput("quarter"), # quarter output
            textOutput("semester"), # semester output
            textOutput("week_jan_1"), # weeks since January 1 output
            textOutput("week_iso_8601"), # ISO 8601 week output
            textOutput("epidemiological_week"), # epidemiological week output
            textOutput("weekday_lubridate"), # weekday lubridate output
            textOutput("day_of_month_lubridate"), # day of month lubridate output
            textOutput("day_of_year_lubridate"), # day of year lubridate output
            textOutput("timezone_lubridate"), # timezone lubridate output
            textOutput("local_timezone"), # local timezone output
            textOutput("decimal_date"), # decimal date output (fraction of year)
            textOutput("progress") # progress date output)
        )
    )
)
# define server component
server <- function(input, output, session) {
    # check if a year is a leap year
    is_leap_year <- function(year) {
        (year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0)
    }
    # calculate the number of days in a month
    days_in_month <- function(month, year) {
        # if month is February, check if it's a leap year
        if (month == 2) {
            # if it's a leap year, return 29 days, otherwise
            # return 28 days
            if (is_leap_year(year)) {
                return(29)
            } else {
                return(28)
            }
            # if month is April, June, September, or November,
            # return 30 days
        } else if (month %in% c(4, 6, 9, 11)) {
            return(30)
            # otherwise, return 31 days
        } else {
            return(31)
        }
    }
    max_days <- reactive({
        # set max value of day slider to the number of days in
        # the month
        days_in_month(as.integer(input$month), as.integer(input$year))
    })
    observeEvent(input$month, {
        # observe month input and update day slider max value
        updateSliderInput(session, "day", max = max_days())
    })
    observeEvent(input$year, {
        # observe year input and update day slider max value
        updateSliderInput(session, "day", max = max_days())
    })
    date_time <- reactive({
        # create date and time object from full date and time
        # string
        as.POSIXct(
            sprintf(
                "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                as.integer(input$year),
                as.integer(input$month),
                as.integer(input$day),
                as.integer(input$hour),
                as.integer(input$minute),
                as.integer(input$second),
                as.integer(input$millisecond)
            ),
            tz = "UTC"
        )
    })
    local_date_time <- reactive({
        # create local date and time object from full date and
        # time string
        as.POSIXct(
            sprintf(
                "%04d-%02d-%02d %02d:%02d:%02d.%03d",
                as.integer(input$year),
                as.integer(input$month),
                as.integer(input$day),
                as.integer(input$hour),
                as.integer(input$minute),
                as.integer(input$second),
                as.integer(input$millisecond)
            ),
            tz = Sys.timezone()
        )
    })
    output$date_time <- renderText({
        # date and time output
        paste("Date and Time:", date_time(), sep = "")
    })
    output$iso_format <- renderText({
        # ISO format output
        paste(
            "ISO Format: ",
            format(date_time(), "%Y-%m-%dT%H:%M:%OS3Z"),
            sep = ""
        )
    })

    output$us_format <- renderText({
        # US format output
        paste(
            "US Format: ",
            format(date_time(), "%m/%d/%Y %I:%M:%S %p"),
            sep = ""
        )
    })

    output$eu_format <- renderText({
        # EU format output
        paste("EU Format: ", format(date_time(), "%d/%m/%Y %H:%M:%S"), sep = "")
    })
    output$weekday <- renderText({
        # weekday function output
        paste(
            "Weekday: ",
            weekdays(date_time()),
            " (",
            format(date_time(), "%A"),
            ")",
            " (",
            format(date_time(), "%a"),
            ")",
            sep = ""
        )
    })
    output$month_number <- renderText({
        # month output
        paste(
            "Month: ",
            format(date_time(), "%B"),
            " (",
            format(date_time(), "%b"),
            ")",
            sep = ""
        )
    })
    output$locale_date_time <- renderText({
        # locale date and time output
        paste("Locale Date and Time: ", format(date_time(), "%c"), sep = "")
    })
    output$day_of_month <- renderText({
        # day of month output
        paste("Day of Month: ", format(date_time(), "%d"), sep = "")
    })
    output$date <- renderText({
        # date output
        paste("Date: ", format(date_time(), "%D"), sep = "")
    })
    output$week_based_year <- renderText({
        # week based year output
        paste("Week Based Year: ", format(date_time(), "%G"), sep = "")
    })
    output$hour <- renderText({
        # hour output
        paste(
            "Hour: ",
            format(date_time(), "%H"),
            " (",
            format(date_time(), "%I"),
            " ",
            format(date_time(), "%p"),
            ")",
            sep = ""
        )
    })
    output$day_of_year <- renderText({
        # day of year output
        paste("Day of Year: ", format(date_time(), "%j"), sep = "")
    })
    output$month <- renderText({
        # month output
        paste("Month: ", format(date_time(), "%m"), sep = "")
    })
    output$minute <- renderText({
        # minute output
        paste("Minute: ", format(date_time(), "%M"), sep = "")
    })
    output$hour_12_clock <- renderText({
        # 12-hour clock output
        paste("12-Hour Clock: ", format(date_time(), "%r"), sep = "")
    })
    output$second <- renderText({
        # second output
        paste("Second: ", format(date_time(), "%S"), sep = "")
    })
    output$weekday_format <- renderText({
        # weekday format output
        paste("Weekday Format: ", format(date_time(), "%u"), sep = "")
    })
    output$week_of_year <- renderText({
        # week of year output
        paste(
            "Week of Year: ",
            format(date_time(), "%V"),
            " (Week of Year for Sunday as first day of week): ",
            format(date_time(), "%U"),
            " (Week of Year for Monday as first day of week): ",
            format(date_time(), "%W"),
            sep = ""
        )
    })
    output$year <- renderText({
        # year output
        paste("Year: ", format(date_time(), "%Y"), sep = "")
    })
    output$timezone <- renderText({
        # timezone output
        paste(
            "Timezone: ",
            format(date_time(), "%Z"),
            " (",
            format(date_time(), "%z"),
            ")",
            sep = ""
        )
    })
    output$millisecond <- renderText({
        # millisecond output
        paste("Millisecond: ", format(date_time(), "%L"), sep = "")
    })
    output$quarter <- renderText({
        # quarter output
        paste("Quarter: ", quarter(date_time()), sep = "")
    })
    output$semester <- renderText({
        # semester output
        paste("Semester: ", semester(date_time()), sep = "")
    })
    output$week_jan_1 <- renderText({
        # weeks since January 1 output
        paste("Weeks Since January 1: ", week(date_time()), sep = "")
    })
    output$week_iso_8601 <- renderText({
        # ISO 8601 week output
        paste("ISO 8601 Week: ", isoweek(date_time()), sep = "")
    })
    output$epidemiological_week <- renderText({
        # epidemiological week output
        paste("Epidemiological Week: ", epiweek(date_time()), sep = "")
    })
    output$weekday_lubridate <- renderText({
        # weekday lubridate output
        paste(
            "Weekday (lubridate): ",
            wday(date_time(), label = TRUE),
            " (",
            wday(date_time(), label = FALSE),
            ")",
            sep = ""
        )
    })
    output$day_of_month_lubridate <- renderText({
        # day of month lubridate output
        paste("Day of Month (lubridate): ", mday(date_time()), sep = "")
    })
    output$day_of_year_lubridate <- renderText({
        # day of year lubridate output
        paste("Day of Year (lubridate): ", yday(date_time()), sep = "")
    })
    output$timezone_lubridate <- renderText({
        # timezone lubridate output
        paste("Timezone: ", tz(date_time()), sep = "")
    })
    output$local_timezone <- renderText({
        # local timezone output
        paste(
            "Local Timezone: ",
            Sys.timezone(),
            " (",
            tz(local_date_time()),
            ")",
            sep = ""
        )
    })
    output$decimal_date <- renderText({
        # decimal date output (fraction of year)
        paste("Decimal Date: ", decimal_date(date_time()), sep = "")
    })
    output$progress <- renderText({
        # progress date output
        paste(
            c(
                "Year Progress: ",
                "Month Progress: ",
                "Week Progress: ",
                "Day Progress: ",
                "Hour Progress: ",
                "Minute Progress: ",
                "Second Progress: "
            ),
            cyclic_encoding(
                date_time(),
                c("year", "month", "week", "day", "hour", "minute", "second"),
                c("abs")
            ) /
                (2 * pi),
            sep = ""
        )
    })
}
# run the Shiny web app server
shinyApp(ui = ui, server = server)
