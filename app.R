library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(plotly)

retrieve_data <- function() {
    url <- 'https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-place-totals.csv'
    
    df <- read_csv(url, col_types = cols(
        date = col_date(format = ""),
        county = col_character(),
        fips = col_character(),
        place = col_character(),
        confirmed_cases = col_double(),
        note = col_character(),
        x = col_double(),
        y = col_double()
    ))
    
    df <- df %>%
        filter(.,!is.na(x) | !is.na(y)) %>%
        filter(place != 'Pacifica') %>%
        # filter(date %in% mdy('5/31/2020')) %>%
        mutate(
            label = glue::glue(
                '<h4>{place}</h4>
        <b>County:</b> {county} <br>
        <b>Confirmed Cases:</b> {confirmed_cases}'
            )
        )
}

retrieve_data2 <- function() {
    url <- 'https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/latimes-county-totals.csv'
    data <- read_csv(url)
}

data <- retrieve_data()
data2 <- retrieve_data2()

data2 <- data2 %>%
    group_by(county) %>%
    mutate(
        seven_day_average = (new_confirmed_cases +
                                 lag(new_confirmed_cases, n = 1) +
                                 lag(new_confirmed_cases, n = 2) +
                                 lag(new_confirmed_cases, n = 3) +
                                 lag(new_confirmed_cases, n = 4) +
                                 lag(new_confirmed_cases, n = 5) +
                                 lag(new_confirmed_cases, n = 6)) / 7
    )


# data <- read_csv('data.csv')
# data2 <- read_csv('data2.csv')

header <- dashboardHeader(
    title = 'California COVID-19'
)

sidebar <- dashboardSidebar(
    disable = T
)

body <- dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
    fluidRow(box(
        selectInput(
            "county",
            label = h4("Selected County"),
            choices = unique(data$county),
            selected = "Los Angeles"
        ),
        width = 12,
        
        sliderInput(
            "date",
            label = h4("Selected Date"),
            min = min(data$date),
            max = max(data$date),
            value = max(data$date)
        )
    )), 
    
    fluidRow(
        valueBoxOutput('total_cases', width = 3),
        valueBoxOutput('new_cases', width = 3),
        valueBoxOutput('deaths', width = 3),
        valueBoxOutput('new_deaths', width = 3),
    ),
    fluidRow(box(leafletOutput('map'), width = 12)),
    fluidRow(box(plotlyOutput('new_case_hist'), width = 12)),
    fluidRow(box(
        DT::dataTableOutput('table'),
        width = 12,
        footer = glue::glue('Source: LA Times Datadesk')
    ))
    
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)

server <- function(input, output, session) {
    
    selected_county <- reactive({
        input$county
    })
    
    selected_date <- reactive({
        input$date
    })
    
    observe({
        
        max <- data %>%
            filter(county == selected_county()) %>%
            distinct(date) %>%
            pull() %>% max()
        
        min <- data %>%
            filter(county == selected_county()) %>%
            distinct(date) %>%
            pull() %>% min()
        
        updateSliderInput(
            session,
            'date',
            max = max,
            min = min
        )
    })
    
    output$map <- renderLeaflet({

        temp <- data %>%
            filter(county == selected_county()) %>%
            filter(date == max(date))

        colorPal <- colorNumeric('viridis', temp$confirmed_cases)

        leaflet(temp) %>%
            addTiles() %>%
            addCircleMarkers(
                lng = ~ x,
                lat = ~ y,
                popup = ~ label,
                radius = ~ sqrt(confirmed_cases) / 2,
                color = ~ colorPal(confirmed_cases)
            ) %>%
            addLegend(
                pal = colorPal,
                values = ~confirmed_cases,
                title = 'Confirmed Cases'
            )

    })
    
    observe({

        temp <- data %>%
            filter(county == selected_county()) %>%
            filter(date == selected_date())

        colorPal <- colorNumeric('viridis', temp$confirmed_cases)

        leafletProxy('map', data = temp) %>%
            clearMarkers() %>%
            clearControls() %>%
            addCircleMarkers(
                lng = ~ x,
                lat = ~ y,
                popup = ~ label,
                radius = ~ sqrt(confirmed_cases) / 2,
                color = ~ colorPal(confirmed_cases)
            ) %>%
            addLegend(
                pal = colorPal,
                values = ~confirmed_cases,
                title = 'Confirmed Cases'
            )

    })
    
    output$table <- DT::renderDataTable({
        
        out <- data %>%
            filter(county == selected_county()) %>%
            filter(date == selected_date()) %>%
            select(
                date,
                place,
                confirmed_cases
            ) %>%
            arrange(
                desc(confirmed_cases)
            )
        
        DT::datatable(out, colnames = c('Date', 'Location', 'Cases'))
        
    })
    
    output$total_cases <- renderValueBox({
        
        total_cases <- data2 %>%
            filter(
                county == selected_county() & 
                    date == max(date)
            ) %>% pull(confirmed_cases)
        
        valueBox(total_cases, subtitle = 'Total Cases', 
                 color = 'blue',
                 icon = icon('fa-viruses'))
            
            
    })
    
    # output$last_update <- renderText({
    #     
    #     date <- data %>%
    #         filter(
    #             county == selected_county() & 
    #                 date == max(date)
    #         ) %>%
    #         pull(date) %>% as.character() %>% .[[1]]
    #     
    #     out <- glue::glue('Data last updated: {date}')
    #     
    #     return(out)
    #     
    # })
    
    output$new_case_hist <- renderPlotly({
        
        temp <- data2 %>% filter(county == selected_county())
        
        plot_ly(temp) %>%
            add_trace(x = ~ date, y = ~ new_confirmed_cases, type = 'bar', name = 'New Confirmed Cases') %>%
            add_trace(x = ~ date, y = ~ seven_day_average, type = 'scatter', mode = 'lines', name = '7 Day Average') %>%
            layout(
                xaxis = list(title = '', fixedrange = T),
                yaxis = list(title = 'New Confirmed Cases', fixedrange = T),
                legend = list(orientation = 'h',
                              xanchor = 'center',
                              x = .5)
            ) %>%
            config(displayModeBar = T)
        
        
        
    })
    
    output$new_cases <- renderValueBox({
        
        out <- data2 %>%
            filter(
                county == selected_county() & 
                    date == max(date)
            ) %>%
            pull(new_confirmed_cases)
        
        valueBox(out, subtitle = 'New Confirmed Cases', color = 'yellow')
    })
    
    output$deaths <- renderValueBox({
        
        out <- data2 %>%
            filter(
                county == selected_county() & 
                    date == max(date)
            ) %>%
            pull(deaths)
        
        valueBox(out, subtitle = 'Deaths', color = 'orange')
    })
    
    output$new_deaths <- renderValueBox({
        
        out <- data2 %>%
            filter(
                county == selected_county() & 
                    date == max(date)
            ) %>%
            pull(new_deaths)
        
        valueBox(out, subtitle = 'New Deaths', color = 'red')
    })
    
}

shinyApp(ui, server)

















