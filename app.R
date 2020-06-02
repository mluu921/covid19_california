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

header <- dashboardHeader(
    title = 'California COVID-19'
)

sidebar <- dashboardSidebar(
    disable = T
    
)

body <- dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 100px) !important;}"),
    fluidRow(
        box(
            selectInput(
                "county",
                label = h4("Selected County"),
                choices = unique(data$county),
                selected = unique(data$county)[[1]]
            ), width = 12,
            footer = textOutput('last_update')
        )
    ),
    fluidRow(
             valueBoxOutput('total_cases', width = 3),
             valueBoxOutput('new_cases', width = 3),
             valueBoxOutput('deaths', width = 3),
             valueBoxOutput('new_deaths', width = 3),
             ), 
    fluidRow(box(leafletOutput('map'), width = 12)),
    fluidRow(box(plotlyOutput('new_case_hist'), width = 12)),
    fluidRow(box(DT::dataTableOutput('table'), width = 12, 
                 footer = glue::glue('Source: LA Times Datadesk')))
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)

server <- function(input, output) {
    
    selected_county <- reactive({
        input$county
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
    
    output$table <- DT::renderDataTable({
        
        out <- data %>%
            filter(county == selected_county()) %>%
            filter(date == max(date)) %>%
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
    
    output$last_update <- renderText({
        
        date <- data2 %>%
            filter(
                county == selected_county() & 
                    date == max(date)
            ) %>%
            pull(date) %>% as.character()
        
        out <- glue::glue('Data last updated: {date}')
        
        return(out)
        
    })
    
    output$new_case_hist <- renderPlotly({
        
        temp <- data2 %>% filter(county == selected_county())
        
        plot_ly(temp, x = ~date, y = ~new_confirmed_cases, type = 'bar') %>%
            layout(
                xaxis = list(title = 'Date', fixedrange = T),
                yaxis = list(title = 'New Confirmed Cases', fixedrange = T)
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

















