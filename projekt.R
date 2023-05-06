library(shiny) # Main library
library(ggplot2) # Plots
library(dplyr) # Data manipulate
library(shinythemes)
library(plotly)

library(sf)
library(rnaturalearth)
library(ggspatial) 
library(ggrepel) 

options(scipen=999)

CO_data <- read.csv("./data.csv",  header= TRUE)

CO_data2 <- CO_data[,-c(1,2,3)]
col_names = colnames(CO_data2)


countries <- unique(CO_data['country'])
years <- unique(sort(CO_data$year))
world <- ne_countries(scale = 'medium', returnclass = 'sf')
country_list <- unique(sort(world$name))

CO_data_filtered <- subset(CO_data, country %in% country_list)

only_co2_and_year <- CO_data[,c('year', 'country', 'co2')]


ui <- navbarPage(
  titlePanel(title=div(img(src="https://siw.amu.edu.pl/__data/assets/file/0004/162751/logo_wersja-podstawowa_granat_1.jpg", width = 50, height = 50), 'explore CO2 data')),
  tabPanel("Linear Chart", 
           sidebarLayout(
             sidebarPanel(
               selectInput('country', 
                           'Select Country', 
                           selected = 'Afghanistan',
                           choices = countries
               ),
               selectInput('category', 
                           'Select Category', 
                           selected = 'population',
                           choices = col_names
               )
             ),
             
             mainPanel(
               plotlyOutput('linear_chart')
             ),
           )
  ),
  tabPanel(
    'GDP',
    plotlyOutput('gdp')
  ),  
  tabPanel("Map: CO2 by year", 
           sidebarLayout(
             sidebarPanel(
               selectInput('year', 
                           'Select year', 
                           selected = '2011',
                           choices = years
               )
             ),
             
             mainPanel(
               plotlyOutput('map'),
             ),
           )
  ), 
  tabPanel("Map: statistics in year 2011", 
           sidebarLayout(
             sidebarPanel(
               selectInput('category2', 
                           'Select category', 
                           selected = 'population',
                           choices = col_names
               )
             ),
             
             mainPanel(
               plotlyOutput('map2'),
             ),
           )
  ), 
  tabPanel(
    'Biggest CO2 Production',
    fluidRow(
      column(6,plotlyOutput(outputId="the_most_1")),  
      column(6,plotlyOutput(outputId="the_most_2")),
      column(6,plotlyOutput(outputId="the_most_3"))
    )
  ),
  tabPanel(
    'Smallest CO2 production',
    fluidRow(
      column(6,plotlyOutput(outputId="the_least_1")),  
      column(6,plotlyOutput(outputId="the_least_2")),
      column(6,plotlyOutput(outputId="the_least_3"))
    )
  ),
  
  tabPanel(
    'Data',
    DT::dataTableOutput('tableData')
  ),  
  
  tabPanel(
    'Theme',
    shinythemes::themeSelector(),
    theme = shinythemes::shinytheme('flatly'),
  )
)


server <- function(input, output, session) {
  output$tableData <- DT::renderDataTable({
    CO_data %>% 
      filter(country == input$country) %>% 
      DT::datatable()
  })
  
  output$linear_chart <- renderPlotly({
    CO_data %>%
      filter(country == input$country) %>%
      ggplot(aes(x = year, y = get(input$category))) + 
      ylab(input$category) +
      geom_line()
  })
  output$gdp <- renderPlotly({
    CO_data_filtered %>%
      filter(year == 2011) %>%
      ggplot(aes(x = gdp, y = co2, label = country)) + 
      geom_line() +
      geom_point() +
      ylim(0,10000) +
      ggtitle('Placement of countries by CO2 and GDP production')
  })
  output$map = renderPlotly({
    countries_data <- filter(only_co2_and_year, year==input$year)
    data <- merge(world, countries_data, by.y="country", by.x="name")
    ggplot(data = data) +
      geom_sf(aes(fill = co2, label = name)) +
      scale_fill_viridis_c(option = "plasma", trans = "sqrt") # colorblind-friendly palette
  })
  output$map2 = renderPlotly({
    countries_data <- filter(CO_data, year==2011)
    data2 <- merge(world, countries_data, by.y="country", by.x="name")
    ggplot(data = data2) +
      geom_sf(aes(fill = get(input$category2), label = input$category2)) +
      labs(title=input$category2) +
      scale_fill_discrete(labels = input$category2) +
      scale_fill_viridis_c(option = "plasma", trans = "sqrt") # colorblind-friendly palette
  })
  output$the_most_1 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_max(n=7, order_by = co2_per_gdp) %>%
      ggplot(aes(x=country, y=co2_per_gdp, fill=country)) +
      xlab('Country') +
      ylab('CO2 per GDP [kilograms per dollar]') +
      ggtitle('the biggest CO2 production per GDP') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
  output$the_most_2 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_max(n=7, order_by = co2_per_capita) %>%
      ggplot(aes(x=country, y=co2_per_capita, fill=country), custom) +
      xlab('Country') +
      ylab('CO2 per capita [tonnes per person]') +
      ggtitle('the biggest CO2 production per capita') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
  output$the_most_3 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_max(n=7, order_by = co2) %>%
      ggplot(aes(x=country, y=co2, fill=country)) +
      xlab('Country') +
      ylab('CO2 overall [million tonnes]') +
      ggtitle('the biggest CO2 production overall') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
  output$the_least_1 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_min(n=7, order_by = co2_per_gdp) %>%
      ggplot(aes(x=country, y=co2_per_gdp, fill=country)) +
      xlab('Country') +
      ylab('CO2 per GDP [kilograms per dollar]') +
      ggtitle('the smallest CO2 production per GDP') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
  output$the_least_2 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_min(n=7, order_by = co2_per_capita) %>%
      ggplot(aes(x=country, y=co2_per_capita, fill=country)) +
      xlab('Country') +
      ylab('CO2 per capita [tonnes per person]') +
      ggtitle('the smallest CO2 production per capita') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
  output$the_least_3 = renderPlotly({
    CO_data_filtered %>%
      filter(year==2011) %>%
      slice_min(n=7, order_by = co2) %>%
      ggplot(aes(x=country, y=co2, fill=country)) +
      xlab('Country') +
      ylab('CO2 overall [million tonnes]') +
      ggtitle('the smallest CO2 production overall') +
      theme(axis.text.x = element_blank()) +
      geom_bar(stat='identity')
  })
}

shinyApp(ui = ui, server = server)