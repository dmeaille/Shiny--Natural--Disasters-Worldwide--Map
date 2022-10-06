# @author : Davi Méaille
# Created : 06/10/2022
# Last modification : 06/10/2022
# Description : 
# 
# This file displays in a shinyApp the
# data on natural disasters that are 
# provided by the website EM-DAT. 


################### DATA PREPARATION

# packages
setwd("C:/Users/davim/Onedrive/Desktop/Informatique/R/Archive/Shiny App - natural disasters")

require(readxl)
require(tidyverse)
require(shiny)
require(leaflet)
require(ggplot2)
require(sf)
require(spdplyr)


# dataset

data <- read_excel("data.xlsx", skip = 6, col_names = T)

data <- data %>% select(-Glide) # the variable is empty

data[,c(1:3, 21:22,39:42)] <- lapply(data[,c(1:3, 21:22,39:42)], as.numeric) # to change the variable types
data[,c(4:20)] <- lapply(data[,c(4:20)], as.factor)

levels(data$ISO)[c(54,56)] <- "DEU" # maybe 54 DDR et 56 DFR for Germany 

lapply(data[,c(21,39:44)], `*`, 1000) # to match with GDP scale




################### SHINY APP ---- 


# we get country level polygons from getMap
worldMap <- rworldmap::getMap()

worldMap@data <- worldMap@data %>% select(NAME)

# complete the remaining countries 
#which(!(worldMap$NAME %in% unique(data$Country)))

# find_country <- function(char, obj){
#   l = stringr::str_split(char)
#   lapply(l, grep, x = obj)
# }
# 



ui <- fluidPage(

    # Application title
    titlePanel("Natural disaster on earth"),

    
    sidebarLayout(
        sidebarPanel(
          
          helpText("Description: This shinyApp displays the data on natural disasters retrieved from", a("EM-DAT homepage", href = "https://public.emdat.be/")),
          checkboxGroupInput(inputId = "type_disaster", 
                             label = "Choose the type of disasters to display",
                             choices = levels(data$`Disaster Type`),
                             selected = unique(data$`Disaster Type`[data$`Disaster Subgroup` == "Meteorological"])),
          checkboxGroupInput(inputId = "continent",  # to choose one or several continents 
                             label = "Which continents to display ?",
                             choices = levels(data$Continent), 
                             selected = "Europe"),
          sliderInput(inputId="years", 
                      label = "Between which years ?",
                      min = 1920, 
                      max = 2022, 
                      value = c(1960, 2020))


        ),

        
        mainPanel(
          
          leafletOutput("disaster_map"),
          br(),
          br(),
          br(),
          br(),
          checkboxGroupInput("by", 
                             label = "By Population/by GDP",
                             choices = list("By population", "By GDP")),
          br(),
          radioButtons(inputId = "cost", 
                       label = "What kind of values do you want to display ?",
                       choices = colnames(data[,34:44]),
                       inline = T),
          radioButtons(inputId = "computation", 
                       label = "What kind of computation do you want to perform ?",
                       choices = list("Maximum" = "max", "Minimum" = "min", "Total sum" = "sum", "Mean" = "mean", "Median" = "median"),
                       inline = T, 
                       selected = "sum"),
          br(),
          br(),
          plotOutput("plot")
      
            
          )
        )
    )




server <- function(input, output) {

    dattt <- reactive(data %>% filter(.data[["Continent"]] == input$continent, .data[["Disaster Type"]] == input$type_disaster, .data[["Year"]] >= input$years[1] ) %>%
                        group_by(.data[["Country"]]) %>% 
                        summarize(sum =  do.call( eval(input$computation), list(.data[[input$cost]], na.rm = T))) %>% 
                        select(.data[["Country"]], .data[["sum"]]))
    
    
    ##### Code for the leaflet map
    
    data_map <- reactive({
      tigris::geo_join(worldMap, dattt(), by_sp = "NAME", by_df = "Country", how = "inner") 
    })
     
     colorpal = reactive({
       colorNumeric(
         palette = "Blues",
         domain = data_map()$sum)
     })
     
     output$`disaster_map` <- renderLeaflet({
       leaflet(data_map()) %>%
         addProviderTiles(providers$Stamen.Terrain) %>%
         addPolygons(stroke = F, smoothFactor =  0.2, fillOpacity = 0.6)
    
     })

     observe({
       pal <- colorpal()
       
       leafletProxy("disaster_map", data = data_map()) %>%
         clearShapes() %>%
         addPolygons(stroke = F, smoothFactor =  0.2, fillOpacity = 0.6,
                    fillColor = ~pal(sum)
         )
     })
     
     
     
     ### Code for the reactive ggplot 
     
     data_plot <- reactive(data %>% 
                            filter(.data[["Continent"]]  == input$continent, .data[["Disaster Type"]] == input$type_disaster, .data[["Year"]] >= input$years[1]) %>% 
                            count(.data[["Year"]], .data[["Continent"]])
                          )
     
     output$plot <- renderPlot(ggplot(data_plot(), aes(x=.data[["Year"]], y = .data[["n"]],  .data[["Continent"]], color = .data[["Continent"]], alpha=I(0.8))) + 
                                 geom_line(linetype = 5, size = 1) +
                                 ggtitle("Number of natural disaster per year and per continent") + 
                                 theme_bw())
     
     
     
}


# Run the application 
shinyApp(ui = ui, server = server)







