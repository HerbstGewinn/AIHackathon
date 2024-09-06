install.packages("shiny")
install.packages("leaflet")
install.packages("dplyr")
install.packages("DT")

library(shiny)
library(leaflet)
library(DT)
library(dplyr)

#shiny dataset erstellen mit Training_PLZ_for_2024 und plz_results_for_2024
shiny_data <-plz_results_for_2024 %>% select(Plz,Month,Forecast)

shiny_data <- inner_join(shiny_data, post2coord, by = "Plz")
shiny_data$Jahr <-2024

#join trainingsdataset mit long und lat
training_shiny <- inner_join(Training_PLZ_for_2024, post2coord, by = "Plz")

#change Months to numbers
shiny_data <- shiny_data %>%
  mutate(Monat = case_when(
    Monat == "Jan" ~ 1,
    Monat == "Feb" ~ 2,
    Monat == "Mar" ~ 3,
    Monat == "Apr" ~ 4,
    Monat == "May" ~ 5,
    Monat == "Jun" ~ 6,
    Monat == "Jul" ~ 7,
    Monat == "Aug" ~ 8,
    Monat == "Sep" ~ 9,
    Monat == "Oct" ~ 10,
    Monat == "Nov" ~ 11,
    Monat == "Dec" ~ 12,
    TRUE ~ as.numeric(NA)  # Handles cases where the month is not recognized
  ))

#umbenennen
shiny_data<-shiny_data %>% rename(Monat= Month)
shiny_data<-shiny_data %>% rename(Gewicht_Total= Forecast)
shiny_data<-shiny_data %>% select(Monat, Plz, Gewicht_Total, Jahr, lat, long)
#rbind training_shiny und shiny_data

shiny_data<-rbind(training_shiny, shiny_data) 

#AFTER binding the data, there was a mistake

#Now comes RShiny app by itself


# Define the User Interface for the Shiny app
library(shiny)
library(leaflet)
library(dplyr)

# Define the User Interface for the Shiny app
library(shiny)
library(leaflet)
library(dplyr)

library(shiny)
library(leaflet)
library(dplyr)

ui <- fluidPage(
  titlePanel("Visualisierung pro PLZ"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("yearSelector", 
                  "Wähle ein Jahr:",
                  choices = NULL), # Will be updated with server logic
      sliderInput("monthSlider",
                  "Wähle einen Monat:",
                  min = 1,
                  max = 12,
                  value = 1,
                  step = 1,
                  ticks = TRUE,
                  sep = '',
                  pre = 'Monat: ')
    ),
    mainPanel(
      leafletOutput("map"),
      dataTableOutput("table")
    )
  )
)

server <- function(input, output, session) {
  
  # Assuming 'shiny_data' is your dataframe that's already available in the global environment
  # Update Year selector based on the dataset
  observe({
    updateSelectInput(session, "yearSelector", choices = sort(unique(shiny_data$Jahr)), selected = max(shiny_data$Jahr))
  })
  
  # Reactive expression to filter data based on selected year and month
  filteredData <- reactive({
    data <- shiny_data %>%
      filter(Jahr == input$yearSelector, Monat == input$monthSlider) %>%
      # Replace NA values in crucial columns with default or estimated values
      mutate(long = ifelse(is.na(long), mean(long, na.rm = TRUE), long),
             lat = ifelse(is.na(lat), mean(lat, na.rm = TRUE), lat),
             Total_Gewicht = ifelse(is.na(Total_Gewicht), 0, Total_Gewicht))  # You may choose to handle NAs differently
    
    # Optional: remove rows with NA in critical columns if not manageable by imputation
    # data <- data[!is.na(data$long) & !is.na(data$lat) & !is.na(data$Total_Gewicht), ]
    data
  })
  
  # Render the leaflet map based on the filtered data
  output$map <- renderLeaflet({
    if(nrow(filteredData()) == 0) {
      return(leaflet() %>%
               addTiles() %>%
               setView(lng = 9.9937, lat = 53.5511, zoom = 12))
    }
    leaflet() %>%
      addTiles() %>%
      setView(lng = 9.9937, lat = 53.5511, zoom = 12) %>%
      addCircles(data = filteredData(),
                 lng = ~long, lat = ~lat,
                 weight = 1, # Border weight for circles
                 radius = ~sqrt(Total_Gewicht) * 50, # Adjust radius based on weight
                 popup = ~paste("Gewicht: ", Total_Gewicht),
                 fillColor = "blue", fillOpacity = 0.5)
  })
  
  # Render the data table based on the filtered data
  output$table <- renderDataTable({
    filteredData()
  })
}

# Run the Shiny app with the specified UI and server logic
shinyApp(ui, server)
