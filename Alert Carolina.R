library(shiny)
library(leaflet)


set.seed(1883)
n_points <- 5
chapel_hill_lon <- -79.0558
chapel_hill_lat <- 35.9132
radius <- 0.02
points <- data.frame(
  lon = chapel_hill_lon + runif(n_points, -radius, radius),
  lat = chapel_hill_lat + runif(n_points, -radius, radius),
  time = sample(seq(as.POSIXct('2023-01-01'), as.POSIXct(Sys.Date()), by = 'day'), n_points),
  category = NA,
  more_info=c("Info for point 1", "Info for point 2", "Info for point 3", "Info for point 4", "Info for point 5") 
)

# Add different case categories
case_categories <- c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational", "Crime Alert")

# Add case categories column to points data frame
points$category <- sample(case_categories, n_points, replace = TRUE)

ui <- fluidPage(
  tags$head(
    tags$style(
      HTML("
           .county-checkbox-wrapper {
             margin-top: 20px;
           }
           .county-checkbox-wrapper .checkbox-group {
             width: 300px;
           }
           .sidebar {
             padding: 20px;
           }
           .title-wrapper {
             text-align: center;
             border: 2px solid yellow; 
             padding: 10px;
             font-weight: bold;  
             text-decoration: underline;  
           }
           ")
    )
  ),
  div(style = "text-align: center;",
      div(class = "title-wrapper",
          h2("Alert Carolina Map")
      )
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "county-checkbox-wrapper",
          checkboxGroupInput(
            inputId = "selectedCounty",
            label = HTML("Select Categories: <em>(For new starters, please select one county option only or it will induce an error to the R Shiny. For categories selection, you can select single or multiple choices.)</em>"),
            choices = c("Carrboro", "Durham", "Apex", "Cary", "Holly Springs",'Graham','Burlington','Raleigh',
                        'Garner','Sanford','Wake Forest','Greensboro','Asheboro'),
            selected = "Carrboro"
          )
      ),
      div(class = "checkbox-input-wrapper",
          checkboxGroupInput(
            inputId = "selectedCategories",
            label = "Select Categories:",
            choices = c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational"),
            selected = c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational")
          )
      )
    ),
    mainPanel(
      leafletOutput(outputId = "distPlot", width = "700px", height = "800px")
    )
  ),
  
  sliderInput(
    inputId = 'date',
    label = "Date Range:",
    min = as.Date('2023-01-01'),
    max = Sys.Date(),
    value = c(as.Date('2023-01-15'), as.Date('2023-02-15')),
    width = '900px',
    timeFormat = "%Y-%m-%d"
  )
)


server <- function(input, output) {
  # Define Icons
  icons <- list(
    "Emergency Warning" = "exclamation",
    "Adverse Conditions" = "exclamation-triangle",
    "Crime Alert" = "shield",
    "Informational" = "info"
  )
  colors <- list(
    "Emergency Warning" = "red",
    "Adverse Conditions" = "orange",
    "Crime Alert" = "blue",
    "Informational" = "green"
  )
  
  # Create a function to generate an AwesomeIcon object based on the category
  makeAwesomeIcon <- function(category) {
    leaflet::makeAwesomeIcon(
      icon = icons[[category]],
      markerColor = colors[[category]],
      library = "fa"
    )
  }
  
  output$distPlot <- renderLeaflet({
    filtered_points <- points[points$time >= as.POSIXct(input$date[1]) &
                                points$time <= as.POSIXct(input$date[2]) &
                                points$category %in% input$selectedCategories, ]
    
    # Add Country Coordinates to activate map with the choice of county
    county_coordinates <- list(
      'Carrboro' = c(lon = -79.0849, lat = 35.9100),
      'Durham' = c(lon = -78.8986, lat = 35.9940),
      'Apex' = c(lon = -78.8503, lat = 35.7327),
      'Cary' = c(lon = -78.7811, lat = 35.7915),
      'Holly Springs' = c(lon = -78.8336, lat = 35.6513),
      'Graham' = c(lon = -79.3932, lat = 36.0686),
      'Burlington' = c(lon = -79.4378, lat = 36.0956),
      'Raleigh' = c(lon = -78.6382, lat = 35.7796),
      'Garner' = c(lon = -78.6142, lat = 35.7113),
      'Sanford' = c(lon = -79.1803, lat = 35.4799),
      'Wake Forest' = c(lon = -78.5361, lat = 35.9799),
      'Greensboro' = c(lon = -79.7910, lat = 36.0726),
      'Asheboro' = c(lon = -79.8269, lat = 35.7079)
    )
    
    selected_county <- input$selectedCounty
    county_lon <- county_coordinates[[selected_county]][1]
    county_lat <- county_coordinates[[selected_county]][2]
    
    leaflet_proxy <- leaflet() %>%
      addTiles() %>%
      setView(lng = county_lon, lat = county_lat, zoom = 12)
    
    iconList <- lapply(filtered_points$category, makeAwesomeIcon)
    
    for (i in seq_along(iconList)) {
      leaflet_proxy <- addAwesomeMarkers(
        leaflet_proxy,
        lng = filtered_points$lon[i], lat = filtered_points$lat[i],
        icon = iconList[[i]],
        label = format(filtered_points$time[i], "%Y-%m-%d"),
        popup = paste0("<strong>", format(filtered_points$time[i], "%Y-%m-%d"), "</strong><br>",
                       filtered_points$category[i], "<br>",
                       filtered_points$more_info[i])
      )
    }
    
    # Add a legend to the map
    leaflet_proxy <- leaflet_proxy %>%
      addLegend(
        position = "topright",
        title = "Categories",
        colors = c(colors["Emergency Warning"], colors["Adverse Conditions"], colors["Crime Alert"], colors["Informational"]),
        labels = c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational"),
        opacity = 1
      )
    
    leaflet_proxy
  })
}

shinyApp(ui, server)