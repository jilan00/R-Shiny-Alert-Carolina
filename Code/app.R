library(shiny)
library(leaflet)

# Generate 5 random points near Chapel Hill with time information
set.seed(1883)
n_points <- 5
chapel_hill_lon <- -79.0558
chapel_hill_lat <- 35.9132
radius <- 0.02
points <- data.frame(
  lon = chapel_hill_lon + runif(n_points, -radius, radius),
  lat = chapel_hill_lat + runif(n_points, -radius, radius),
  time = sample(seq(as.POSIXct('2023-01-01'), as.POSIXct(Sys.Date()), by = 'day'), n_points),category=NA
)
  
  #Add different case categories
 case_categories <- c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational", "Crime Alert")
 
 #Add case categories column to points data frame
 points$category<-sample(case_categories,n_points,replace=TRUE)



ui <- fluidPage(
  titlePanel("Alert Carolina Map"),
  
  
  # Sliderinput: select date range between Jan 1st 2023 to today
  sliderInput(
    inputId = 'date',
    label = "Date Range:",
    min = as.Date('2023-01-01'),
    max = Sys.Date(),
    value = c(as.Date('2023-01-15'), as.Date('2023-02-15')),
    width = '600px',
    timeFormat = "%Y-%m-%d"
  ),
  
  # Plot leaflet object
  leafletOutput(outputId = "distPlot", width = "700px", height = "500px"),
  
  checkboxGroupInput(
    inputId = "selectedCategories",
    label = "Select Categories:",
    choices = c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational"),
    selected = c("Emergency Warning", "Adverse Conditions", "Crime Alert", "Informational")
  ),
  
  # Sidebar Panel
  sidebarPanel(
    h4('County'),
    selectInput(
      inputId = 'selectedcounty',
      label = NULL,
      choices = list(
        'Carrboro', 'Durham', 'Apex', 'Cary', 'Holly Springs',
        'Graham', 'Burlington', 'Raleigh', 'Garner', 'Sanford',
        'Wake Forest', 'Greensboro', 'Asheboro'
      ),
      selected = 'Carrboro'
    )
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
    filtered_points <- points[points$time >= as.POSIXct(input$date[1]) & points$time <= as.POSIXct(input$date[2])] & points$category %in% input$selectedCategories,]
  
    
    
    #Add Country Coordinates to activate map with the choice of country
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
    
    selected_county <- input$selectedcounty
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
        label = format(filtered_points$time[i], "%Y-%m-%d")
      )
    }
    
    #Add a legend to the map
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

