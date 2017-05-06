library(shiny)
library(leaflet)
library(leaflet.extras)
library(RColorBrewer)
library(dplyr)

london <- readRDS("london.rds")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
    sliderInput("range", "Total Ambulance Incidents \n(London city)", min(london$Events), max(london$Events),
      value = range(london$Events), step = 200,
    ),

         selectInput('month',
                  'Selected Month',
                  #choices = c('Mar.2016', 'Apr.2016', 'Jun.2016', 'Jul.2016'),
                  choices = unique(london$Month) ,
                  selected = c('Jul.2016'),
                  multiple = FALSE,
                  selectize = FALSE),
                  
    
    selectInput("colors", "Color Scheme",
      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
    ),
        
    checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {

  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    london = subset(london, Month %in% input$month)
    london[london$Events >= input$range[1] & london$Events <= input$range[2],]
  
  })
  
  
  filterdBeat <- reactive({
  data1 = subset(london, Month %in% input$month)
   })

  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  colorpal <- reactive({
    colorNumeric(input$colors, london$Events)
  })

  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that limit london city

    leaflet(london) %>% addTiles() %>%
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })

  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) 
  observe({
    pal <- colorpal()
    
    dd1 = filterdBeat()
    icon.pop <- pulseIcons(color = ifelse(dd1$Events < 3000,'green','red'),
                       heartbeat = ifelse(dd1$Events < 3000,'1.0','0.5')) 

    leafletProxy("map", data = filteredData()) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      clearShapes() %>%
      addCircles(radius = ~(Events), weight = 1, color = "#777777",
        fillColor = ~pal(Events), fillOpacity = 0.7, popup = ~paste(Events)
      ) %>%
     clearMarkers() %>%
     addPulseMarkers(
     lng=dd1$Longitude, lat=dd1$Latitude,
     label=dd1$Area,
     icon = icon.pop)        
      
  })

  # Use a separate observer to recreate the legend as needed.
  observe({
    proxy <- leafletProxy("map", data = london)

    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    
    proxy %>% clearControls()
    if (input$legend) {
      pal <- colorpal()
      proxy %>% addLegend(position = "bottomright",
        title = "incidents", opacity = 1,
        pal = pal, values = ~Events
      )
    }
  })
}

shinyApp(ui, server)