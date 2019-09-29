rm(list=ls())
library(shiny)
library(oce)
library(ocedata)
library(measurements)
library(leaflet)
library(RCurl)
library(geosphere)
library(XML)
library(stringr)

missions <- readRDS('missions.rds')

## halifax line stations
hfxlon <- c(-63.450000, -63.317000, -62.883000, -62.451000, -62.098000, -61.733000, -61.393945, -62.7527, -61.8326)
hfxlat <- c(44.400001, 44.267001, 43.883001, 43.479000, 43.183000, 42.850000, 42.531138, 43.7635, 42.9402)

## bonavista line stations (BB01 - BB15)
bblon <- c(-52.967, -52.750, -52.650, -52.400, -52.067, -51.830, -51.542, -51.280, -51.017, -50.533, -50.017, -49.500, -49, -48.472, -47.947)
bblat <- c(48.7300, 48.800, 48.833, 48.917, 49.025, 49.100, 49.190, 49.280, 49.367, 49.517, 49.683, 49.850, 50.000, 50.177, 50.332)

mcolors <- oce.colorsJet(n=length(missions$choices))

                                        # Define UI for app that draws a histogram ----
ui <- fluidPage(

    fluidRow(
        column(3, wellPanel(
                      checkboxInput('selectAll', 'Select all/none'),
                      checkboxGroupInput("mission", 
                                         h3("Glider missions"), 
                                         choices=missions$choices),
                      actionButton(inputId = 'plot',
                                   label = 'Plot tracks')
                  )#closes wellpanel
               
               ), #closes column
        
        column(9,
               leafletOutput("map", height = '620px'))
    ) #closes fluidRow  
) #closes ui


## Define server
server <- function(input, output, session) {
    state <- reactiveValues()

    data <- reactiveFileReader(10000,
                               session,
                               'missions.rds',
                               readRDS)
    
    observe({
        updateCheckboxGroupInput(
            session, 'mission', choices = data()[['choices']],
            selected = if (input$selectAll) data()[['choices']]
        )
    })

    ## download data and load when actionButton clicked
    ## make plots too
    observeEvent(input$plot,{

        d <- data()
        
        ok <- as.numeric(input$mission)
        df <- data.frame(longitude=unlist(d$mlon[ok]),
                         latitude=unlist(d$mlat[ok]),
                         group=unlist(lapply(1:length(d$mlat[ok]),function(k) rep(k,length(d$mlat[[ok[k]]])))))
        ## leaflet map plot
        
        ## map groups
        map_wp <- "Line waypoints"
        
        map <- leaflet(as.data.frame(cbind(d$mlon[[1]], d$mlat[[1]]))) %>%
            addProviderTiles(providers$Esri.OceanBasemap) %>%
            fitBounds(lng1 = max(d$mlon[[1]], na.rm = TRUE) - 0.2,
                      lat1 = min(d$mlat[[1]], na.rm = TRUE) + 0.2,
                      lng2 = min(d$mlon[[1]], na.rm = TRUE) + 0.2,
                      lat2 = max(d$mlat[[1]], na.rm = TRUE) - 0.2) %>%
            ## use NOAA graticules
            ## not sure if it does much, but it allows to zoom further in
            ## no bathy when zoomed less than 500m though.
            addWMSTiles(
                "https://maps.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
                layers = c("1-degree grid", "5-degree grid"),
                options = WMSTileOptions(format = "image/png8", transparent = TRUE),
                attribution = "NOAA") %>%
                                        # add extra map features
                                        #addMouseCoordinates(style = 'basic')%>%
            addScaleBar(position = 'topright')%>%
            addMeasure(primaryLengthUnit = "kilometers",
                       secondaryLengthUnit = 'miles',
                       primaryAreaUnit = "hectares",
                       secondaryAreaUnit="acres",
                       position = 'bottomleft') %>%

    ##map_kml
    
    
    {
        for(i in unique(df$group)){
            . <- addPolylines(., lng = df$longitude[df$group == i],
                              lat = df$latitude[df$group == i],
                              col=mcolors[ok[i]],
                              weight = 4, opacity = 1)
        }
        return (.)
    } %>%
    ## group-less map items
    ## halifax line
    addCircleMarkers(lng = hfxlon, lat = hfxlat,
                     radius = 7, fillOpacity = 1, stroke = F,
                     color = 'gray48',
                     popup = paste(sep = "<br/>",
                                        #paste0("HL", as.character(1:7)),
                                   c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3", "HL5.5"),
                                   paste0(as.character(round(hfxlat,4)), ',', as.character(round(hfxlon,3)))),
                                        # label = paste0("HL", 1:7))
                     label = c("HL1","HL2","HL3","HL4","HL5","HL6","HL7","HL3.3", "HL5.5"),
                     group = map_wp) %>%
                                        # bonavista line
    addCircleMarkers(lng = bblon, lat = bblat,
                     radius = 7, fillOpacity = 1, stroke = F,
                     color = 'gray48',
                     popup = paste(sep = "<br/>",
                                   paste0('BB', seq(1,15)),
                                   paste0(as.character(round(bblat, 4)), ',', as.character(round(bblon, 3)))),
                     label = paste0('BB', seq(1,15)),
                     group = map_wp) %>%
    
    
    ## layer control legend
    addLayersControl(overlayGroups = c(map_wp),
                                        #map_track_kml),
                     options = layersControlOptions(collapsed = FALSE, autoZIndex = FALSE),
                     position = 'bottomright') %>%
    setView(tail(d$mlon[[1]], 1), tail(d$mlat[[1]], 1), zoom=11)
        output$map <- renderLeaflet(map) #closes leafletplot

        
    }) #closes download observeEvent
    
}

## Create Shiny app ----
shinyApp(ui = ui, server = server)
