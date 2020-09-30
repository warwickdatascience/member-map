#### SETUP ####

# Imports
library(conflicted)
library(readr)
library(leaflet)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Conflict handling
# N/A

#### GLOBAL ####

# Leaflet icons
blue_icon <- awesomeIcons(
  icon = 'circle', library = 'fa', markerColor = 'blue'
)
orange_icon <- awesomeIcons(
  icon = 'circle', library = 'fa', markerColor = 'orange'
)
green_icon <- awesomeIcons(
  icon = 'circle', library = 'fa', markerColor = 'green'
)

#### UI ####

ui <- dashboardPage(
  dashboardHeader(
    title = "WDSS Member Map",
    tags$li(
      actionLink(
        "change_palette",
        label = "Change Palette",
        title = "Change Palette",
        icon = icon("palette")
      ),
      class = "dropdown"
    ),
    tags$li(
      actionLink(
        "help",
        label = "Help",
        title = "Help",
        icon = icon("question")
      ),
      class = "dropdown"
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "favicon.ico")),
    tags$link(rel = 'stylesheet', type = 'text/css', href = 'style.css'),
    tags$style(paste0("@import url(https://use.fontawesome.com/releases/",
                      "v5.7.2/css/all.css);")),
    leafletOutput("map")
  )
)

#### SERVER ####

server <- function(input, output, session) {
  points <- read_csv('data.csv', col_types = cols(.default = col_double()))
  provider <- reactiveVal('OpenStreetMap.Mapnik')
  open_modal <- reactiveVal(0)
  selection <- reactiveVal()
  added <- reactiveVal(FALSE)

  observeEvent(input$change_palette, {
    if (provider() == 'OpenStreetMap.Mapnik') {
      provider('Stamen.Watercolor')
    } else {
      provider('OpenStreetMap.Mapnik')
    }

    # Add marker
    if (!added()) {
      if (!is.null(input$map_click)) {
        leafletProxy("map") %>%
          addAwesomeMarkers(lng = input$map_click$lng, lat = input$map_click$lat,
                            icon = orange_icon, layerId = 'new')
      }
    } else {
      leafletProxy("map") %>%
        addAwesomeMarkers(lng = selection()$lng, lat = selection()$lat,
                          icon = green_icon, layerId = 'new')
    }
  })

  observeEvent(input$help, {
    open_modal(open_modal() + 1)
  })

  observeEvent(open_modal(), {
    showModal(
      modalDialog(title = "Help",
                  HTML(paste0(
                    "<center>",
                    "<b>Where do WDSS members originally come from?</b><br>",
                    "<br>Select a location on the map and click the marker to ",
                    "confirm the selection.<br><br>A small amount of privacy-",
                    "preserving random noise will be added to the input ",
                    "before being saved to the map.<br><br>",
                    "We'll post a final visualisation our social media in ",
                    "the coming days.</center>"
                  )))
    )
  })

  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) %>%
      addProviderTiles(providers[[provider()]],
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addAwesomeMarkers(data = points, icon = blue_icon)
  })

  observeEvent(input$map_click, {
    req(!added())
    leafletProxy("map") %>%
      removeMarker('new') %>%
      addAwesomeMarkers(lng = input$map_click$lng, lat = input$map_click$lat,
                        icon = orange_icon, layerId = 'new')
  })

  observeEvent(input$map_marker_click, {
    if (!is.null(input$map_marker_click$id) && !added()) {
      selection(input$map_marker_click)

      confirmSweetAlert(
        session,
        'confirm',
        "Add Location to Map",
        paste("Would you like to add the current marker location to our",
              "member map? (Note, we'll add some random noise to the input",
              "to ensure privacy)")
      )
    }
  })

  observeEvent(input$confirm, {
    req(input$confirm)

    noisy_lat <- selection()$lat + rnorm(1, 0, 0.01)
    noisy_lng <- selection()$lng + rnorm(1, 0, 0.007)

    tibble(lat = noisy_lat, lng = noisy_lng) %>%
      write_csv('data.csv', append = TRUE)

    leafletProxy("map") %>%
      removeMarker('new') %>%
      addAwesomeMarkers(lng = noisy_lng, lat = noisy_lat,
                        icon = green_icon, layerId = 'new')

    added(TRUE)
  })
}

shinyApp(ui, server)
