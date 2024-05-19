get_states_map <- function(states_sf){

  static <- renderLeaflet({
    leaflet(data = states_sf) %>%
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      addPolygons(
        fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = ~paste(NAME),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        layerId = ~STUSPS  # Assign layerId here
      ) %>%
      addTiles() %>%  # Add default OpenStreetMap tiles
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)  # Center on the US
  })
  
  return(static)
}