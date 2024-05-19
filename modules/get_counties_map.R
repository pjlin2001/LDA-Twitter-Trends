get_counties_map <- function(selected_counties){
  state_map <- renderLeaflet({
    leaflet(data = selected_counties) %>%
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
        layerId = ~NAME  # Assign layerId here
      )
  })
  
  return(state_map)
}