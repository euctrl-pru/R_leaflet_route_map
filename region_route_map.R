#packages
##general
library(dplyr)
library(tidyverse) 
library(openxlsx)
library(xlsx)
library(geojsonio)
library(jsonlite)


##maps
library(leaflet.extras)
library(leaflet)
library(leafem)
library(sf)
library(rlang)
library(htmlwidgets)
library(geosphere)

#importing top 5 city pairs, cities and airports data
city_pairs <- openxlsx::read.xlsx("Excel files/top_5_city_pairs_by_region.xlsx",
                              sheet = "city_pairs")
cities <- openxlsx::read.xlsx("Excel files/top_5_city_pairs_by_region.xlsx",
                                sheet = "cities")
airports <- openxlsx::read.xlsx("Excel files/top_5_city_pairs_by_region.xlsx",
                    sheet = "airports")

#mapping
#route function
journeys_to_sf <- function(journeys_data,
                           start_long = LON1,
                           start_lat = LAT1,
                           end_long = LON2,
                           end_lat = LAT2) {
  
  quo_start_long <- enquo(start_long)
  quo_start_lat <- enquo(start_lat)
  quo_end_long <- enquo(end_long)
  quo_end_lat <- enquo(end_lat)
  
  
  journeys_data %>%
    select(
      !! quo_start_long,
      !! quo_start_lat,
      !! quo_end_long,
      !! quo_end_lat
    ) %>%
    transpose() %>%
    map(~ matrix(flatten_dbl(.), nrow = 2, byrow = TRUE)) %>%
    map(st_linestring) %>%
    st_sfc(crs = 4326) %>% #sf functions
    st_sf(geometry = .) %>% #sf functions
    bind_cols(journeys_data) %>%
    select(everything(), geometry)
}

#creating segment
#routes
#segment for city pairs route
segment <- data.frame(city_pairs) %>%
  journeys_to_sf() %>%
  st_segmentize(units::set_units(100, km))

#adding show all
segment2 <- segment
segment2$region <- "Show All"
segment_final <- rbind(segment,segment2)


#airport
#segment for airport to city route
segment_airport <- data.frame(airports) %>%
  journeys_to_sf(start_long = LON,
                 start_lat = LAT,
                 end_long = city_LON,
                 end_lat = city_LAT) %>%
  st_segmentize(units::set_units(100, km))

#show all
segment_airport2 <- segment_airport
segment_airport2$region <- "Show All"
segment_airport_final <- rbind(segment_airport,segment_airport2)

segment_airport_final <- segment_airport_final %>% 
  group_by(airport,
           airport_name,
           airport_region,
           region,
           LAT,
           LON,
           city,
           city_LAT,
           city_LON,
           color,
           Distance
  ) %>%
  summarise(airport_links = paste0(airport_links,collapse="<br>")) 

#cities
#show all
cities2 <- cities
cities2$region <- "Show All"
cities_final <- rbind(cities,cities2)

cities_final <- cities_final %>% 
  distinct(LAT,
           LON,
           LAT_label,
           LON_label,
           city,
           country,
           region,
           color,
           city_link,
           country_link,
           avg_daily) %>%
  group_by(LAT,
           LON,
           LAT_label,
           LON_label,
           city,
           country,
           region,
           color
  ) %>%
  reframe(flight_link=paste0(city_link,", ",country_link,": ", avg_daily,collapse="<br>"))

#regions map
##statfor regions
json_data <- fromJSON("Maps/statfor_regions_cleaned.json")
##country geometries
countries_geojson_ <- st_read("Maps/map_final.json")
##Merging json_data with the country geometries
merged_data <- merge(countries_geojson_, json_data, 
                     by.x = "id", 
                     by.y = "adm0_a3")
##Convert back to GeoJSON
statfor_region_fill_ <- geojson_json(merged_data)

##reading geojson data
statfor_region_fill <- st_read(statfor_region_fill_)
#colouring regions
color_function_map <- function(table_field){
  new_field <- ifelse(table_field=="Asia/Pacific","darkgreen",
                      ifelse(table_field=="ECAC","#34cccc",
                             ifelse(table_field=="Mid-Atlantic","#cc349b",
                                    ifelse(table_field=="Middle-East","#8ec421",
                                           ifelse(table_field=="North-Africa","#fc9b33",
                                                  ifelse(table_field=="North Atlantic","#fc9b9b",
                                                         ifelse(table_field=="Other Europe","#04639b",
                                                                ifelse(table_field=="South-Atlantic","#d00434",
                                                                       "#fccc34"
                                                                ))))))))
  return(new_field)
}



statfor_region_fill$color <- color_function_map(statfor_region_fill$statfor)



#final leaflet map
map1 <-leaflet(segment_final,
               options = 
                 leafletOptions(zoomControl = TRUE, ##zoom button
                                ##setting zoom levels
                                minZoom = 2, maxZoom = 15,
                                #defining map bounds
                                maxBounds = list(list(-100, -180),
                                                 list(100, 180)))
)  %>%
  addProviderTiles(
    ##nowrap stops maps repeating
    providers$CartoDB.Positron,options=providerTileOptions(noWrap = TRUE) )  %>%
  ##coluoring STATFOR regions
  addPolygons(data = statfor_region_fill,
              fillColor = ~color,
              fillOpacity = 0.15,
              #color = ~color, ##coloring country outline
              color = NA,
              weight = 1) %>%
  fitBounds(-180, -80, 200, 90) %>%
  ##Reset map to default setting button
  leaflet.extras::addResetMapButton() %>%
  ##add polylines for city1 to city2 routes
  addPolylines( weight = 3,color=~color,
                popup=~paste0("<center> <b>",city1," - ",
                              city2,"</b> </center>","<b>Average number of daily flights: </b>",
                              round(avg_daily,0),"<br> <b>Average number of daily flights by airport</b> <br>",
                              airport_link),
                ##grouping by region
                group=~region)  %>%
  ##add circles for cities
  addCircleMarkers(data = cities_final,
                   lng=~LON, 
                   lat=~LAT,
                   group = ~region,
                   color = ~color,
                   radius = 5,
                   popup = ~paste0("<b> <center>",city,", ",
                                   country," </b> </center> ","<b>Average number of daily flights </b><br>",
                                   flight_link)) %>%
  ##adding city labels
  addLabelOnlyMarkers(data = cities_final,lng=~LON_label, lat=~LAT_label,
                      label = ~city,
                      labelOptions = labelOptions(noHide = TRUE, offset=c(1,1), textOnly = TRUE,
                                                  style = list(
                                                    "color" = "black", 
                                                    "font-style" = "normal",
                                                    "font-weight" = "bold",
                                                    #"box-shadow" = "1px 1px rgba(0,0,0,0.25)",
                                                    "font-size" = "12px",
                                                    #"border-color" = "rgba(0,0,0,0.5)",
                                                    "padding" = "2px" 
                                                  )),
                      group = ~region) %>%
  ##adding polylines for airport to city routes
  addPolylines( data= segment_airport_final,
                weight = 3,
                color="black",
                popup = ~paste0("<b> <center> Distance from ",
                                city," city to ",
                                airport_name," airport: </b>",
                                Distance," km"),
                group="polylineGroup")  %>%
  ##adding airport circles
  addCircleMarkers(data = segment_airport_final,
                   lng=~LON, 
                   lat=~LAT,
                   group = "circleMarkerGroup",
                   color = "black",
                   radius = 3,
                   #clusterOptions = markerClusterOptions(removeOutsideVisibleBounds = FALSE),
                   popup = ~paste0("<b> <center> ",
                                   airport_name," (",
                                   airport,") airport <br>", "</center> Average number of daily flights</b> <br>", 
                                   airport_links)) %>%
  ##adding filter button
  addLayersControl(
    ##listing elements in filter
    overlayGroups = c("Show All",sort(unique(segment$region))),
    ##filter position
    position = "topleft",
    ##hiding filter
    options = layersControlOptions(collapsed = TRUE))  %>%
  ##adding a title to the region filter
  htmlwidgets::onRender("function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Regions</label>');
        }
    ") %>% 
  hideGroup(sort(unique(segment$region))) %>%
  #adding full screen button
  addFullscreenControl() %>%
  ##adding Eurocontrol logo
  leafem::addLogo(
    img = "https://www.eurocontrol.int/sites/default/files/2024-06/eurocontrol-boxed-rgb.svg",
    position = "topright",
    width = 210,
    height = 80)  %>%
  #@adding Eurocontrol disclaimer text to the bottom of map
  addControl(
    "<div style='width:98vw; font-size:1.5vw; margin: 0; padding: 0; box-sizing: border-box;'>
      <p style='margin: 0; padding: 0; line-height: 1.1; font-size: 1.1vw;'><b>Map designations</b> <br/> The designations employed and the presentation of the material on maps, videos and animations
      do not imply the expression of any opinion whatsoever on the part of EUROCONTROL concerning the legal status of
      any country, territory, city or area or of its authorities, or concerning the delimitation of its frontiers or boundaries. <br>
      The routes displayed on the map are used solely for visualisation purposes and do not represent the actual
      flown routes.</p>
    </div>",
    position = 'bottomleft'
  ) 

##removing labels and airports based on zoom level
##using html widget
map <- map1 %>%
  onRender("
    function(el, x) {
      var map = this;
      var polylineGroup = L.layerGroup();
      var circleMarkerGroup = L.layerGroup();
      var labelGroup = L.layerGroup();

      map.eachLayer(function(layer) {
        if (layer.options && layer.options.group === 'polylineGroup') {
          polylineGroup.addLayer(layer);
          map.removeLayer(layer); // Hide initially
        }
        if (layer.options && layer.options.group === 'circleMarkerGroup') {
          circleMarkerGroup.addLayer(layer);
          map.removeLayer(layer); // Hide initially
        }
        if (layer.options && layer.options.group === 'labelGroup') {
          labelGroup.addLayer(layer);
          map.removeLayer(layer); // Hide initially
        }
      });

      map.on('zoomend', function() {
        var zoom = map.getZoom();
        var labels = document.getElementsByClassName('leaflet-tooltip');
        //specifying zoom level
        if (zoom < 6) {
          //if zoom is less than 6, hide airports circle and lines
          map.removeLayer(polylineGroup);
          map.removeLayer(circleMarkerGroup);
          //if zoom is less than 6, show labels
          for (var i = 0; i < labels.length; i++) 
          {labels[i].style.display = 'block';}
        } else {
          //if zoom is greater or equal to 6, add airports circle and lines
          map.addLayer(polylineGroup);
          map.addLayer(circleMarkerGroup);
          //if zoom is greater or equal to 6, hide labels
          for (var i = 0; i < labels.length; i++) 
          {labels[i].style.display = 'none';}
        }
      });
    }
  ")

map