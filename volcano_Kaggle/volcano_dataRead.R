setwd('c:/Kaggle_funny/volcano_Kaggle')
volcano_df_raw <- read.csv('data/database.csv')


# Volcanoes around the world
library(magrittr)
library(leaflet)
# Tectonic Setting

class_tectonic <- unique(volcano_df_raw$Tectonic.Setting)

pal <- colorFactor(c("navy", "red", "yellow", "pink", "green", "blue", "black", "lightgreen", "lightblue", "orange", "purple", "grey"), domain = c(as.character(class_tectonic)))

world_volcano_map <- leaflet(data = volcano_df_raw) %>%
  setView(lng = 90.00, lat = 0.00, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = volcano_df_raw$Longitude,
                   lat = volcano_df_raw$Latitude,
                   popup =~as.character(volcano_df_raw$Name),
                   radius = 5,
                   weight = 1,
                   opacity = 0.8,
                   fill = TRUE,
                   fillOpacity = 0.8,
                   fillColor = ~pal(volcano_df_raw$Tectonic.Setting),
                   color = "pink") %>%
  addLegend("bottomright",
            pal = pal,
            values = ~volcano_df_raw$Tectonic.Setting,
            title = "Tectonic Setting",
            opacity = 1)
world_volcano_map

# Last Eruption

subUnkown <- sub("Unknown",replacement ="Unknown Unknown",volcano_df_raw$Last.Known.Eruption)

splitTime <- strsplit(as.character(subUnkown), split=" ", fixed=T)

year <- c()
yearCE <- c()
for(i in 1:length(subUnkown)){
  year <- c(year, splitTime[[i]][1])
  yearCE <- c(yearCE,  splitTime[[i]][2])
}

Last_Eruption <- data.frame(year, yearCE)

CEyearTransfrom <- ifelse(Last_Eruption$yearCE=="BCE", as.numeric(as.vector(Last_Eruption$year))*-1, as.numeric(as.vector(Last_Eruption$year))*1)
Last_Eruption <- cbind(Last_Eruption, last_time_eruption = CEyearTransfrom)

volcano_df_raw <- cbind(volcano_df_raw, Last_Eruption)
#write.csv(volcano_df_raw, "c://Kaggle_funny/volcano_Kaggle/data/newDataBase.csv")

Last_Eruption_NOUnkown <- subset(Last_Eruption, Last_Eruption$last_time_eruption!= "NA")

# Plotly
library(plotly)

eruption_plotlt <- plot_ly(data = Last_Eruption_NOUnkown,
                           x = Last_Eruption_NOUnkown$last_time_eruption,
                           type = "histogram"
                           )
eruption_plotlt



