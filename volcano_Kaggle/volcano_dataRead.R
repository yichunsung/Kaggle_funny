setwd('c:/Kaggle_funny/volcano_Kaggle')
volcano_df_raw <- read.csv('data/database.csv')

# Volcanoes around the world
library(magrittr)
library(leaflet)
library(plotly)
# Tectonic Setting

class_tectonic <- unique(volcano_df_raw$Tectonic.Setting) # 看一下有幾個不同的板塊邊界

# Last Eruption
# 重新整理上一次噴發時間
# 將時間與BCE & CE 切開
subUnkown <- sub("Unknown",replacement ="Unknown Unknown",volcano_df_raw$Last.Known.Eruption) # 方便切分 將Unknown複製兩次
splitTime <- strsplit(as.character(subUnkown), split=" ", fixed=T) # 切開
# 重新整理 collation
year <- c()
yearCE <- c()
for(i in 1:length(subUnkown)){
  year <- c(year, splitTime[[i]][1])
  yearCE <- c(yearCE,  splitTime[[i]][2])
}
Last_Eruption <- data.frame(year, yearCE)
# if my time data is before Christian era, I let it * -1. 
CEyearTransfrom <- ifelse(Last_Eruption$yearCE=="BCE", as.numeric(as.vector(Last_Eruption$year))*-1, as.numeric(as.vector(Last_Eruption$year))*1)
Last_Eruption <- cbind(Last_Eruption, last_time_eruption = CEyearTransfrom)
volcano_df_raw <- cbind(volcano_df_raw, Last_Eruption) # 增加進總表
# 捨棄沒有噴發時間的資料
Last_Eruption_NOUnkown <- subset(Last_Eruption, Last_Eruption$last_time_eruption!= "NA") 

# 重新整理板塊邊界欄位
splitTectonic <- strsplit(as.character(volcano_df_raw$Tectonic.Setting), split="/", fixed=T) # 切開
tectonic_type <- c()
crust_type <- c()
for(i in 1:length(volcano_df_raw$Tectonic.Setting)){
  tectonic_type <- c(tectonic_type, splitTectonic[[i]][1])
  crust_type <- c(crust_type,  splitTectonic[[i]][2])
}
tectonic_df <- data.frame(tectonic_type, crust_type)
tectonic_df$crust_type <- sub(" ",replacement ="", tectonic_df$crust_type)

volcano_df_raw <- cbind(volcano_df_raw, tectonic_df)# 增加進總表 

# drop last eruption Unknown data
Drop_unknown_eruption <- subset(volcano_df_raw, volcano_df_raw$yearCE!="Unknown")

write.csv(volcano_df_raw, "c://Kaggle_funny/volcano_Kaggle/data/newDataBase.csv")
write.csv(Drop_unknown_eruption, "c://Kaggle_funny/volcano_Kaggle/data/recordEruption.csv")






##### figure output #######

# Plotly 畫出噴發時間

eruption_plotlt <- plot_ly(data = Last_Eruption_NOUnkown,
                           x = Last_Eruption_NOUnkown$last_time_eruption,
                           type = "histogram"
                           )
eruption_plotlt


# 畫出所有火山分布，根據Tectonic Setting著色

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


# 畫出已知有噴發紀錄的火山
pal <- colorFactor(c("navy", "red", "yellow", "pink", "green", "blue", "black", "lightgreen", "lightblue", "orange", "purple", "grey"), domain = c(as.character(class_tectonic)))

known_eruption_volcano_map <- leaflet(data = Drop_unknown_eruption) %>%
  setView(lng = 90.00, lat = 0.00, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = Drop_unknown_eruption$Longitude,
                   lat = Drop_unknown_eruption$Latitude,
                   popup =~as.character(Drop_unknown_eruption$last_time_eruption),
                   radius = 5,
                   weight = 1,
                   opacity = 0.8,
                   fill = TRUE,
                   fillOpacity = 0.8,
                   fillColor = ~pal(Drop_unknown_eruption$Tectonic.Setting),
                   color = "pink") %>%
  addLegend("bottomright",
            pal = pal,
            values = ~Drop_unknown_eruption$Tectonic.Setting,
            title = "Tectonic Setting",
            opacity = 1)

known_eruption_volcano_map

# 畫出分群結果
pal <- colorFactor(c("navy", "red", "yellow", "green"), domain = c(1:4))

cluster_volcano_map <- leaflet(data = VC_km) %>%
  setView(lng = 90.00, lat = 0.00, zoom = 3) %>%
  addProviderTiles("Esri.WorldImagery") %>%
  addCircleMarkers(lng = VC_km$lng,
                   lat = VC_km$lat,
                   popup =~as.character(VC_km$erupt_time),
                   radius = 5,
                   weight = 1,
                   opacity = 0.8,
                   fill = TRUE,
                   fillOpacity = 0.8,
                   fillColor = ~pal(VC_km$cluster),
                   color = "black") %>%
  addLegend("bottomright",
            pal = pal,
            values = ~VC_km$cluster,
            title = "erupt time",
            opacity = 1)

cluster_volcano_map

