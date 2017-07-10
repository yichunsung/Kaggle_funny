### Clustering
## Kmeans
# read data
setwd('c:/Kaggle_funny/volcano_Kaggle')
RecordVolcano <- read.csv('data/recordEruption.csv')

# Let's go K-means
VC_km <- data.frame(name = RecordVolcano$Name, 
                    lat = RecordVolcano$Latitude,
                    lng = RecordVolcano$Longitude,
                    erupt_time = RecordVolcano$last_time_eruption,
                    tectonic_type = RecordVolcano$tectonic_type,
                    crust_type = RecordVolcano$crust_type)


# Let Intraplate = 1, Rift Zone = 2, Subduction Zone = 3.
type_tectonic <- names(summary(VC_km$tectonic_type))
for(i in 1:length(type_tectonic)){
  VC_km$tectonic_type <- sub(type_tectonic[[i]], replacement =i*2200, VC_km$tectonic_type)
}
# Let Continental Crust = 10, Crust Thickness Unknown = 20, Intermediate Crust =30, 
# Oceanic Crust  = 40, NA's = 5.
splitCrust <- strsplit(as.character(VC_km$crust_type), split="(", fixed=T) # 切開
crust <- c()
for(i in 1:length(splitCrust)){
  crust <- c(crust,  splitCrust[[i]][1])
}
VC_km$crust_type <- crust
VC_km$crust_type <- sub(" ", replacement = "", VC_km$crust_type)
type_crust <- unique(VC_km$crust_type)
VC_km$crust_type <- sub('ContinentalCrust', replacement = 1*1000, VC_km$crust_type)
VC_km$crust_type <- sub('CrustThickness Unknown', replacement = 2*1000, VC_km$crust_type)
VC_km$crust_type <- sub('IntermediateCrust', replacement = 3*1000, VC_km$crust_type)
VC_km$crust_type <- sub('OceanicCrust', replacement = 4*1000, VC_km$crust_type)

# VC_KM 
VC_KM_fit <- VC_km[,2:6]
VC_KM_fit <- subset(VC_KM_fit, VC_KM_fit$tectonic_type!= "NA")
VC_KM_fit <- subset(VC_KM_fit, VC_KM_fit$crust_type!= "NA")
# 組間差距
ratio_ss <- rep(NA, times = 12)
for (k in 1:length(ratio_ss)) {
  fit_km <- kmeans(VC_KM_fit, centers=k, nstart=20)
  ratio_ss[k] <- fit_km$tot.withinss/fit_km$totss
}
ratio_ss_df <- data.frame(Kn =  c(1:length(ratio_ss)), ratio_ss)
plot_ly(data = ratio_ss_df,
        x =~ratio_ss_df$Kn, 
        y=~ratio_ss_df$ratio_ss, type = "scatter", mode = "markers+lines")

# k-maens = 7
set.seed(2017)
VC_km_kmeans <- kmeans(VC_KM_fit, nstart = 100, centers = 7)
table(VC_km_kmeans$cluster)

plot(VC_km_1$lng, VC_km_1$lat, col=VC_km_kmeans$cluster)
VC_km <- subset(VC_km, VC_km$tectonic_type!= "NA")
VC_km <- cbind(VC_km, cluster = VC_km_kmeans$cluster)

VC_km_C1 <- subset(VC_km, VC_km$cluster==1)
VC_km_C2 <- subset(VC_km, VC_km$cluster==2)
VC_km_C3 <- subset(VC_km, VC_km$cluster==3)
VC_km_C4 <- subset(VC_km, VC_km$cluster==4)
# plot
plot_ly(alpha = 0.6) %>%
  add_histogram(x = VC_km_C1$erupt_time, name = "cluster1") %>%
  add_histogram(x = VC_km_C2$erupt_time, name = "cluster2") %>%
  add_histogram(x = VC_km_C3$erupt_time, name = "cluster3") %>%
  add_histogram(x = VC_km_C4$erupt_time, name = "cluster4") %>%
  layout(barmode = "overlay")


# ======================================only lng+lat+eruption time==================
VC_km_1 <- VC_km[, 2:4]
set.seed(2017)
VC_kmeans <- kmeans(VC_km_1, nstart = 100, centers = 3)
table(VC_kmeans$cluster)
plot(VC_km_1$lng, VC_km_1$lat, col=VC_kmeans$cluster)


# 來看一下組間差距 (經緯度+上一次噴發時間)


ratio_ss <- rep(NA, times = 12)
for (k in 1:length(ratio_ss)) {
  fit_km <- kmeans(VC_km_1, centers=k, nstart=20)
  ratio_ss[k] <- fit_km$tot.withinss/fit_km$totss
}
ratio_ss_df <- data.frame(Kn =  c(1:length(ratio_ss)), ratio_ss)
plot_ly(data = ratio_ss_df,x =~ratio_ss_df$Kn, y=~ratio_ss_df$ratio_ss, type = "scatter", mode = "markers+lines")
plot(ratio_ss, type="b", xlab="k", main = "screeplot") # "b" as in both

# 決定K = 4


#重新
set.seed(2017)
VC_kmeans <- kmeans(VC_km_1, nstart = 20, centers = 4)
table(VC_kmeans$cluster)
plot(VC_km_1$lng, VC_km_1$lat, col=VC_kmeans$cluster)

# ==================== 看一下不同分群之下各群的板塊邊界組成
VC_km <- cbind(VC_km, cluster = VC_kmeans$cluster)
VC_km <- subset(VC_km, VC_km$tectonic_type!="NA")
VC_km_C1 <- subset(VC_km, VC_km$cluster==1)
VC_km_C2 <- subset(VC_km, VC_km$cluster==2)
VC_km_C3 <- subset(VC_km, VC_km$cluster==3)
VC_km_C4 <- subset(VC_km, VC_km$cluster==4)

tectonic_class_count <- function(vc_df) {
  type_count <- c()
  tectonic_class <-  unique(VC_km$tectonic_type)
  for(i in 1:3){
    type_count <- c(type_count, subset(vc_df, vc_df$tectonic_type==tectonic_class[[i]]) %>% nrow())
  }
  return(type_count)
}

vc_count_df <- data.frame(type = unique(VC_km$tectonic_type)[1:3], 
                          cluster_1 = tectonic_class_count(VC_km_C1),
                          cluster_2 = tectonic_class_count(VC_km_C2),
                          cluster_3 = tectonic_class_count(VC_km_C3),
                          cluster_4 = tectonic_class_count(VC_km_C4)
                          )

# ===== Plotly
every_plotly <- function(vc_km_df){
  eruption_plotlt <- plot_ly(data = vc_km_df,
                             x = vc_km_df$erupt_time,
                             type = "histogram")
  return(eruption_plotlt)
}

every_plotly(VC_km_C1)
every_plotly(VC_km_C2)
every_plotly(VC_km_C3)
every_plotly(VC_km_C4)

# plot
plot_ly(alpha = 0.6) %>%
  add_histogram(x = VC_km_C1$erupt_time, name = "cluster1") %>%
  add_histogram(x = VC_km_C2$erupt_time, name = "cluster2") %>%
  add_histogram(x = VC_km_C3$erupt_time, name = "cluster3") %>%
  add_histogram(x = VC_km_C4$erupt_time, name = "cluster4") %>%
  layout(barmode = "overlay")

# map



