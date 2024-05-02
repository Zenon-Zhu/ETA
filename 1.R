# 加载库
library(sf)
library(ggplot2)
library(dplyr)

# 读取数据
shapefile <- st_read("C:/Users/61723/Desktop/GBR_adm/GBR_adm2.shp")
points_data <- read.csv("C:/Users/61723/Desktop/x.csv")

# 执行聚类
set.seed(123)
clusters <- kmeans(points_data[, c("longitude", "latitude")], centers = 1000, nstart = 35)
points_data$cluster <- clusters$cluster
points_data <- points_data %>%
  group_by(cluster) %>%
  mutate(count = n()) %>%
  ungroup()

# 转换为SF对象
points_sf <- st_as_sf(points_data, coords = c("longitude", "latitude"), crs = st_crs(shapefile), agr = "constant")

# 绘图
ggplot() +
  geom_sf(data = shapefile, fill = "lightblue", color = "black") +
  geom_sf(data = points_sf, aes(size = count), color = "red") +
  scale_size(range = c(1, 1)) +
  labs(title = "Map Visualization with Clusters") +
  theme_minimal() +
  guides(size = guide_legend(title = "Number of Points"))


# 加载必要的库
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("cluster")
library(sf)
library(ggplot2)
library(dplyr)
library(cluster)

# 读取数据
shapefile <- st_read("C:/Users/61723/Desktop/GBR_adm/GBR_adm2.shp")
points_data <- read.csv("C:/Users/61723/Desktop/x.csv")

# 数据处理：确保longitude和latitude是数值型
points_data$longitude <- as.numeric(points_data$longitude)
points_data$latitude <- as.numeric(points_data$latitude)
data <- points_data[, c("longitude", "latitude")]

# 使用轮廓系数来确定最佳聚类数
silhouette_widths <- sapply(2:15, function(k) {
  model <- kmeans(data, centers = k, nstart = 25)
  ss <- silhouette(model$cluster, dist(data))
  mean(ss[, "sil_width"])
})

# 寻找轮廓系数最大的聚类数
optimal_clusters <- which.max(silhouette_widths)
print(paste("Optimal number of clusters:", optimal_clusters))

# 绘制轮廓系数图
plot(2:15, silhouette_widths, type = "b", pch = 19, col = "red", xlab = "Number of Clusters", ylab = "Average Silhouette Width", main = "Silhouette Method for Determining Optimal k")

# 使用最佳聚类数进行聚类
set.seed(123)
final_clusters <- kmeans(data, centers = optimal_clusters, nstart = 25)
points_data$cluster <- final_clusters$cluster
points_data$size <- table(points_data$cluster)[points_data$cluster]  # 计算每个聚类的大小

# 创建点的SF对象
points_sf <- st_as_sf(points_data, coords = c("longitude", "latitude"), crs = st_crs(shapefile), agr = "constant")

# 绘制地图和点
ggplot() +
  geom_sf(data = shapefile, fill = "lightblue", color = "black") +
  geom_sf(data = points_sf, aes(size = size), color = "red", show.legend = "point") +
  scale_size(range = c(1, 10)) +
  labs(title = "Map Visualization with Optimal Clusters Based on Silhouette Scores") +
  theme_minimal() +
  guides(size = guide_legend(title = "Cluster Size"))
