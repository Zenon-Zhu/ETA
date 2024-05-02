# 安装并加载必要的库
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dbscan", quietly = TRUE)) install.packages("dbscan")
library(sf)
library(ggplot2)
library(dbscan)

# 读取Shapefile和CSV数据
shapefile <- st_read("C:/Users/61723/Desktop/GBR_adm/GBR_adm2.shp")
points_data <- read.csv("C:/Users/61723/Desktop/x.csv")

# 确保longitude和latitude是数值型
points_data$longitude <- as.numeric(as.character(points_data$longitude))
points_data$latitude <- as.numeric(as.character(points_data$latitude))

# 准备数据
data <- points_data[, c("longitude", "latitude")]

# 执行DBSCAN聚类
# eps 和 minPts 需要根据数据的具体情况进行调整
dbscan_result <- dbscan(data, eps = 0.03, minPts = 5)  # 这里的参数eps和minPts是示例，可能需要调整

# 将聚类结果添加到数据中
points_data$cluster <- dbscan_result$cluster

# 计算每个聚类的大小
cluster_sizes <- table(points_data$cluster)

# 使用聚类标签创建一个新的向量，该向量直接映射每个点的聚类大小
# 使用 as.numeric 将聚类大小映射回对应的数据行
points_data$size <- cluster_sizes[as.character(points_data$cluster)]

# 确保 size 列是数值型
points_data$size <- as.numeric(points_data$size)

# 创建点的SF对象
points_sf <- st_as_sf(points_data, coords = c("longitude", "latitude"), crs = st_crs(shapefile), agr = "constant")




# 绘制地图和聚类点
ggplot() +
  geom_sf(data = shapefile, fill = "white", color = "red") +  # 绘制底图
  geom_sf(data = points_sf, aes(color = as.factor(cluster), size = size), alpha = 0.7, show.legend = "point") +  # 添加聚类点，设置透明度为0.5
  scale_size_continuous(range = c(1, 3), breaks = pretty(points_data$size, n = 5)) +  # 调整点的大小范围，最大大小设为5
  scale_color_viridis_d(option = "C") +  # 设置颜色
  labs( color = "Cluster", size = "Cluster Size") +
  theme_minimal() +
  guides(color = guide_legend(override.aes = list(size = 6)))  # 更新图例以反映点大小的更改
