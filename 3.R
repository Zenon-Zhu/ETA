# 安装和加载必要的包
if (!require("readr")) install.packages("readr", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
library(readr)
library(ggplot2)

# 读取CSV文件
data <- read_csv("C:/Users/61723/Desktop/y.csv")  # 替换为你的文件路径

# 查看数据结构
print("Data structure:")
str(data)

# 查看前几行数据以确保正确加载
print("Head of data:")
head(data)

# 绘制散点图
p <- ggplot(data, aes(x = location, y = number)) +
  geom_point() +  # 添加点
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # 添加线性回归线
  xlab(NULL) +  # 移除X轴标题
  ylab(NULL) +
  theme_minimal() +  # 使用简洁的主题
  theme(axis.text.x = element_text(angle = 65, hjust = 1))  # 将X轴标签旋转45度并进行水平调整
# 打印图形
print(p)
