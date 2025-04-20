###读取数据—————————————————————————————————————————————————————————————————————
install.packages("readxl")
library(readxl)
data <- read_excel("C:/Users/10781/Desktop/上海（合并）(插值）.xlsx",col_names = TRUE)
data <- cbind(data[,13],data[,15:18])
as.matrix(data)
p = dim(data)[2]
n = dim(data)[1]

#VARMA模型
install.packages("MTS")
library(MTS)

###初步分析——————————————————————————————————————————————————————————————————-——
pairs(data)
(cor(data))
par(mfrow = c(3,2))
for(i in 1:p){
  plot(data[,i],main = names(data)[i],xlab = "天数",ylab = names(data)[i])
}

###去除周期和趋势后提取残差————————————————————————————————————————————————————-
###第一个变量
  ts_data1 <- ts(data[,1], frequency = 240)
  stl_decomp1 <- stl(ts_data1, s.window = "periodic")
  plot(stl_decomp1, main = "")    # AQI时间序列STL分解（趋势、季节、残差）
  # 残差部分
  remainder1 <- stl_decomp1$time.series[, 3]
  # 平稳性检验
  install.packages("tseries")
  library(tseries)
  # 对平稳序列检验
  adf_stationary <- adf.test(remainder1)
  print(adf_stationary) # p值<0.05（拒绝原假设，认为平稳）
  # 自相关检验
  Box_test <- Box.test(remainder,type = "Ljung-Box",lag = 10)
  print(Box_test)
  
###第二个变量
  ts_data2 <- ts(data[,2], frequency = 240)
  stl_decomp2 <- stl(ts_data2, s.window = "periodic")
  plot(stl_decomp2, main = "")    # AQI时间序列STL分解（趋势、季节、残差）
  # 残差部分
  remainder2 <- stl_decomp2$time.series[, 3]
  # 对平稳序列检验
  adf_stationary <- adf.test(remainder2)
  print(adf_stationary) # p值<0.05（拒绝原假设，认为平稳）
  # 自相关检验
  Box_test <- Box.test(remainder2,type = "Ljung-Box",lag = 10)
  print(Box_test)

###第三个变量
  ts_data3 <- ts(data[,3], frequency = 240)
  stl_decomp3 <- stl(ts_data3, s.window = "periodic")
  plot(stl_decomp3, main = "")    # AQI时间序列STL分解（趋势、季节、残差）
  # 残差部分
  remainder3 <- stl_decomp3$time.series[, 3]
  # 对平稳序列检验
  adf_stationary <- adf.test(remainder3)
  print(adf_stationary) # p值<0.05（拒绝原假设，认为平稳）
  # 自相关检验
  Box_test <- Box.test(remainder2,type = "Ljung-Box",lag = 10)
  print(Box_test)
  
###第四个变量
  ts_data4 <- ts(data[,4], frequency = 240)
  stl_decomp4 <- stl(ts_data4, s.window = "periodic")
  plot(stl_decomp4, main = "")    # AQI时间序列STL分解（趋势、季节、残差）
  # 残差部分
  remainder4 <- stl_decomp4$time.series[, 3]
  # 对平稳序列检验
  adf_stationary <- adf.test(remainder4)
  print(adf_stationary) # p值<0.05（拒绝原假设，认为平稳）
  # 自相关检验
  Box_test <- Box.test(remainder4,type = "Ljung-Box",lag = 10)
  print(Box_test)

###第五个变量
  ts_data5 <- ts(data[,5], frequency = 240)
  stl_decomp5 <- stl(ts_data5, s.window = "periodic")
  plot(stl_decomp5, main = "")    # AQI时间序列STL分解（趋势、季节、残差）
  # 残差部分
  remainder5 <- stl_decomp5$time.series[, 3]
  # 对平稳序列检验
  adf_stationary <- adf.test(remainder5)
  print(adf_stationary) # p值<0.05（拒绝原假设，认为平稳）
  # 自相关检验
  Box_test <- Box.test(remainder2,type = "Ljung-Box",lag = 10)
  print(Box_test)

# 整合剩余数据
data_remainder = cbind(remainder1, remainder2, remainder3, remainder4, remainder5)

# ACF和PACF图
acf(data_remainder[,1])
pacf(data_remainder[,1])

acf(data_remainder[,2])
pacf(data_remainder[,2])

acf(data_remainder[,3])
pacf(data_remainder[,3])

acf(data_remainder[,4])
pacf(data_remainder[,4])

acf(data_remainder[,5])
pacf(data_remainder[,5])

#多元混成检验
mq_test <- MTS::mq(data_remainder, lag = 240)  # 设置滞后阶数为240
                     #结果发现在划定周期内p-value均小于0.01

# 用前70%作训练集拟合VARMA模型
varma_model <- VARMA(data_remainder[1:(0.7*n),], p = 1,q =1)
summary(varma_model)

# 残差诊断
MTSplot(varma_model$residuals)
mq(varma_model$residuals, lag = 240)

# 检查VAR部分的平稳性-----------------------------------------------------------
# 提取系数矩阵
names(varma_model)
Phi <- varma_model$Phi
Theta <- varma_model$Theta

# 计算矩阵Phi的特征值
eigen_values <- eigen(Phi)$values
cat("VAR部分的特征值模长:", Mod(eigen_values), "\n")
if (all(Mod(eigen_values) < 1)) {
  cat("VAR部分满足平稳性条件\n")
} else {
  cat("VAR部分不平稳\n")
}

# 检查VMA部分的可逆性
# 计算矩阵Theta的特征值
eigen_values_ma <- eigen(Theta)$values
cat("VMA部分的特征值模长:", Mod(eigen_values_ma), "\n")
if (all(Mod(eigen_values_ma) < 1)) {
  cat("VMA部分满足可逆性条件\n")
} else {
  cat("VMA部分不可逆\n")
}

# 静态预测——————————————————————————————————————————————————————————————————————
forecast <- VARMApred(varma_model, h = 0.3*n)
print(forecast)
# 提取预测值和置信区间
forecast_mean <- forecast$pred          # 预测均值
forecast_lower <- forecast$pred - 1.96 * forecast$se.err  # 95%置信下限
forecast_upper <- forecast$pred + 1.96 * forecast$se.err  # 95%置信上限
real = remainder5[(0.7*n+1):length(remainder5)]
par(mfrow = c(2,2))
plot(forecast_mean[,5],main = "forecast_mean",xlab = "天数",ylab = "forecast_mean",ylim = c(-10,10))
plot(forecast_lower[,5],main = "forecast_lower",xlab = "天数",ylab = "forecast_lower",ylim = c(-80,0))
plot(forecast_upper[,5],main = "forecast_upper",xlab = "天数",ylab = "forecast_upper",ylim = c(50,100))
plot(real,main = "real",xlab = "天数",ylab = "real",ylim = c(-100,200))
###同时显示
time <- seq(1,750)
data1 <- forecast_mean[,5]
data2 <- forecast_lower[,5]
data3 <- forecast_upper[,5]
data4 <- real

# 绘制第一个数据系列（初始化画布）
par(mfrow = c(1,1))
plot(time, data1, 
     type = "l", 
     col = "#EF5675", 
     ylim = c(-80, 80),  # 统一y轴范围
     xlab = "时间", 
     ylab = "PM2.5", 
     main = "后30%训练集")

# 添加其他三组数据
lines(time, data2, col = "#FF764A")    # 第二组数据
lines(time, data3, col = "#7A5195")   # 第三组数据
lines(time, data4, col = "#3A5F85")  # 第四组数据

# 添加图例
legend("topright", 
       legend = c("均值",  "下限", "上限", "实际"),
       col = c("#EF5675", "#FF764A", "#7A5195", "#3A5F85"),
       lty = 1)


###固定模型系数的滚动预测———————————————————————————————————————————————————————
initial_train <- data_remainder[1:(0.7*n), ]
# 提取模型参数
Phi <- varma_model$Phi
Theta <- varma_model$Theta
k <- ncol(initial_train)  # 变量数
p <- 1  # VAR阶数
q <- 1  # VMA阶数

# 初始化历史数据窗口（矩阵格式）
history_y <- matrix(tail(initial_train, p), nrow = p, ncol = k)
history_epsilon <- matrix(tail(varma_model$residuals, q), nrow = q, ncol = k)

# 准备测试集
test_start <- floor(0.7 * n) + 1
test_data <- data_remainder[test_start:n, , drop = FALSE]
test_size <- nrow(test_data)

# 存储预测结果
rolling_forecasts <- matrix(NA, nrow = test_size, ncol = k)
colnames(rolling_forecasts) <- colnames(data_remainder)

# 滚动预测循环
for (i in 1:test_size) {
  # 预测计算
  y_prev <- history_y[nrow(history_y), , drop = FALSE]  # 保持矩阵格式
  epsilon_prev <- history_epsilon[nrow(history_epsilon), , drop = FALSE]
  forecast <- y_prev %*% t(Phi) + epsilon_prev %*% t(Theta)
  
  # 保存预测值
  rolling_forecasts[i, ] <- forecast
  
  # 获取实际值并计算残差
  actual <- matrix(test_data[i, ], nrow = 1, ncol = k)  # 强制为矩阵
  epsilon_new <- actual - forecast
  
  # 更新历史窗口（滑动机制）
  history_y <- rbind(history_y[-1, , drop = FALSE], actual)
  if (nrow(history_epsilon) >= q) {
    history_epsilon <- rbind(history_epsilon[-1, , drop = FALSE], epsilon_new)
  } else {
    history_epsilon <- rbind(history_epsilon, epsilon_new)
  }
}

# 对比预测结果和实际值
#同时显示
time <- seq(1,750)
data1 <- rolling_forecasts[,5]
data2 <- real

# 绘制第一个数据系列（初始化画布）
par(mfrow = c(1,1))
plot(time, data1, 
     type = "l", 
     col = "#EF5675", 
     ylim = c(-80, 80),  # 统一y轴范围
     xlab = "epoch", 
     ylab = "PM2.5", 
     )

# 添加另一组数据
lines(time, data2, col = "#67A9CF")    # 第二组数据

# 添加图例
legend("topright", 
       legend = c("预测",  "实际"),
       col = c("#EF5675", "#67A9CF"),
       lty = 1)

c("#2166AC", "#67A9CF", "#D1E5F0", "#FDDBC7", "#EF8A62", "#B2182B")



##计算MSE和MAE——————————————————————————————————————————————————————————————————
# 定义评估函数（输入实际值和预测值矩阵）
calculate_metrics <- function(actual, forecast) {
  # 确保输入为矩阵或数据框
  actual <- as.matrix(actual)
  forecast <- as.matrix(forecast)
  
  # 计算各变量的MSE和MAE
  mse <- colMeans((actual - forecast)^2, na.rm = TRUE)
  mae <- colMeans(abs(actual - forecast), na.rm = TRUE)
  
  # 整理结果
  metrics <- data.frame(
    Variable = colnames(actual),
    MSE = round(mse, 4),
    MAE = round(mae, 4)
  )
  return(metrics)
}
# 调用评估函数
metrics_result <- calculate_metrics(
  actual = test_data,
  forecast = rolling_forecasts
)

# 打印结果
cat("滚动预测评估结果:\n")
print(metrics_result)

###预测误差分布—————————————————————————————————————————————————————————————————
library(ggplot2)

# 通过列索引提取数据（假设第一列是目标变量）
errors <- data.frame(
  Error = test_data[, 1] - rolling_forecasts[, 1]
)

# 绘制误差分布图
ggplot(errors, aes(x = Error)) +
  geom_histogram(aes(y = ..density..), bins = 15, fill = "blue", alpha = 0.6) +
  geom_density(color = "red") +
  labs(title = "预测误差分布", x = "误差", y = "密度") +
  theme_minimal()
