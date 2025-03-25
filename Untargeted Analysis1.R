
data <- read.csv("D:\\20250227非靶原始数据.csv", fileEncoding = "GBK")

names(data)

data$Retention_time_rounded <- round(data$`Retention.time..min.`, 2)

result <- data[do.call(order, data.frame(data$Retention_time_rounded, -data$`Maximum.Abundance`)), ]
result <- result[!duplicated(data$Retention_time_rounded), ]

print(result)

write.csv(result, "D:\\20250227非靶分析完数据（1）.csv", row.names = FALSE)

