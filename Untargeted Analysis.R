data <- read.csv("D:\\20250227非靶原始数据.csv", fileEncoding = "GBK")
library(dplyr)
names(data)
data <- data %>%
  mutate(Retention_time_rounded = round(`Retention.time..min.`, 2))
result <- data %>%
  group_by(Retention_time_rounded) %>%
  top_n(1, `Maximum.Abundance`)
print(result)
write.csv(result, "D:\\20250227非靶分析完数据.csv", row.names = FALSE)

