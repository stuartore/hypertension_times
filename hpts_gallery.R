library(ggplot2)

# 引用文件
source("hpts_excel.R")

baseline_df <- stu_read_excel(excel_file = "excel/基线数据.xlsx", sheet_list =  c("基线生化", "血压", "病史","药物"))

# 根据ARR值进行针对原发性高血压进行分组，使用Q1、Q2、Q3进行分组
quantiles <- quantile(baseline_df$`卧位ARR`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
baseline_df$ARR_group <- cut(baseline_df$`卧位ARR`, 
                             breaks = c(-Inf, quantiles[1], quantiles[2], quantiles[3], Inf),
                             labels = c("low", "medium", "high", "very high"))

# 绘制小提琴图
# x = ARR_group, y = AST
ggplot(baseline_df, aes(x = ARR_group, y = AST)) +
    geom_violin() +
    labs(x = "ARR_group", y = "AST") +
    ggtitle("Violin Plot of AST by ARR group")

# x = ARR_group, y = ALT
ggplot(baseline_df, aes(x = ARR_group, y = AST)) +
  geom_violin() +
  labs(x = "ARR_group", y = "ALT") +
  ggtitle("Violin Plot of ALT by ARR group")

# x = ARR_group, y = `载脂蛋白-B`
ggplot(baseline_df, aes(x = ARR_group, y = `载脂蛋白-B` )) +
   geom_violin() +
   labs(x = "ARR_group", y = "Apo-B") +
   ggtitle("Violin Plot of Apo-B by ARR group")