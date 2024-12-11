source("hpts_excel.R")
source("./nanshan_indexes.R")

library(openxlsx)
library(dplyr)
library(tableone)
library(showtext)

# 图片显示中文
showtext.auto()

DataColStr2Num <- function(data, columns=c()) {
  # 如果columns为空，使用默认列表
  if (length(columns) == 0) {
    columns <- c("身高", "体重", "BMI", "BSA", "白细胞计数", "红细胞计数", "血红蛋白", "血小板计数", "中性粒细胞百分比", "淋巴细胞百分比", "单核细胞百分比", "嗜酸性粒细胞百分比", "嗜碱性粒细胞百分比", "中性粒细胞绝对值", "淋巴细胞绝对值", "单核细胞绝对值", "嗜酸性粒细胞绝对值", "嗜碱性粒细胞绝对值", "红细胞压积", "平均红细胞体积", "平均红细胞Hb含量", "平均红细胞Hb浓度", "红细胞分布宽度(CV)", "红细胞分布宽度(SD)", "血小板压积", "血小板分布宽度", "平均血小板体积", "大血小板比例", "尿素", "肌酐", "估算肾小球滤过率", "尿酸", "钾", "钠", "氯", "总钙", "肌酸激酶", "肌酸激酶同工酶", "乳酸脱氢酶", "α-羟丁酸脱氢酶", "肌红蛋白", "同型半胱氨酸", "超敏C反应蛋白测定（全程）", "游离三碘甲状腺原氨酸", "游离甲状腺素", "超敏促甲状腺素", "糖化血红蛋白", "总蛋白", "白蛋白", "球蛋白", "白球比例", "总胆红素", "直接胆红素", "间接胆红素", "天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶", "碱性磷酸酶", "γ-谷氨酰基转移酶", "总胆固醇", "甘油三酯", "高密度脂蛋白胆固醇", "低密度脂蛋白胆固醇", "非HDL胆固醇", "葡萄糖", "高密低密之和", "尿微量白蛋白", "肌酐(尿)", "尿微量白蛋白/肌酐比值", "游离甲氧基肾上腺素", "游离甲氧基去甲肾上腺素", "Free（MN+NMN）", "肾动脉水平腹主动脉PSV", "左肾动脉主干PSV", "右肾动脉主干PSV", "左肾叶间动脉PSV", "右肾叶间动脉PSV", "RCCA_内径", "RCCA_PSV", "RCCA_EDV", "LCCA_内径", "LCCA_PSV", "LCCA_EDV", "RICA_内径", "RICA_PSV", "RICA_EDV", "LICA_内径", "LICA_PSV", "LICA_EDV", "RVA_内径", "RVA_PSV", "RVA_EDV", "LVA_内径", "LVA_PSV", "LVA_EDV", "左侧内中膜", "右侧内中膜", "主动脉窦部内径", "主动脉瓣环前后径", "升主动脉内径", "左房前后径", "右室前壁厚度", "右室前后径", "室间隔厚度", "左室后壁厚度", "左室舒张末径", "左室收缩末径", "左室射血分数", "左室短轴缩短率", "肺动脉内径", "右房左右径", "肺动脉瓣口血流速度", "24小时收缩压平均值", "24小时舒张压平均值", "白天收缩压平均值", "白天舒张压平均值", "夜间收缩压平均值", "夜间舒张压平均值", "红细胞成熟度", "网织红细胞比值", "网织红细胞计数", "低荧光网红细胞比值", "中荧光网红细胞比值", "高荧光网红细胞比值")
  }

  for (col in columns) {
    # 检查列是否存在于数据框中
    if (col %in% names(data)) {
      # 转换数据类型
      data[[col]] <- as.numeric(as.character(data[[col]])) 
    }
  }
  return(data)
}

data <- read.xlsx("./excel/数据内容1.xlsx", sheet = 1)

# 假设您的数据框名为data
# 将“血糖”从mmol/L转换为mg/dL
data$`葡萄糖_mg_dl` <-  as.numeric(as.character(data$`葡萄糖`)) * 18

# 字符类型转换为数据类型
data <- DataColStr2Num(data)

data$LVMI <- LVMI(data$`室间隔厚度`, data$`左室后壁厚度`, data$`左室舒张末径`, data$`BSA`)
data$HGI <- HGI(data$`葡萄糖_mg_dl`, data$`糖化血红蛋白`)
data$PHR <- PHR(data$`血小板计数`, data$`高密度脂蛋白胆固醇`)
data$METS_IR <- METS_IR(data$`葡萄糖_mg_dl`, data$`总胆固醇`, data$`体重`, data$`身高`, data$`高密度脂蛋白胆固醇`)
data$AIP <- AIP(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`)
data$UHR <- UHR(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`)

data <- data %>%
  mutate(
    `颈动脉靶器官损害` = ifelse(`左侧内中膜` > 0.9 | grepl("斑块", `颈动脉彩超(颈总颈内颈外椎动脉锁骨下)+图文报告诊断意见`),
        1,
        0
    )
  )

data <- data %>%
  mutate(
    `心脏靶器官损害` = ifelse(
      LVMI > 120,
      1,
      0
    )
  )

# 计算指数
blood_list <- c(
  "白细胞计数", "红细胞计数", "血红蛋白", "血小板计数", "中性粒细胞百分比", "淋巴细胞百分比", "单核细胞百分比", "嗜酸性粒细胞百分比", "嗜碱性粒细胞百分比", "中性粒细胞绝对值", "淋巴细胞绝对值", "单核细胞绝对值", "嗜酸性粒细胞绝对值", "嗜碱性粒细胞绝对值", "红细胞压积", "平均红细胞体积", "平均红细胞Hb含量", "平均红细胞Hb浓度", "红细胞分布宽度(CV)", "红细胞分布宽度(SD)", "血小板压积", "血小板分布宽度", "平均血小板体积", "大血小板比例", "尿素", "肌酐", "估算肾小球滤过率", "尿酸", "钾", "钠", "氯", "总钙", "肌酸激酶", "肌酸激酶同工酶", "乳酸脱氢酶", "α-羟丁酸脱氢酶", "肌红蛋白", "同型半胱氨酸", "超敏C反应蛋白测定（全程）", "游离三碘甲状腺原氨酸", "游离甲状腺素", "超敏促甲状腺素", "糖化血红蛋白", "总蛋白", "白蛋白", "球蛋白", "白球比例", "总胆红素", "直接胆红素", "间接胆红素", "天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶", "碱性磷酸酶", "γ-谷氨酰基转移酶", "总胆固醇", "甘油三酯", "高密度脂蛋白胆固醇", "低密度脂蛋白胆固醇", "非HDL胆固醇", "葡萄糖", "高密低密之和", 
  "游离甲氧基肾上腺素", "游离甲氧基去甲肾上腺素", "Free（MN+NMN）", 
  "红细胞成熟度", "网织红细胞比值", "网织红细胞计数", "低荧光网红细胞比值", "中荧光网红细胞比值", "高荧光网红细胞比值"
)
ultrasound_list <- c("肾动脉水平腹主动脉PSV", "左肾动脉主干PSV", "右肾动脉主干PSV", "左肾叶间动脉PSV", "右肾叶间动脉PSV", "RCCA_内径", "RCCA_PSV", "RCCA_EDV", "LCCA_内径", "LCCA_PSV", "LCCA_EDV", "RICA_内径", "RICA_PSV", "RICA_EDV", "LICA_内径", "LICA_PSV", "LICA_EDV", "RVA_内径", "RVA_PSV", "RVA_EDV", "LVA_内径", "LVA_PSV", "LVA_EDV", "左侧内中膜", "右侧内中膜", "主动脉窦部内径", "主动脉瓣环前后径", "升主动脉内径", "左房前后径", "右室前壁厚度", "右室前后径", "室间隔厚度", "左室后壁厚度", "左室舒张末径", "左室收缩末径", "左室射血分数", "左室短轴缩短率", "肺动脉内径", "右房左右径", "肺动脉瓣口血流速度")
blood_pressure_list <- c("24小时收缩压平均值", "24小时舒张压平均值", "白天收缩压平均值", "白天舒张压平均值", "夜间收缩压平均值", "夜间舒张压平均值")
other_list <- c("尿微量白蛋白", "肌酐(尿)", "尿微量白蛋白/肌酐比值")

indexes_list <- c("BMI", "BRI", "LVMI", "HGI", "PHR", "METS_IR", "AIP", "UHR")

# 绘制table one
tableone_vars <- c(blood_list, ultrasound_list, blood_pressure_list, other_list, indexes_list)
the_table1 <- CreateTableOne(vars = tableone_vars, 
  strata = "颈动脉靶器官损害", 
  data = data)

the_table1_1 <- CreateTableOne(vars = tableone_vars, 
  strata = "心脏靶器官损害", 
  data = data)

library(ggplot2)

# 循环针对tableone_vars所有变量，拟合平滑曲线
p <- ggplot(data, aes(x = UHR, y = `LVMI`)) +
  geom_point() +  # 添加散点图
  geom_smooth(method = "lm", formula = y ~ x, color = "blue")  # 添加平滑曲线

print(p)