source("hpts_base_vars_functions.R")
source("nanshan_indexes.R")

library(openxlsx)
library(dplyr)
library(compareGroups)
library(labelled)
library(showtext)
library(xgboost)
library(shapr)
library(SHAPforxgboost)
library(caret)
library(pROC)
library(tibble)

# 图片显示中文
showtext_auto()

DataColStr2Num <- function(data, columns=c()) {
  # 如果columns为空，使用默认列表
  if (length(columns) == 0) {
    columns <- c("身高", "体重", "BMI", "BSA", "白细胞计数", "红细胞计数", "血红蛋白", "血小板计数", "中性粒细胞百分比", "淋巴细胞百分比", "单核细胞百分比", "嗜酸性粒细胞百分比", "嗜碱性粒细胞百分比", "中性粒细胞绝对值", "淋巴细胞绝对值", "单核细胞绝对值", "嗜酸性粒细胞绝对值", "嗜碱性粒细胞绝对值", "红细胞压积", "平均红细胞体积", "平均红细胞Hb含量", "平均红细胞Hb浓度", "红细胞分布宽度(CV)", "红细胞分布宽度(SD)", "血小板压积", "血小板分布宽度", "平均血小板体积", "大血小板比例", "尿素", "肌酐", "估算肾小球滤过率", "尿酸", "钾", "钠", "氯", "总钙", "肌酸激酶", "肌酸激酶同工酶", "乳酸脱氢酶", "α-羟丁酸脱氢酶", "肌红蛋白", "同型半胱氨酸", "超敏C反应蛋白测定（全程）", "游离三碘甲状腺原氨酸", "游离甲状腺素", "超敏促甲状腺素", "糖化血红蛋白", "总蛋白", "白蛋白", "球蛋白", "白球比例", "总胆红素", "直接胆红素", "间接胆红素", "天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶", "碱性磷酸酶", "γ-谷氨酰基转移酶", "总胆固醇", "甘油三酯", "高密度脂蛋白胆固醇", "低密度脂蛋白胆固醇", "非HDL胆固醇", "葡萄糖", "高密低密之和",  "尿微量白蛋白", "肌酐(尿)", "尿微量白蛋白/肌酐比值", "游离甲氧基肾上腺素", "游离甲氧基去甲肾上腺素", "Free（MN+NMN）", "肾动脉水平腹主动脉PSV", "左肾动脉主干PSV", "右肾动脉主干PSV", "左肾叶间动脉PSV", "右肾叶间动脉PSV", "RCCA_内径", "RCCA_PSV", "RCCA_EDV", "LCCA_内径", "LCCA_PSV", "LCCA_EDV", "RICA_内径", "RICA_PSV", "RICA_EDV", "LICA_内径", "LICA_PSV", "LICA_EDV", "RVA_内径", "RVA_PSV", "RVA_EDV", "LVA_内径", "LVA_PSV", "LVA_EDV", "左侧内中膜", "右侧内中膜", "主动脉窦部内径", "主动脉瓣环前后径", "升主动脉内径", "左房前后径", "右室前壁厚度", "右室前后径", "室间隔厚度", "左室后壁厚度", "左室舒张末径", "左室收缩末径", "左室射血分数", "左室短轴缩短率", "肺动脉内径", "右房左右径", "肺动脉瓣口血流速度", "24小时收缩压平均值", "24小时舒张压平均值", "白天收缩压平均值", "白天舒张压平均值", "夜间收缩压平均值", "夜间舒张压平均值", "红细胞成熟度", "网织红细胞比值", "网织红细胞计数", "低荧光网红细胞比值", "中荧光网红细胞比值", "高荧光网红细胞比值")
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

data <- read.xlsx("./excel/数据内容_ARR.xlsx", sheet = 1)
data <- data[1:175, ] # 去除无用行
data <- FormatDataColumnsKuohao(data)

#data1 <- readxl::read_xlsx("excel/ARR.xlsx", sheet = 2, range = "C1:S135") %>%
#  select(-c("登记号", "性别", "年龄"))

#data <- FormatRegisterID(data, column = "ID")
#data <- FormatName(data, column = "ID")
#data1 <- FormatRegisterID(data1, column = "登记号")
#data1$`入院时间` <- as.character(data1$`入院时间`)

#data2 <- full_join(data, data1, by = c("姓名")) %>%
#  select("ID", "登记号", "姓名", "性别", "年龄", "身高", "体重", "BMI", "BSA", everything())

#data <- data %>%
#  select("ID", "登记号", "姓名", "性别", "年龄", "身高", "体重", "BMI", "BSA", everything()) %>%
#  select(-c("Sex_Hb"))

# 假设您的数据框名为data
# 将“血糖”从mmol/L转换为mg/dL
data$`葡萄糖_mg_dl` <-  as.numeric(as.character(data$`葡萄糖`)) * 18
data$`肌酐_mg_dl` <- as.numeric(as.character(data$`肌酐`)) / 88.4

# 字符类型转换为数据类型
data <- DataColStr2Num(data)

data$LVMI <- LVMI(data$`室间隔厚度`, data$`左室后壁厚度`, data$`左室舒张末径`, data$`BSA`)
data$HGI <- HGI(data$`葡萄糖_mg_dl`, data$`糖化血红蛋白`)
data$PHR <- PHR(data$`血小板计数`, data$`高密度脂蛋白胆固醇`)
data$METS_IR <- METS_IR(data$`葡萄糖_mg_dl`, data$`总胆固醇`, data$`体重`, data$`身高`, data$`高密度脂蛋白胆固醇`)
data$AIP <- AIP(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`)
data$SII <- SII(data$`中性粒细胞绝对值`, data$`血小板计数`, data$`淋巴细胞绝对值`)
data$RC <- RC(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`, data$`低密度脂蛋白胆固醇`)
data$UHR <- UHR(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`)
data$NLR <- NLR(data$`中性粒细胞绝对值`, data$`淋巴细胞绝对值`)
data$NHR <- NHR(data$`中性粒细胞绝对值`, data$`高密度脂蛋白胆固醇`)
data$NHHR <- NHHR(data$`总胆固醇`, data$`高密度脂蛋白胆固醇`)
data$DCS <- DCS(data$`血浆皮质醇早上8:00`, data$`血浆皮质醇晚上0:00`)
data$ALI <- ALI(data$`体重`, data$`身高`, data$`白蛋白`, data$`中性粒细胞绝对值`, data$`淋巴细胞绝对值`)
data$MDRD_eGFR <- MDRD_eGFR(data$`肌酐_mg_dl`, data$`性别`, data$`年龄`)
data$RAR_index <- RAR_index(data$`红细胞分布宽度_SD_`, data$`白蛋白`)

# 整理性别变量
data <- data %>%
  mutate(
    `性别` = case_when(
      `性别` == "男" ~ 1,
      `性别` == "女" ~ 2,
      TRUE ~ NA_integer_
    )
  )

# 颈动脉内膜平均厚度
data <- data %>%
  mutate(
    # 左侧右侧/2
    `颈动脉内膜厚度` = (`左侧内中膜` + `右侧内中膜`) / 2
  )

data <- data %>%
  mutate(
    `颈动脉靶器官损害` = ifelse(`左侧内中膜` > 0.9 | `右侧内中膜` > 0.9 | grepl("斑块", `颈动脉彩超_颈总颈内颈外椎动脉锁骨下_+图文报告诊断意见`),
        1,
        0
    )
  )

data <- data %>%
  mutate(
    # LVMI 男大于115，女大于95，默认大于95
    `心脏靶器官损害` = ifelse(
      `性别` == 1,
      ifelse(LVMI >= 109, 1, 0),
      # 默认NA和女大于95为1
      ifelse(LVMI >= 105, 1, 0)
    )
  )

data <- data %>%
  mutate(
    `皮质醇节律异常` = ifelse(
      (`血浆皮质醇下午16:00` / `血浆皮质醇早上8:00` > 0.5) | (`血浆皮质醇晚上0:00` / `血浆皮质醇早上8:00` > 0.5),
      1,
      0
    )
  )

data <- data %>%
  mutate(
    # eGFR
    `肾脏损害` = ifelse(
        # 30 <= eGFR <= 59
        #MDRD_eGFR < 60 | (`性别` == 1 & (`肌酐` >= 115) & (`肌酐` < 133)) | (`性别` == 2 & (`肌酐` >= 107) & (`肌酐` < 124)),
        MDRD_eGFR < 60 | (`性别` == 1 & (`肌酐` >= 115)) | (`性别` == 2 & (`肌酐` >= 107)),
        1,
        0
    )
  )

# 计算指数
# 修正列名
# 嗜碱性粒胞绝对值 - 嗜碱性粒细胞绝对值
# ACTH - "促肾上腺皮质激素"
data$`嗜碱性粒细胞绝对值` <- as.numeric(data$`嗜碱性粒胞绝对值`)
data$`嗜碱性粒胞绝对值` <- as.numeric(data$`嗜碱性粒胞绝对值`)
data$`促肾上腺皮质激素` <- as.numeric(data$`促肾上腺皮质激素`)
data$`ACTH` <- as.numeric(data$`ACTH`)

data <- data %>%
  mutate(
    ACTH = ifelse(is.na(`促肾上腺皮质激素`), ACTH, `促肾上腺皮质激素`),
    `嗜碱性粒细胞绝对值` = ifelse(is.na(`嗜碱性粒细胞绝对值`), `嗜碱性粒胞绝对值`, `嗜碱性粒细胞绝对值`)
  )

biomaker_list <- c("性别", "年龄", "身高", "体重", "BMI", "BSA")


blood_list <- c( "白细胞计数", "红细胞计数", "血红蛋白", "血小板计数", "中性粒细胞百分比", "淋巴细胞百分比", "单核细胞百分比", "嗜酸性粒细胞百分比", "嗜碱性粒细胞百分比", "中性粒细胞绝对值", "淋巴细胞绝对值", "单核细胞绝对值", "嗜酸性粒细胞绝对值", "嗜碱性粒细胞绝对值", "红细胞压积", "平均红细胞体积", "平均红细胞Hb含量", "平均红细胞Hb浓度", "红细胞分布宽度_CV_", "红细胞分布宽度_SD_", "血小板压积", "血小板分布宽度", "平均血小板体积", "大血小板比例", "尿素", "肌酐", "估算肾小球滤过率", "尿酸", "钾", "钠", "氯", "总钙", "肌酸激酶", "肌酸激酶同工酶", "乳酸脱氢酶", "α-羟丁酸脱氢酶", "肌红蛋白", "同型半胱氨酸", "超敏C反应蛋白测定_全程_", "游离三碘甲状腺原氨酸", "游离甲状腺素", "超敏促甲状腺素", "糖化血红蛋白", "总蛋白", "白蛋白", "球蛋白", "白球比例", "总胆红素", "直接胆红素", "间接胆红素", "天门冬氨酸氨基转移酶", "丙氨酸氨基转移酶", "碱性磷酸酶", "γ-谷氨酰基转移酶", "总胆固醇", "甘油三酯", "高密度脂蛋白胆固醇", "低密度脂蛋白胆固醇", "非HDL胆固醇", "葡萄糖", "空腹血糖", "60分钟血糖", "120分钟血糖", "高密低密之和"
  )


urine_list <- c("24h尿蛋白定量", "尿蛋白浓度", "尿微量白蛋白", "肌酐_尿_", "尿微量白蛋白/肌酐比值", "尿检_钾", "尿检_钠", "尿检_氯", "尿检_总钙")
renal_list <- c(
  "游离甲氧基肾上腺素", "游离甲氧基去甲肾上腺素", "Free_MN+NMN_", 
  "醛固酮立位_7点_", "肾素活性立位", "醛固酮卡托普利试验后", "肾素活性卡托普利试验后", "醛固酮_6点_", "醛固酮_10点_", 
  "血浆皮质醇早上8:00", "血浆皮质醇下午16:00", "血浆皮质醇晚上0:00", 
  "皮质醇_早上08:00_", "皮质醇_下午16:00_", "皮质醇_晚上00:00_"
)

ultrasound_list <- c("肾动脉水平腹主动脉PSV", "左肾动脉主干PSV", "右肾动脉主干PSV", "左肾叶间动脉PSV", "右肾叶间动脉PSV", "RCCA_内径", "RCCA_PSV", "RCCA_EDV", "LCCA_内径", "LCCA_PSV", "LCCA_EDV", "RICA_内径", "RICA_PSV", "RICA_EDV", "LICA_内径", "LICA_PSV", "LICA_EDV", "RVA_内径", "RVA_PSV", "RVA_EDV", "LVA_内径", "LVA_PSV", "LVA_EDV", "左侧内中膜", "右侧内中膜", "主动脉窦部内径", "主动脉瓣环前后径", "升主动脉内径", "左房前后径", "右室前壁厚度", "右室前后径", "室间隔厚度", "左室后壁厚度", "左室舒张末径", "左室收缩末径", "左室射血分数", "左室短轴缩短率", "肺动脉内径", "右房左右径", "肺动脉瓣口血流速度", "主动脉瓣口血流速度")
blood_pressure_list <- c("24小时收缩压平均值", "24小时舒张压平均值", "白天收缩压平均值", "白天舒张压平均值", "夜间收缩压平均值", "夜间舒张压平均值", "是否为继发性高血压")
other_list <- c(
  "磷", "镁", "α-L-岩藻糖苷酶", "载脂蛋白-A1", "载脂蛋白-B", "载脂蛋白E", "脂蛋白a", "谷胱甘肽还原酶", 
  "甲胎蛋白", "癌胚抗原", "铁蛋白", "糖类抗原-199"
)

indexes_list <- c(
  #"BMI", "BRI",
  "LVMI", "HGI","PHR", 
  "SII", "RC",
  "NLR", "NHR", "NHHR", 
  #"ALI", "RAR_index",  # 太多NA
  #"METS_IR", 
  "AIP",
  "DCS",
  "UHR"
)

# 获取LVMI的四分位数切点
lvmi_quantiles <- quantile(data$LVMI, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

# 创建一个新的变量用于存储分组标签
data$`LVMI Group` <- cut(data$LVMI, 
                       breaks = c(-Inf, lvmi_quantiles, Inf), 
                       labels = c("Q1", "Q2", "Q3", "Q4"), 
                       include.lowest = TRUE)

data$`MDRD_eGFR Group` <- cut(data$MDRD_eGFR, 
  breaks = c(-Inf, 60, Inf), 
  labels = c("lower", "higher"), 
  include.lowest = TRUE)

DCS_median <- median(data$DCS, na.rm = TRUE)
data$`DCS Group` <- cut(data$DCS, 
  breaks = c(-Inf, DCS_median, Inf), 
  labels = c("DCS lower", "DCS higher"), 
  include.lowest = TRUE)

# 绘制table one
tableone_vars <- c(biomaker_list, blood_list, ultrasound_list, blood_pressure_list, other_list, indexes_list)
# 去除数据不足变量
tableone_vars <- setdiff(tableone_vars, "超敏C反应蛋白测定_全程_")

tableone_data <- data[, c(biomaker_list, blood_list, urine_list, ultrasound_list, blood_pressure_list, c("MDRD_eGFR", "MDRD_eGFR Group", "LVMI Group", "DCS Group"), indexes_list, c("颈动脉靶器官损害","心脏靶器官损害", "皮质醇节律异常"))]  #%>%
#  select(-`超敏C反应蛋白测定_全程_`) #%>%
#  select(-`LVMI`)

# 将变量转化为因子
tableone_data$`性别` <- factor(tableone_data$`性别`, levels = c(1, 2), labels = c("男", "女"))

# 将颈动脉内膜损害、心脏靶器官损害转换为factor因子
#tableone_data$`颈动脉靶器官损害` <- factor(tableone_data$`颈动脉靶器官损害`, levels = c(0, 1), labels = c("未发生颈动脉内膜损害", "颈动脉内膜损害"))
#tableone_data$`心脏靶器官损害` <- factor(tableone_data$`心脏靶器官损害`, levels = c(0, 1), labels = c("未发生心脏靶器官损害", "心脏靶器官损害"))
#tableone_data$`皮质醇节律异常` <- factor(tableone_data$`皮质醇节律异常`, levels = c(0, 1), labels = c("皮质醇节律存在", "皮质醇节律异常"))

# 颈动脉靶器官损害、心脏靶器官损害
tableone_tid <- descrTable(
  `颈动脉靶器官损害` ~ .
    , data = tableone_data,
    sd.type = 3, hide.no = "no", include.label = FALSE
)

tableone_heart <- descrTable(
  `心脏靶器官损害` ~ .
    , data = tableone_data,
    sd.type = 3, hide.no = "no", include.label = FALSE
)

tableone_cort <- descrTable(
  `皮质醇节律异常` ~ .
    , data = tableone_data,
    sd.type = 3, hide.no = "no", include.label = FALSE
)

tableone_dcs <- descrTable(
  `DCS Group` ~ .
    , data = tableone_data,
    sd.type = 3, hide.no = "no", include.label = FALSE
)

# export2word导出
#export2word(tableone_tid, "颈动脉内膜基线.docx")
#export2word(tableone_heart, "心脏靶器官基线.docx")
#export2word(tableone_cort, "皮质醇节律基线.docx")

library(ggplot2)

# 循环针对tableone_vars所有变量，拟合平滑曲线
p <- ggplot(data, aes(x = DCS, y = `高密度脂蛋白胆固醇`)) +
  geom_point() +  # 添加散点图
  geom_smooth(method = "lm", formula = y ~ x, color = "blue")  # 添加平滑曲线

print(p)

# 加载必要的包
library(rms)
library(ggplot2)

# 删除data数据框中RAR_index列包含NA的行
data_rcs <- na.omit(data[, c("DCS", "心脏靶器官损害")])

# 假设你的数据存储在名为data的数据框中，先准备数据分布对象
dd <- datadist(data_rcs)
options(datadist = 'dd')

# 使用rcs()函数创建受限三次样条，这里同样以4个节点为例
# 拟合逻辑回归模型，以颈动脉靶器官损害为响应变量，RAR_index为自变量且应用受限三次样条变换
# 同时调整性别、年龄和BMI协变量
model <- lrm(`心脏靶器官损害` ~ rcs(DCS, 4) , data = data_rcs)

# 检查模型摘要
summary(model)

# 使用anova()提取模型的p值
anova_results <- anova(model)

print(anova_results)

# 提取p值
p_overall <- anova_results[1, 3]
p_nolinear <- anova_results[2, 3]

# 使用Predict()函数预测概率
pred <- Predict(model, DCS, fun = plogis, ref.zero = TRUE)

# 使用Predict()函数预测优势比
pred_or <- Predict(model, DCS, fun = exp, ref.zero = TRUE)

# 使用ggplot2绘制RCS图
# 预测概率
gg_predict <- ggplot(data = pred, aes(x = DCS, y = pred)) +
  geom_line(color = "#fe4d4b") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#ffb3b3", alpha = 0.8) +
  labs(title = "RCS for DCS and Heart Target Damage", x = "DCS", y = "Predicted Probability") +
  theme_minimal() + 
    annotate("text", x = 0.2, y = 0.9, 
      label = paste("p.overall=", round(p_overall, 4)), 
      size = 3, color = "black", hjust = 0, vjust = 0.2) +
    annotate("text", x = 0.2, y = 0.85, 
      label = paste("p.nolinear=", round(p_nolinear, 4)),
      size = 3, color = "black", hjust = 0, vjust = 0.2)

print(gg_predict)

# 包含OR值的RCS图
gg_or <- ggplot(data = pred_or, aes(x = DCS, y = pred)) +
  geom_line(color = "#fe4d4b") +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#ffb3b3", alpha = 0.8) +
  labs(title = "RCS for DCS and Heart Target Damage", 
       x = "DCS", 
       y = "Odds Ratio") +
  theme_minimal() + 
  annotate("text", x = 0.2, y = 4.9, 
    label = paste("p.overall=", round(p_overall, 4)), 
    size = 3, color = "black", hjust = 0, vjust = 1) +
  annotate("text", x = 0.2, y = 4.6, 
    label = paste("p.nolinear=", round(p_nolinear, 4)), 
    size = 3, color = "black", hjust = 0, vjust = 1)

print(gg_or)