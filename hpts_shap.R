# 引用包
library(openxlsx)
library(xgboost)
library(shapr)
library(SHAPforxgboost)
library(caret)
library(pROC)
library(tibble)

# PA患者发生CVD风险事件的机器学习

# 引用文件
source("hpts_excel.R")

baseline_df <- stu_read_excel(excel_file = "excel/基线数据.xlsx", sheet_list =  c("基线生化", "血压", "病史","药物"))
#baseline_df <- na.omit(baseline_df)

# 转换为因子
# 是否诊断PA、是否诊断原高、性别（男1、女0）、冠心病、脑卒中、糖尿病、房颤、目前是否吸烟、饮酒、ACEIs/ARBs/ARNI、a受体阻滞剂、B受体阻滞剂、利尿剂、钙通道阻滞剂、降脂药、抗凝剂、螺内酯、降糖药

# 增加一列CVD，如果冠心病或脑卒中为1，则CVD为1，否则为0
baseline_df$CVD <- ifelse(baseline_df$`冠心病` == 1 | baseline_df$`脑卒中` == 1, 1, 0)

# 筛选“是否诊断PA”为1的数据
baseline_df <- baseline_df[baseline_df$`是否诊断PA` == 1,]

# 转换为因子
baseline_df$`是否诊断PA` <- as.numeric(baseline_df$`是否诊断PA`)
baseline_df$CVD <- as.numeric(baseline_df$CVD)
baseline_df$`是否诊断原高` <- as.factor(baseline_df$`是否诊断原高`)
baseline_df$`性别（男1、女0）` <- as.factor(baseline_df$`性别（男1、女0）`)
baseline_df$`冠心病` <- as.factor(baseline_df$`冠心病`)
baseline_df$`脑卒中` <- as.factor(baseline_df$`脑卒中`)
baseline_df$`糖尿病` <- as.factor(baseline_df$`糖尿病`)
baseline_df$`房颤` <- as.factor(baseline_df$`房颤`)
baseline_df$`目前是否吸烟` <- as.factor(baseline_df$`目前是否吸烟`)
baseline_df$`饮酒` <- as.factor(baseline_df$`饮酒`)
baseline_df$`ACEIs/ARBs/ARNI` <- as.factor(baseline_df$`ACEIs/ARBs/ARNI`)
baseline_df$`a受体阻滞剂` <- as.factor(baseline_df$`a受体阻滞剂`)
baseline_df$`B受体阻滞剂` <- as.factor(baseline_df$`B受体阻滞剂`)
baseline_df$`利尿剂` <- as.factor(baseline_df$`利尿剂`)
baseline_df$`钙通道阻滞剂` <- as.factor(baseline_df$`钙通道阻滞剂`)
baseline_df$`降脂药` <- as.factor(baseline_df$`降脂药`)
baseline_df$`抗凝剂` <- as.factor(baseline_df$`抗凝剂`)
baseline_df$`螺内酯` <- as.factor(baseline_df$`螺内酯`)
baseline_df$`降糖药` <- as.factor(baseline_df$`降糖药`)


# 将需要分析的提取出来
baseline_df_sample <- baseline_df[,c("CVD", "BMI", 
  #"BSA", 
  "AHI指数", "糖化血红蛋白", 
  "白细胞", 
  "中性粒细胞", "淋巴细胞", "血小板", "AST", 
  "ALT", 
  "甘油三酯", "LDL-C", "载脂蛋白-B", "脂蛋白a", "肌酐", 
  #"eGFR", "钾",
  #"卧位醛固酮（5点）", "卧位肾素（5点）", "卧位ARR", "立位醛固酮（7点）", "立位肾素（7点）", "立位ARR", 
  #"血浆皮质醇测定（早上8:00）", "血浆皮质醇测定（下午16:00）", "血浆皮质醇测定（晚上0:00）", "24小时平均收缩压", "24小时平均舒张压", "24小时平均心率",
  "主动脉瓣环", "室间隔厚度", "左室射血分数"
)]

baseline_df_sample <- na.omit(baseline_df_sample)

# 对数据标准化
f1<-function(x){
  return((x-min(x)) / (max(x)-min(x)))
}

# 分成建模和验证组
set.seed(1234)
inTrain <- sample(nrow(baseline_df_sample), 0.7*nrow(baseline_df_sample))
baseline_df_sample_train <- baseline_df_sample[inTrain,]#70%数据集
baseline_df_sample_test<- baseline_df_sample[-inTrain,]#30%数据集

### 生成模型
# 向量机模型
#svm  <- svm(`是否诊断PA` ~ . ,data=baseline_df_sample.scale, probability = TRUE)

# XGboost模型
model_xgboost = xgboost(
  data = as.matrix(baseline_df_sample_train[,c(2:15)]),#训练集的自变量矩阵
                   label = baseline_df_sample_train$`CVD`,
                   max_depth = 3, 
                   eta = 1, 
                   nthread = 2, 
                   nrounds = 10,
                   objective = "binary:logistic")


# 预测
#pred_svm <- predict(svm, baseline_df_sample.scale, type = "prob")
pred_xgboost <- predict(model_xgboost, as.matrix(baseline_df_sample_test[,c(2:15)]), type = "prob")

# 绘制ROC曲线
roc_obj <- roc(response = baseline_df_sample_test$`CVD`, predictor = pred_xgboost)
plot(roc_obj, main = "ROC Curve CVD Predict for XGBoost Model")
abline(a = 0, b = 1, lty = 2, col = "gray")  # 添加对角线

# 计算AUC值
auc_value <- auc(roc_obj)
text(x = 0.5, y = 0.25, labels = paste("AUC =", round(auc_value, 4)), cex = 1.2, col = "#0077ff")

# 计算SHAP值
shap_values <- shap.values(model_xgboost, as.matrix(baseline_df_sample_train[,c(2:15)]))

# To prepare the long-format data:
shap_long <- shap.prep(xgb_model = model_xgboost, X_train = as.matrix(baseline_df_sample_train[,c(2:15)]),)
#shap.plot.summary(shap_long)