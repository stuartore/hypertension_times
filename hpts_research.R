# 引用包
library(dplyr)
library(compareGroups)

# 引用文件
source("hpts_excel.R")
source("nanshan_indexes.R")

baseline_df <- stu_read_excel(excel_file = "excel/基线数据.xlsx", sheet_list =  c("基线生化", "血压", "病史","药物"))

# 根据ARR值进行针对原发性高血压进行分组，使用Q1、Q2、Q3进行分组
quantiles <- quantile(baseline_df$`卧位ARR`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
baseline_df$ARR_group <- cut(baseline_df$`卧位ARR`, 
                             breaks = c(-Inf, quantiles[1], quantiles[2], quantiles[3], Inf),
                             labels = c("low", "medium", "high", "very high"))

baseline_df$`葡萄糖mg_dl` <- as.numeric(as.character(baseline_df$`空腹血糖mmol/L`)) * 18
baseline_df$`肌酐_mg_dl` <- as.numeric(as.character(baseline_df$`肌酐`))

baseline_df <- baseline_df %>%
  mutate(
    `性别` = case_when(
      `性别（男1、女0）` == 1 ~ 1,
      `性别（男1、女0）` == 0 ~ 2
    )
  )

baseline_df$LVMI <- LVMI(baseline_df$`室间隔厚度`, baseline_df$`左室后壁厚度`, baseline_df$`左室收缩末径`, baseline_df$BSA)
baseline_df$PHR <- PHR(baseline_df$`血小板`, baseline_df$`HDL-C`)
baseline_df$METS_IR <- METS_IR(baseline_df$`葡萄糖mg_dl`, baseline_df$`总胆固醇`, baseline_df$`体重`, baseline_df$`身高`, baseline_df$`HDL-C`)
baseline_df$HGI <- HGI(baseline_df$`葡萄糖mg_dl`, baseline_df$`糖化血红蛋白`)
baseline_df$AIP <- AIP(baseline_df$`总胆固醇`, baseline_df$`HDL-C`)
baseline_df$NHHR <- NHHR(baseline_df$`总胆固醇`, baseline_df$`HDL-C`)
baseline_df$UHR <- UHR(baseline_df$`总胆固醇`, baseline_df$`HDL-C`)
baseline_df$MDRD_eGFR <- MDRD_eGFR(baseline_df$`肌酐_mg_dl`, baseline_df$`性别`, baseline_df$`年龄`)

# 皮质醇节律异常：若 16：00 及 24：00 皮质醇水平未下降至 8：00 皮质醇水平的 50%，则认为该患者皮质醇节律异常
baseline_df <- baseline_df %>%
  mutate(
    `皮质醇节律异常` = ifelse(
      (`血浆皮质醇测定（下午16:00）` / `血浆皮质醇测定（早上8:00）` > 0.5) | (`血浆皮质醇测定（晚上0:00）` / `血浆皮质醇测定（早上8:00）` > 0.5),
      1,
      0
    )
  )
baseline_df$`皮质醇节律异常` <- factor(baseline_df$`皮质醇节律异常`, levels = c(0, 1), labels = c("皮质醇节律无异常", "皮质醇节律异常"))

# 转化为因子
factor_columns <- c("冠心病", "脑卒中", "糖尿病", "房颤", "高血压/月", "目前是否吸烟", "饮酒", "ACEIs/ARBs/ARNI", "a受体阻滞剂", "B受体阻滞剂", "利尿剂", "钙通道阻滞剂", "降脂药", "抗凝剂", "螺内酯", "降糖药")
# 使用lapply对指定列进行转换，注意as.factor的参数设置方式需要调整正确
baseline_df[factor_columns] <- lapply(baseline_df[factor_columns], function(x) {
  factor(x, levels = c(0, 1), labels = c("无", "有"))
})

# 删除不需要出现在表格的变量
baseline_df <- baseline_df %>%
  select(-c("DIAG_PA", "METS_IR", "性别"))

baseline_df_ph <- baseline_df %>%
  filter(`是否诊断原高` == 1)

baseline_df_pa <- baseline_df %>%
  filter(`是否诊断PA` == 1)

# 制作基线特征表
# bio_data_summary_table <<- descrTable(`MyGroup` ~ . , data = bio_data[ , !(names(bio_data) %in% c('AHI_group'))],, sd.type = 3)
baseline_all_table1 <- descrTable(`皮质醇节律异常` ~ ., 
          data = baseline_df,
          sd.type = 3
        )

baseline_ph_table1 <- descrTable(`皮质醇节律异常` ~ ., 
        data = baseline_df_ph,
        sd.type = 3
      )

baseline_pa_table1 <- descrTable(`皮质醇节律异常` ~ ., 
      data = baseline_df_pa,
      sd.type = 3
    )

export2word(baseline_all_table1, "总体皮质醇节律基线.docx")
export2word(baseline_ph_table1, "原发性高血压皮质醇节律基线.docx")
export2word(baseline_pa_table1, "原发性醛固酮增多症皮质醇节律基线.docx")

# export2word(baseline_arr_group_table1, "baseline_arr_group_table1.docx")
# system("rm *.Rmd")
