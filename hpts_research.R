# 引用包
library(compareGroups)

# 引用文件
source("hpts_excel.R")

baseline_df <- stu_read_excel(excel_file = "excel/基线数据.xlsx", sheet_list =  c("基线生化", "血压", "病史","药物"))

# 根据ARR值进行针对原发性高血压进行分组，使用Q1、Q2、Q3进行分组
quantiles <- quantile(baseline_df$`卧位ARR`, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
baseline_df$ARR_group <- cut(baseline_df$`卧位ARR`, 
                             breaks = c(-Inf, quantiles[1], quantiles[2], quantiles[3], Inf),
                             labels = c("low", "medium", "high", "very high"))

# 制作基线特征表
# bio_data_summary_table <<- descrTable(`MyGroup` ~ . , data = bio_data[ , !(names(bio_data) %in% c('AHI_group'))],, sd.type = 3)
baseline_arr_group_table1 <- descrTable(`ARR_group` ~., data = baseline_df[,!(names(baseline_df) %in% c('AHI_group'))], sd.type = 3)


# export2word(baseline_arr_group_table1, "baseline_arr_group_table1.docx")
# system("rm *.Rmd")
