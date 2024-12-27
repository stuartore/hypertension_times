source("hpts_base_vars_functions.R")
source("nanshan_indexes.R")

library(readxl)
library(dplyr)
library(compareGroups)
library(labelled)
library(showtext)

# Merge 2 Excel
#data <- read_xlsx("./excel/数据内容_ARR.xlsx", sheet = 1)
#data1 <- read_xlsx("./excel/数据内容1_ARR.xlsx", sheet = 1, range = "A1:FQ31")

#data2 <- rbind(data, data1) #%>%
  #select("ID", "入院时间", "登记号", "姓名", "性别", "年龄", "身高", "体重", "BMI", "BSA", everything())

# Get from python 3.12
data <- read_xlsx("./excel/数据内容.xlsx", sheet = 1)

data <- FormatNessary(data) %>%
  select(-c("入院时间", "年龄"))

# Read from WPS
data1 <- read_xlsx("excel/ARR.xlsx", sheet = 1, range = "C1:P533")

data2 <- read_xlsx("./excel/ARR.xlsx", sheet = 2, range = "C1:S130")

data1 <- FormatRegisterID(data1, "登记号")
data2 <- FormatRegisterID(data2, "登记号")
common_col_data1_2 <- intersect(names(data1), names(data2))

data3 <- full_join(data1, data2, by = common_col_data1_2)

common_col_data_3 <- intersect(names(data), names(data3))

data3 <- FormatCheckInHospitalTime(data3)
data4 <- full_join(data, data3, by = c("登记号", "姓名", "性别" )) %>%
  select("ID", "登记号", "姓名", "性别", "入院时间", "年龄", "身高", "体重", "BMI", "BSA", everything())

write.xlsx(data2, "./excel/数据内容_ARR.xlsx", sheet = 1)