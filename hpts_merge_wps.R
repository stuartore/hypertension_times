source("hpts_base_vars_functions.R")
source("nanshan_indexes.R")

library(openxlsx)
library(dplyr)
library(compareGroups)
library(labelled)
library(showtext)

# Get from python 3.12
data <- read.xlsx("./excel/数据内容.xlsx", sheet = 1)
# Read from WPS
data1 <- readxl::read_xlsx("excel/ARR.xlsx", sheet = 2)
range_data1 = paste0("C1:S", nrow(data1))
# Select nrow, col
data1 <- readxl::read_xlsx("excel/ARR.xlsx", sheet = 2, range = "C1:S135")

data <- FormatRegisterID(data, column = "ID")
data <- FormatName(data, column = "ID")
data1 <- FormatRegisterID(data1, column = "登记号")
data1$`入院时间` <- as.character(data1$`入院时间`)

data1 <- data1 %>%
  select(-c("登记号"))

data2 <- full_join(data, data1, by = c("姓名")) %>%
  select("ID", "入院时间", "登记号", "姓名", "性别", "年龄", "身高", "体重", "BMI", "BSA", everything())

write.xlsx(data2, "./excel/数据内容_ARR.xlsx", sheet = 1)