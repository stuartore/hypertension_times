library(openxlsx)

stu_read_excel <- function(excel_file, sheet_list, identify_by = "住院号"){
  sheet_names <- sheet_list
  data_base <- data.frame()
  for (sheet in sheet_names) {
    # 读取当前 sheet 的数据
    data_new <- openxlsx::read.xlsx(excel_file, sheet = sheet)
    if (ncol(data_base) == 0){
      data_base <- data_new
    }else{
      common_cols <- intersect(names(data_base), names(data_new))
      # 确保不剔除identify_by列
      if (identify_by %in% common_cols) {
        common_cols <- setdiff(common_cols, identify_by)
      }
      if (length(common_cols) > 0) {
        data_new <- data_new[,!(names(data_new) %in% common_cols)]
      }
      data_base <- dplyr::inner_join(data_base, data_new, by = identify_by)
    }
  }
  return(data_base)
}

# 对ID的格式为a+登记号+姓名：提取登记号
FormatRegisterID <- function(data, column="登记号") {
  # 匹配a+数字+姓名：第二段作为登记号
  data$`登记号` <- data[[column]]
  data$`登记号` <- sub("^([a-z]?)(\\d+)(.*)", "\\2", as.character(data$`登记号`))

  # 用数字进行补足
  data$`登记号` <- as.numeric(data$`登记号`)
  data$`登记号` <- sprintf("%010d", data$`登记号`)
  # 转化为字符类型
  data$`登记号` <- as.character(data$`登记号`)
  return(data)
}

# 对ID的格式为a+登记号+姓名：提取姓名
FormatName <- function(data, column="姓名") {
  # 匹配a+数字+姓名：第三段作为姓名
  data$`姓名` <- data[[column]]
  data$`姓名` <- sub("^([a-z]?)(\\d+)(.*)", "\\3", as.character(data$`姓名`))
  return(data)
}

# 将表格的data，中英文括号替换成指定字符
FormatDataColumnsKuohao <- function(data, symbol = "_"){
  # 获取数据框 data 的列名
  colnames_data <- colnames(data)
  # 使用 gsub 函数替换括号为下划线
  new_colnames <- gsub("[（）()]", symbol, colnames_data)
  # 将替换后的列名重新赋值给 data 数据框
  colnames(data) <- new_colnames
  return(data)
}