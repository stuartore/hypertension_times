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