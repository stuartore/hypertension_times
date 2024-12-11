# 体圆度指数(BRI)计算
BRI <- function(WC_cm, H_cm) {
  # 将腰围和身高从厘米转换为米
  WC_m <- WC_cm / 100
  H_m <- H_cm / 100
  # 计算BRI
  BRI_value <- 364.2 - 365.5 * sqrt(1 - ((WC_m / (2 * pi))^2) / (0.5 * H_m)^2)
  return(BRI_value)
}

# 左心质量指数
LVMI <- function(IVST_mm, PWT_mm, LVDd_mm, BSA) {
  # IVST：舒张末期室间隔厚度（cm）
  # PWT：舒张末期后壁厚度（cm）
  # LVDd：舒张末期左心室内径（cm）
  # BSA体表面积
  IVST_cm <- IVST_mm / 10
  PWT_cm <- PWT_mm / 10
  LVDd_cm <- LVDd_mm / 10
  
  # 计算左心室重量（LVM）
  LVM <- 0.8 * 1.04 * ((IVST_cm + PWT_cm + LVDd_cm)^3 - LVDd_cm^3) + 0.6
  
  # 计算左心室重量指数（LVMI）
  LVMI <- LVM / BSA
  
  return(LVMI)
}

# 体型指数(ABSI)计算
ABSI <- function(waist_cm, height_cm, weight_kg) {
  waist_m <- waist_cm / 100  # 腰围转换为米
  height_m <- height_cm / 100  # 身高转换为米
  BMI <- weight_kg / (height_m^2)
  ABSI_value <- waist_m / (BMI^(2/3) * (height_m^(1/2)))
  return(ABSI_value)
}

# 性别和年龄特异性正常值和风险关联，将 BMI 和 ABSI 的原始值转换为 Z 分数和 ARI 值
# 六项人体测量指标——腰围 （WC）、体重指数 （BMI）、腰高比 （WHtR）、体型指数 （ABSI）、体圆指数 （BRI） 和腰高比0.5比值 （WHT.5R）

# ASCVD 风险低于 7.5% 的参与者与风险大于或等于 7.5% 的参与者之间的基线特征存在显著差异，
# 按性别分层。在男性和女性组中，ASCVD 风险较高的个体表现出更高的年龄、腰围、BMI 和更高的
# 健康损害行为的发生率。ABSI 成为 ASCVD 风险最准确的预测指标，在两性中均具有最高的曲线下面积 （AUC） 值。
# 建立 ABSI 的最佳临界值以进行有效的风险分层 （临界值 = 0.08）。

# 锥度指数(C-index)计算
C_Index <- function(waist_cm, weight_kg, height_cm) {
  # 将腰围和身高从厘米转换为米
  waist_m <- waist_cm / 100
  height_m <- height_cm / 100
  
  # 计算C-index
  c_index_value <- waist_m / (0.109 * sqrt(weight_kg / height_m))
  return(c_index_value)
}

# 葡萄糖处置率eGDR
eGDR <- function(waist_cm, hpts, hbalc) {
  # 校正hpts: Have - 1, Not have - 0
  hpts <- ifelse(as.numeric(hpts) == 1, 1,
                 ifelse(as.numeric(hpts) == 2, 0, NA))
  
  # 计算eGDR
  eGDR <- 21.158 - (0.09 * waist_cm) - (3.407 * hpts) - (0.551 * hbalc)
  
  return(eGDR)
}

# HGI计算
HGI <- function(bl_glu_mg_dl, bl_hbalc) {
  # 将mg/dL转换为mmol/L
  bl_glu_mmol_l <- bl_glu_mg_dl / 18
  HbA1c_predicted <- 0.013 * bl_glu_mmol_l + 4.804
  HGI <- bl_hbalc - HbA1c_predicted
  return(HGI)
}

# CALLY index 函数
CALLY_index <- function(Albumin, Lymphocyte, CRP) {
  result <- Albumin * Lymphocyte / (CRP * 10)
  return(result)
}

# NLR 函数
NLR <- function(Neutrocyte, Lymphocyte) {
  result <- Neutrocyte / Lymphocyte
  return(result)
}

# PLR 函数
PLR <- function(Platelet, Lymphocyte) {
  result <- Platelet / Lymphocyte
  return(result)
}

# SII 函数
SII <- function(Neutrocyte, Platelet, Lymphocyte) {
  result <- Neutrocyte * Platelet / Lymphocyte
  return(result)
}

# 计算心脏代谢指数CMI ，发过frontiers
CMI <- function(bl_tg, bl_hdl, WC_cm, height_cm) {
  # 计算腰高比 WHtR
  WHtR <- WC_cm / height_cm
  
  # 计算 CMI
  CMI_value <- (bl_tg / bl_hdl) * WHtR
  
  return(CMI_value)
}


# eGFR估算，公式参考https://pubmed.ncbi.nlm.nih.gov/36720134/ IF = 96.2
# EKFC eGFRcys 方程无偏见，在白人患者和黑人患者（欧洲 11231 名患者、美国 1093 名患者和非洲 508 名患者）中的准确性与 EKFC eGFRcr 方程相似，
# 并且比《肾脏病》杂志推荐的慢性肾脏病流行病学协作组织 eGFRcys 方程更准确：改善全球结果》推荐的 eGFRcys 方程更准确。
# * 基于胱抑素 C 的方程 （EKFC eGFRcys） 在估计测得的 GFR 方面并不比基于血清肌酐的方程 （EKFC eGFRcr） 更准确。这些结果与以前的研究一致。仅在组合 EKFC eGFRcr-cys 方程中观察到 GFR 估计的改善
# EKFC eGFRcr 和 EKFC eGFRcys 的算术平均值进一步提高了估算 GFR 的准确性，超过了单独使用其中一种生物标记方程估算的准确性
# EKFC eGFRcys 方程的数学形式与 EKFC eGFRcr 方程相同，但其胱抑素 C 的比例因子并不因种族或性别而异。
# 欧洲肾功能联盟 （EKFC） 基于肌酐的计算式
EKFC_eGFRcr_cys <- function(scr_mg_dl, cystatinc, age, gender){
  # EKFC eGFRcr 和 EKFC eGFRcys (无性别) 的平均值EKFC eGFRcr-cys | EKFC方程在整个标准化SCr范围内也显示出几乎为零的偏倚
  # eGFRcys [胱抑素C]或肌酐-胱抑素C [eGFRcr-cys]）
  # 新的eGFRcr（AS）公式可能比新的eGFRcr（ASR-NB）公式更公平。因为它平均了所有人之间观察到的差异，可能更适合日益多样化的美国人口。与ASR-NB方程相比，AS方程减少了黑人的偏见，但也引入了非黑人的偏见。新的eGFRcr-cys（ASR-NB）和eGFRcr-cys（AS）方程在黑人参与者中的偏倚较小
  # 新的eGFRcr-cys（ASR-NB）（包含人种变量）和eGFRcr-cys（AS）方程在黑人参与者中的偏倚较小
  # 黑人受试者的eGFRcr在当前ASR方程中最高，在新ASR-NB方程中最低，在新AS方程中居中。对于非黑人受试者，ASR和ASR-NB方程的eGFRcr相等，AS方程的eGFRcr更高（图S3和表S10）

  # EKFC eGFRcr-cys 是 EKFC eGFRcr 和 EKFC eGFRcys 的算术平均值
  EKFC_eGFRcr_cys_value <- (EKFC_eGFRcr(scr_mg_dl, age, gender) + EKFC_eGFRcys(cystatinc, age)) / 2
  # CKD-EPI eGFRcys
  return(EKFC_eGFRcr_cys_value)
}

# 参考新英格兰杂志https://pubmed.ncbi.nlm.nih.gov/36720134/ IF = 96.2
# https://pubmed.ncbi.nlm.nih.gov/33166224/ IF = 19
# DOI: 10.7326/M20-4366
EKFC_eGFRcr <- function(scr_mg_dl, age, gender) {
  # scr_mg_dl = scr_umol_l / 88.4

  # Gender: 
  # 1 Male
  # 2 Female
  age <- as.numeric(age)
  gender <- as.numeric(gender)

  # 计算Q值
  Q <- ifelse(age <= 25 & gender == 1,
    exp(3.200 + 0.259 * age - 0.543 * log(age) - 0.00763 * age^2 + 0.0000790 * age^3),
    ifelse(age <= 25 & gender == 2,
          exp(3.080 + 0.177 * age - 0.223 * log(age) - 0.00596 * age^2 + 0.0000686 * age^3),
          ifelse(gender == 1,
                  0.9,  # 80 μmol/L (0.90 mg/dL)
                  0.7))) # 62 μmol/L (0.70 mg/dL)
  
  
  # 计算eGFRcr
  EKFC_eGFRc_value <- ifelse(age > 40,
    107.3 * (scr_mg_dl / Q)^(-0.322) * 0.990^(age - 40),
    107.3 * (scr_mg_dl / Q)^(-0.322))
  
  return(EKFC_eGFRc_value)
}

# DOI: 10.1056/NEJMoa2203769
# EKFC eGFRcys 方程具有与 EKFC eGFRcr 方程相同的数学形式，但它具有胱抑素 C 的比例因子，该因子不因种族或性别而异。在来自欧洲、美国和非洲的队列中，该方程式比常用方程式提高了 GFR 评估的准确性
EKFC_eGFRcys <- function(cystatin_mg_dl, age, gender = NULL) {
  age <- as.numeric(age)
  
  # 定义重缩放因子Q
  Q <- ifelse(age < 50, 0.83, 0.83 + 0.005 * (age - 50))
  
  # 计算biomarker/Q
  biomarker_over_Q <- cystatin_mg_dl / Q
  
  # 定义α值
  alpha <- ifelse(biomarker_over_Q < 1, 0.322, 1.132)
  
  # 计算EKFC eGFRcys
  EKFC_eGFRcys <- ifelse(age > 40, 
                         107.3 / (biomarker_over_Q ^ alpha) * (0.990 * (age - 40)),
                         107.3 / (biomarker_over_Q ^ alpha))
  
  return(EKFC_eGFRcys)
}
# eGFR : CKD-EPI公式（包括黑人）

# PHR指数
PHR <- function(bl_plt, bl_hdl) {
  # 计算PHR
  PHR <- bl_plt / bl_hdl
  return(PHR)
}

# 胰岛素抵抗代谢评分（Metabolic Score for Insulin Resistance，METS-IR）
METS_IR <- function(bl_glu_mg_dl, bl_tg, weight_kg, height_cm, bl_hdl) {
  # 将身高从厘米转换为米
  height_m <- height_cm / 100

  # 计算BMI
  bmi <- weight_kg / (height_m^2)
  
  # 计算公式中的对数部分
  log_FBG_plus_TG <- log(2 * bl_glu_mg_dl + bl_tg) * bmi

  # 计算HDL的对数部分
  log_HDL <- log(bl_hdl)
  
  # 计算METS-IR
  METS_IR_value <- log_FBG_plus_TG / log_HDL
  
  # 返回计算结果
  return(METS_IR_value)
}

# 血浆动脉粥样硬化指数（AIP）
AIP <- function(bl_tg, bl_hdl) {
  # 计算TG与HDL-C的比值
  ratio <- bl_tg / bl_hdl
  
  # 计算比值的对数
  AIP <- log(ratio)
  
  return(AIP)
}

# UHR指数
UHR <- function(bl_ua, bl_hdl){
  UHR_value <- bl_ua / bl_hdl
  return(UHR_value)
}

## CHARLS 2015 数据
# 在Biomarker.dta
# qm002 - 腰围
# ql002 - 体重
# qi002 - 身高

# 在Blood.dta
# bl_hbalc - 糖化血红蛋白
