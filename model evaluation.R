library(readxl)
library(pROC)
library(dplyr)
library(here)

# 逐一读取文件以调试可能的问题
model1_train <- read.xlsx(here("input", "model1_train.xlsx"))
model1_internal <- read.xlsx(here("input", "model1_internal.xlsx"))
model1_external <- read.xlsx(here("input", "model1_external.xlsx"))
model2_train <- read.xlsx(here("input", "model2_train.xlsx"))
model2_internal <- read.xlsx(here("input", "model2_internal.xlsx"))
model2_external <- read.xlsx(here("input", "model2_external.xlsx"))
model3_train <- read.xlsx(here("input", "model4_train.xlsx"))
model3_internal <- read.xlsx(here("input", "model4_internal.xlsx"))
model3_external <- read.xlsx(here("input", "model4_external.xlsx"))

# 创建数据列表
data_list <- list(
  model1_train = model1_train,
  model1_internal = model1_internal,
  model1_external = model1_external,
  model2_train = model2_train,
  model2_internal = model2_internal,
  model2_external = model2_external,
  model3_train = model3_train,
  model3_internal = model3_internal,
  model3_external = model3_external
)

# 打印数据列表
print(data_list)

# 确保数据是数值类型
convert_to_numeric <- function(df) {
  df[] <- lapply(df, as.numeric)
  return(df)
}

data_list <- lapply(data_list, convert_to_numeric)

# 计算最佳阈值函数
calculate_optimal_threshold <- function(df) {
  true_labels <- df[, 1]
  predicted_probs <- df[, 2]
  roc_obj <- roc(true_labels, predicted_probs, quiet = TRUE)
  optimal <- as.numeric(coords(roc_obj, "best", ret = "threshold", best.method = "youden"))
  return(optimal)
}

# 计算9组预测概率的最佳阈值
optimal_thresholds <- sapply(data_list, calculate_optimal_threshold)

# 打印最佳阈值
print(optimal_thresholds)

# 计算评价指标函数
calculate_metrics_with_threshold <- function(df, threshold) {
  true_labels <- df[, 1]
  pred_probs <- df[, 2]
  pred_labels <- ifelse(pred_probs >= threshold, 1, 0)
  roc_obj <- roc(true_labels, pred_probs, quiet = TRUE)
  auc <- auc(roc_obj)
  confusion_matrix <- table(True = true_labels, Predicted = pred_labels)
  sensitivity <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1]) # TP / (TP + FN)
  specificity <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]) # TN / (TN + FP)
  return(list(auc = auc, sensitivity = sensitivity, specificity = specificity))
}

# 计算每个最佳阈值下的指标
metrics_list <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  threshold <- optimal_thresholds[name]
  metrics <- calculate_metrics_with_threshold(df, threshold)
  return(metrics)
})

# 打印每组数据的指标
print(metrics_list)

library(tibble)

# 计算评价指标函数
calculate_metrics_with_threshold <- function(df, threshold) {
  true_labels <- df[, 1]
  pred_probs <- df[, 2]
  pred_labels <- ifelse(pred_probs >= threshold, 1, 0)
  roc_obj <- roc(true_labels, pred_probs, quiet = TRUE)
  auc_value <- auc(roc_obj)
  confusion_matrix <- table(True = true_labels, Predicted = pred_labels)
  sensitivity_value <- confusion_matrix[2, 2] / (confusion_matrix[2, 2] + confusion_matrix[2, 1]) # TP / (TP + FN)
  specificity_value <- confusion_matrix[1, 1] / (confusion_matrix[1, 1] + confusion_matrix[1, 2]) # TN / (TN + FP)
  return(list(auc = auc_value, sensitivity = sensitivity_value, specificity = specificity_value))
}

# 计算每个最佳阈值下的指标
metrics_list <- lapply(names(data_list), function(name) {
  df <- data_list[[name]]
  threshold <- optimal_thresholds[name]
  metrics <- calculate_metrics_with_threshold(df, threshold)
  metrics$name <- name  # 添加名字字段
  return(metrics)
})

# 将metrics_list转换为data frame
metrics_df <- do.call(rbind, lapply(metrics_list, function(metrics) {
  data.frame(
    Model = sub("_(train|internal|external)", "", metrics$name),
    Dataset = sub(".*_", "", metrics$name),
    AUC = as.numeric(metrics$auc),
    Sensitivity = as.numeric(metrics$sensitivity),
    Specificity = as.numeric(metrics$specificity)
  )
}))

# 查看数据框结构
print("Structure of metrics_df:")
print(metrics_df)

# Pivot longer for easier plotting
metrics_long <- metrics_df %>%
  pivot_longer(cols = c(AUC, Sensitivity, Specificity), names_to = "Metric", values_to = "Value")

# 查看转换后的数据框结构
print("Structure of metrics_long:")
print(metrics_long)

# Line plot for model evaluation metrics
ggplot(metrics_long, aes(x = Model, y = Value, color = Metric, group = Metric)) +
  geom_line(linewidth = 1) +  # 使用linewidth代替size
  geom_point(size = 3) +
  facet_wrap(~ Dataset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Evaluation Metrics", y = "Value", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 条形图
ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Dataset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Evaluation Metrics", y = "Value", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.xlsx(metrics_long,"metrics_long.xlsx")
metrics_long <- read.xlsx(here("input", "metrics_long_wbyz.xlsx"))

library(ggplot2)
library(dplyr)

# 假设 metrics_long 是你的数据框

# 调整因子水平
metrics_long$Dataset <- factor(metrics_long$Dataset, levels = c("Train set", "Internal set", "External set"))
metrics_long$Model <- factor(metrics_long$Model, levels = c("Clinical", "+O_RADS", "+DL_Radiomics"))

# 绘制条形图
ggplot(metrics_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Dataset, scales = "free_y") +
  theme_minimal() +
  labs(title = "Model Evaluation Metrics", y = "Value", x = "Model") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write.xlsx(metrics_df,"metrics_df.xlsx")


# 计算AUC及其置信区间的函数
calculate_auc_and_ci <- function(df) {
  true_labels <- df$true_labels
  predicted_probs <- df$predicted_probs
  roc_obj <- roc(true_labels, predicted_probs)
  auc_value <- auc(roc_obj)
  ci <- ci.auc(roc_obj)
  return(list(auc = auc_value, ci = ci))
}

# 计算训练集、内部验证集和外部验证集的AUC及置信区间
auc_ci_results <- lapply(data_list, calculate_auc_and_ci)

# 打印结果
print(auc_ci_results)

# 绘制ROC曲线
plot_roc_curve <- function(df1, df2, df3, dataset_type) {
  true_labels1 <- df1$true_labels
  predicted_probs1 <- df1$predicted_probs
  true_labels2 <- df2$true_labels
  predicted_probs2 <- df2$predicted_probs
  true_labels3 <- df3$true_labels
  predicted_probs3 <- df3$predicted_probs
  
  roc1 <- roc(true_labels1, predicted_probs1)
  roc2 <- roc(true_labels2, predicted_probs2)
  roc3 <- roc(true_labels3, predicted_probs3)
  
  plot(roc1, col = "#56B4E9", lwd = 2, smooth = TRUE, main = paste("ROC Curve -", dataset_type))
  lines(roc2, col = "#E69F00", lwd = 2, smooth = TRUE)
  lines(roc3, col = "#009E73", lwd = 2, smooth = TRUE)
  legend("bottomright", legend = c("Clinical", "Clinical+O-RADS", "Clinical+O-RADS+DL-Radiomics"), col = c("#56B4E9", "#E69F00", "#009E73"), lwd = 2)
}

# 绘制训练集、内部验证集和外部验证集的ROC曲线
plot_roc_curve(model1_train, model2_train, model3_train, "Training Set")
plot_roc_curve(model1_internal, model2_internal, model3_internal, "Internal Validation Set")
plot_roc_curve(model1_external, model2_external, model3_external, "External Validation Set")

# 加载必要的包
library(Hmisc)
library(boot)

# 计算IDI和NRI的函数
calculate_idi_nri <- function(df1, df2) {
  true_labels <- df1$true_labels
  probs1 <- df1$predicted_probs
  probs2 <- df2$predicted_probs
  
  improveProb_results <- improveProb(x1 = probs1, x2 = probs2, y = true_labels)
  return(improveProb_results)
}

# 计算IDI和NRI并输出结果
idi_nri_results <- list(
  train_model2_vs_model1 = calculate_idi_nri(data_list$model1_train, data_list$model2_train),
  train_model3_vs_model2 = calculate_idi_nri(data_list$model2_train, data_list$model3_train),
  internal_model2_vs_model1 = calculate_idi_nri(data_list$model1_internal, data_list$model2_internal),
  internal_model3_vs_model2 = calculate_idi_nri(data_list$model2_internal, data_list$model3_internal),
  external_model2_vs_model1 = calculate_idi_nri(data_list$model1_external, data_list$model2_external),
  external_model3_vs_model2 = calculate_idi_nri(data_list$model2_external, data_list$model3_external)
)

# 打印IDI和NRI结果
print(idi_nri_results)


# 加载必要的包
library(dcurves)

# 准备训练集数据
dca_data_train <- data.frame(
  truth = model1_train$true_labels,
  Clinical = model1_train$predicted_probs,
  Clinical_ORADS = model2_train$predicted_probs,
  Clinical_ORADS_DLRadiomics = model3_train$predicted_probs
)
# 准备内部验证集数据
dca_data_internal_val <- data.frame(
  truth = model1_internal$true_labels,
  Clinical = model1_internal$predicted_probs,
  Clinical_ORADS = model2_internal$predicted_probs,
  Clinical_ORADS_DLRadiomics = model3_internal$predicted_probs
)
# 准备外部验证集数据
dca_data_external_val <- data.frame(
  truth = model1_external$true_labels,
  Clinical = model1_external$predicted_probs,
  Clinical_ORADS = model2_external$predicted_probs,
  Clinical_ORADS_DLRadiomics = model3_external$predicted_probs
)

# 检查数据框的前几行
print(head(dca_data_train))
print(head(dca_data_internal_val))
print(head(dca_data_external_val))

# 计算训练集的DCA曲线
dca_train <- dca(truth ~ Clinical + Clinical_ORADS + Clinical_ORADS_DLRadiomics, data = dca_data_train, thresholds = seq(0, 0.5, by = 0.01))

# 绘制训练集的DCA曲线
plot(dca_train, main = "DCA Curve - Training Set", col = c("blue", "red", "green"), lwd = 3, smooth = TRUE)
legend("topright", legend = c("Clinical", "Clinical+O-RADS", "Clinical+O-RADS+DL-Radiomics"), col = c("blue", "red", "green"), lwd = 3)

# 计算内部验证集的DCA曲线
dca_internal_val <- dca(truth ~ Clinical + Clinical_ORADS + Clinical_ORADS_DLRadiomics, data = dca_data_internal_val, thresholds = seq(0, 0.5, by = 0.01))

# 绘制内部验证集的DCA曲线
plot(dca_internal_val, main = "DCA Curve - Internal Validation Set", col = c("blue", "red", "green"), lwd = 3, smooth = TRUE)
legend("topright", legend = c("Clinical", "Clinical+O-RADS", "Clinical+O-RADS+DL-Radiomics"), col = c("blue", "red", "green"), lwd = 3)

# 计算外部验证集的DCA曲线
dca_external_val <- dca(truth ~ Clinical + Clinical_ORADS + Clinical_ORADS_DLRadiomics, data = dca_data_external_val, thresholds = seq(0, 0.5, by = 0.01))

# 绘制外部验证集的DCA曲线
plot(dca_external_val, main = "DCA Curve - External Validation Set", col = c("blue", "red", "green"), lwd = 3, smooth = TRUE)
legend("topright", legend = c("Clinical", "Clinical+O-RADS", "Clinical+O-RADS+DL-Radiomics"), col = c("blue", "red", "green"), lwd = 3)


# 加载必要的包
library(boot)

# 定义计算NRI和IDI的函数
calculate_nri_idi <- function(data, indices, model1_probs_col, model2_probs_col) {
  d <- data[indices, ]
  
  # 预测概率
  p1 <- d[[model1_probs_col]]
  p2 <- d[[model2_probs_col]]
  
  # 事件和非事件数量
  events <- d$truth == 1
  non_events <- d$truth == 0
  
  # 计算NRI
  nri_events <- sum((p2 > p1)[events]) / sum(events) - sum((p1 > p2)[events]) / sum(events)
  nri_nonevents <- sum((p1 > p2)[non_events]) / sum(non_events) - sum((p2 > p1)[non_events]) / sum(non_events)
  nri <- nri_events + nri_nonevents
  
  # 计算IDI
  idi <- mean(p2[events]) - mean(p1[events]) + mean(p1[non_events]) - mean(p2[non_events])
  
  return(c(nri, idi))
}

# 函数：计算数据集的NRI和IDI置信区间
calculate_nri_idi_ci <- function(data, model1_probs_col, model2_probs_col) {
  results <- boot(data, function(data, indices) calculate_nri_idi(data, indices, model1_probs_col, model2_probs_col), R = 1000)
  nri_ci <- boot.ci(results, type = "perc", index = 1)
  idi_ci <- boot.ci(results, type = "perc", index = 2)
  nri_p_value <- sum(results$t[,1] < 0) / length(results$t[,1])
  idi_p_value <- sum(results$t[,2] < 0) / length(results$t[,2])
  list(nri_ci = nri_ci, idi_ci = idi_ci, nri_p_value = nri_p_value, idi_p_value = idi_p_value)
}

# 准备数据集
datasets <- list(
  train = list(
    model1 = model1_train,
    model2 = model2_train,
    model3 = model3_train
  ),
  internal = list(
    model1 = model1_internal,
    model2 = model2_internal,
    model3 = model3_internal
  ),
  external = list(
    model1 = model1_external,
    model2 = model2_external,
    model3 = model3_external
  )
)

# 计算并打印结果
for (dataset_name in names(datasets)) {
  data <- datasets[[dataset_name]]
  combined_data <- data.frame(
    truth = data$model1$true_labels,
    model1_probs = data$model1$predicted_probs,
    model2_probs = data$model2$predicted_probs,
    model3_probs = data$model3$predicted_probs
  )
  
  cat("\nResults for", dataset_name, "dataset:\n")
  
  # Model2 vs Model1
  results_model2_vs_model1 <- calculate_nri_idi_ci(combined_data, "model1_probs", "model2_probs")
  cat("Model2 vs Model1:\n")
  print(results_model2_vs_model1)
  
  # Model3 vs Model1
  results_model3_vs_model1 <- calculate_nri_idi_ci(combined_data, "model1_probs", "model3_probs")
  cat("Model3 vs Model1:\n")
  print(results_model3_vs_model1)
  
  # Model3 vs Model2
  results_model3_vs_model2 <- calculate_nri_idi_ci(combined_data, "model2_probs", "model3_probs")
  cat("Model3 vs Model2:\n")
  print(results_model3_vs_model2)
}


# 加载必要的包
library(officer)
library(flextable)

# 创建数据框
results_df <- data.frame(
  Dataset = c(rep("Train", 3), rep("Internal", 3), rep("External", 3)),
  Comparison = rep(c("Model2 vs Model1", "Model3 vs Model1", "Model3 vs Model2"), 3),
  NRI_CI = c("-0.2184 (-0.5012, 0.0598)", "1.1135 (0.901, 1.326)", "1.3725 (1.186, 1.559)", 
             "-0.1418 (-0.5486, 0.2650)", "0.9261 (0.5491, 1.3031)", "1.255 (0.924, 1.586)", 
             "-0.7269 (-0.9692, -0.4646)", "0.8365 (0.5863, 1.0866)", "1.273 (1.055, 1.491)"),
  NRI_p_value = c(0.936, 0, 0, 0.725, 0, 0, 1, 0, 0),
  IDI_CI = c("0.0253 (-0.0231, 0.0756)", "0.1958 (0.1409, 0.2507)", "0.1678 (0.1409, 0.1947)", 
             "0.0056 (-0.0642, 0.0754)", "0.1737 (0.0907, 0.2568)", "0.1683 (0.1110, 0.2256)", 
             "-0.0337 (-0.0798, 0.0153)", "0.0995 (0.0353, 0.1636)", "0.1284 (0.0887, 0.1681)"),
  IDI_p_value = c(0.16, 0, 0, 0.423, 0, 0, 0.909, 0.001, 0)
)

# 创建Word文档
doc <- read_docx()

# 添加标题
doc <- doc %>%
  body_add_par("Comparison of Models", style = "heading 1")

# 创建表格
ft <- flextable(results_df)
ft <- autofit(ft)

# 添加表格到文档
doc <- body_add_flextable(doc, value = ft)

# 保存文档
print(doc, target = "model_comparison_results.docx")
