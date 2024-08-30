library(mlr3verse)
library(tidyverse)
library(openxlsx)
library(here)
library(data.table)
library(mlr3)
library(mlr3pipelines)
library(dlookr)
library(paradox)
library(mlr3tuning)
library(DALEX)
library(mlr3learners)
library(mlr3extralearners)

# Function to preprocess data
preprocess_data <- function(df) {
  diagnose(df)
  df$label <- factor(df$label)
  df$Menopausal_status <- factor(df$Menopausal_status)
  df$Menopausal_status <- relevel(df$Menopausal_status, ref = "Premenopause")
  df$O_RADS <- factor(df$O_RADS)
  df$O_RADS <- relevel(df$O_RADS, ref = "2")
  return(df)
}

# Load and preprocess training data
df_train4 <- read.xlsx(here("input", "df_train4.xlsx"))
df_train4 <- preprocess_data(df_train4)

# Load and preprocess internal validation data
df_test4 <- read.xlsx(here("input", "df_test4.xlsx"))
df_test4 <- preprocess_data(df_test4)

# Load and preprocess external validation data
df_wbyz4 <- read.xlsx(here("input", "df_wbyz4.xlsx"))
df_wbyz4 <- preprocess_data(df_wbyz4)

# Create classification tasks
task_train4 <- as_task_classif(x = df_train4, target = "label")
task_train4$positive = "1"

task_internal4 <- as_task_classif(x = df_test4, target = "label")
task_internal4$positive = "1"

task_external4 <- as_task_classif(x = df_wbyz4, target = "label")
task_external4$positive = "1"

# Define preprocessing pipeline steps
po_encode <- po("encode", method = "treatment")
po_impute <- po("imputehist")
po_scale <- po("scale")
po_smote <- po("smote")

# Define the complete preprocessing pipeline
preproc_pipeline <- po_encode %>>% po_impute %>>% po_scale %>>% po_smote

# Define individual learners
learners <- list(
  ranger = lrn("classif.ranger", predict_type = "prob", id = "ranger"),
  cv_glmnet = lrn("classif.cv_glmnet", predict_type = "prob", id = "cv_glmnet"),
  glmboost = lrn("classif.glmboost", predict_type = "prob", id = "glmboost")
)

# Add preprocessing to each learner
graph_learners <- lapply(learners, function(learner) {
  preproc_pipeline %>>% learner
})

# Create the ensemble learner
ensemble <- po("branch", options = names(learners)) %>>%
  gunion(graph_learners) %>>%
  po("unbranch") %>>%
  po("classifavg", innum = 3)

learner <- GraphLearner$new(ensemble)

# Visualize the graph learner
graph <- learner$graph
graph$plot()

# 查看所有参数及其路径
param_table = as.data.table(learner$param_set)
print(param_table)

# Define the parameter search space
ps <- ParamSet$new(list(
  ParamInt$new("ranger.ranger.num.trees", lower = 100, upper = 2000),
  ParamInt$new("ranger.ranger.min.node.size", lower = 1, upper = 100),
  ParamDbl$new("ranger.ranger.sample.fraction", lower = 0.1, upper = 1),
  ParamInt$new("glmboost.glmboost.mstop", lower = 50, upper = 300),
  ParamInt$new("ranger.ranger.mtry", lower = 1, upper = 6),
  ParamDbl$new("cv_glmnet.cv_glmnet.alpha", lower = 0, upper = 1)
))

# Function to evaluate model
evaluate_model <- function(seed, learner, task_train, task_internal, task_external, ps) {
  set.seed(seed)
  
  # Perform hyperparameter tuning
  instance <- tune(
    task = task_train,
    learner = learner,
    resampling = rsmp("cv", folds = 5),
    measure = msr("classif.auc"),
    search_space = ps,
    terminator = trm("evals", n_evals = 100),
    tuner = tnr("random_search")
  )
  
  best_params <- instance$result_learner_param_vals
  
  # Train the model with best parameters
  learner$param_set$values <- best_params
  learner$train(task_train)  
  
  # Evaluate on training set
  pred_train <- learner$predict(task_train)
  auc_train <- msr("classif.auc")$score(pred_train)
  
  # Evaluate on internal validation set
  pred_internal <- learner$predict(task_internal)
  auc_internal <- msr("classif.auc")$score(pred_internal)
  
  # Evaluate on external validation set
  pred_external <- learner$predict(task_external)
  auc_external <- msr("classif.auc")$score(pred_external)
  
  return(list(params = best_params, results = data.table(seed = seed, auc_train = auc_train, auc_internal = auc_internal, auc_external = auc_external)))
}

# Define seeds for model evaluation
seeds <- c(123, 456, 789, 101112, 131415)

# Evaluate model for each seed and save results
all_results <- lapply(seeds, function(seed) {
  evaluate_model(seed, learner, task_train4, task_internal4, task_external4, ps)
})

# Extract AUC results and best parameters
results <- rbindlist(lapply(all_results, function(x) x$results))
best_params_list <- lapply(all_results, function(x) x$params)

# Print AUC results
print(results)

# Calculate average AUC
average_auc <- results[, .(auc_train_mean = mean(auc_train), auc_internal_mean = mean(auc_internal), auc_external_mean = mean(auc_external))]
print(average_auc)

# Print best parameters for each seed
for (i in seq_along(seeds)) {
  cat("\nBest parameters for seed", seeds[i], ":\n")
  print(best_params_list[[i]])
}

# 定义获取预测概率的函数
get_prediction_probabilities <- function(learner, task) {
  # 进行预测
  prediction <- learner$predict(task)
  # 提取预测概率
  prediction_prob <- as.data.table(prediction$prob)
  return(prediction_prob)
}

# 获取训练集的预测概率
train_prediction_prob <- get_prediction_probabilities(learner, task_train4)
print(train_prediction_prob)

# 获取内部验证集的预测概率
internal_prediction_prob <- get_prediction_probabilities(learner, task_internal4)
print(internal_prediction_prob)

# 获取外部验证集的预测概率
external_prediction_prob <- get_prediction_probabilities(learner, task_external4)
print(external_prediction_prob)
write.xlsx(train_prediction_prob,"train_prediction_prob4.xlsx")
write.xlsx(internal_prediction_prob,"internal_prediction_prob4.xlsx")
write.xlsx(external_prediction_prob,"external_prediction_prob4.xlsx")

###########模型解释###################

#变量重要性
library(mlr3)
library(mlr3learners)
library(mlr3extralearners)
library(dplyr)
library(DALEX)
library(DALEXtra)

# 将目标变量从因子转换为数值型
task_data4 <- task_train4$data()
task_data4$label <- as.numeric(as.character(task_data4$label))

# 创建 DALEX 解释器
explainer <- explain_mlr3(learner, data = task_train4$data(), y = as.numeric(as.character(task_train4$data()$label)), label = "Ensemble Model")

# 计算整个数据集的SHAP值
shap_values <- predict_parts(explainer, new_observation = task_train4$data(), type = "shap", B = 10)

# 确认shap_values的数据结构
str(shap_values)

# 将SHAP值转换为长格式，并去除label特征
shap_long <- shap_values %>%
  as.data.frame() %>%
  filter(variable_name != "label") %>%
  mutate(variable_value = as.numeric(as.character(variable_value)))  # 确保variable_value转换为数值型

# 打印前几行数据检查
head(shap_long)

# 创建SHAP summary plot
ggplot(shap_long, aes(x = contribution, y = reorder(variable_name, abs(contribution)), color = variable_value)) +
  geom_point(alpha = 0.6, size = 3.5) +
  scale_color_viridis_c() +
  theme_minimal() +
  labs(title = "SHAP Summary Plot",
       x = "SHAP value",
       y = "Feature",
       color = "Feature value")

# 计算变量重要性
vip <- model_parts(explainer, loss_function = loss_one_minus_auc, variables = setdiff(names(task_train4$data()), "label"))

# 移除_full_model_和_baseline_
vip <- vip[!vip$variable %in% c("_full_model_", "_baseline_"), ]

# 确保 dropout_loss 在 0 到 1 的范围内
vip$dropout_loss <- pmin(vip$dropout_loss, 1)

# 将变量按重要性从高到低排序
vip$variable <- reorder(vip$variable, vip$dropout_loss)

# 绘图
ggplot(vip, aes(x = dropout_loss, y = variable)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Permutation-based Variable Importance",
       subtitle = "Ensemble Model",
       x = "Loss function: 1-AUC",
       y = "Feature")

print(vip)

#SHAP解释模型--全部解释1
library(kernelshap)
library(shapviz)
# Step to compute and visualize SHAP values
# Ensure your data is in data.table format and select a subset for explanation
X_explain <- as.data.table(df_train4[sample(nrow(df_train4), 413), ])

# Compute SHAP values using the kernelshap package
ps <- permshap(
  object = learner, 
  X = X_explain, 
  bg_X = X_explain[1:200, ],  # Background data for SHAP calculations
  feature_names = colnames(X_explain)
)

# Convert the results to a shapviz object, choosing the second class for binary classification
sv <- shapviz(ps)[[2]]

# Print shapviz object for verification
print(sv)

# Generate SHAP importance plot using a bee plot

sv_importance(sv, "bee")

# Additional optional visualizations (commented out)
# sv_dependence(sv, "specific_feature_name") # For feature dependence plot
sv_waterfall(sv)                           # For waterfall plot

#全局解释2
# Step to compute and visualize SHAP values
# Ensure your data is in data.table format and select a subset for explanation
X_explain <- as.data.table(df_wbyz4[sample(nrow(df_wbyz4), 312), ])  # Reduce sample size to speed up SHAP calculations

# Remove the label column from the features to be explained
X_explain <- X_explain[, !c("label"), with = FALSE]

# Compute SHAP values using the kernelshap package
ps <- permshap(
  object = learner, 
  X = X_explain, 
  bg_X = X_explain[1:200, ],  # Reduce background data size to speed up SHAP calculations
  feature_names = colnames(X_explain)
)

# Convert the results to a shapviz object, choosing the second class for binary classification
sv <- shapviz(ps)[[2]]

# Print shapviz object for verification
print(sv)

# Generate SHAP importance plot using a bee plot
sv_importance(sv, "bee")

# Additional optional visualizations (commented out)
# sv_dependence(sv, "specific_feature_name") # For feature dependence plot
sv_waterfall(sv)                           # For waterfall plot

#实例解释1
library(data.table)
library(fastshap)
library(shapviz)
library(mlr3)
library(mlr3pipelines)

# 确保 df_train 和 learner 已经定义，并且模型 learner 已经训练好

# 选择特定的实例进行解释
instance_index <- 112
single_observation <- df_train4[instance_index, ]

# 确保特征和数据一致，移除标签列
df_train_dt <- as.data.table(df_train4)
X_train <- df_train_dt[, !c("label"), with = FALSE]
X_single <- as.data.table(single_observation)[, !c("label"), with = FALSE]

# 创建预测函数
predict_function <- function(model, newdata) {
  pred <- as.data.table(model$predict_newdata(newdata))
  as.numeric(pred$prob.1)
}

# 计算单个实例的SHAP值
shap_values <- fastshap::explain(
  learner,
  X = X_train,
  pred_wrapper = predict_function,
  newdata = X_single,
  nsim = 100,
  shap_only = FALSE
)

# 创建 shapviz 对象，包含特征值
shap_viz <- shapviz(shap_values, X = X_single)

# 生成瀑布图
sv_waterfall(shap_viz)

# 检查实际预测值
actual_pred <- predict_function(learner, X_single)
print(actual_pred)

#实例解释2
library(data.table)
library(fastshap)
library(shapviz)
library(mlr3)
library(mlr3pipelines)

# 确保 df_train, df_external 和 learner 已经定义，并且模型 learner 已经训练好

# 加载和预处理外部验证数据
df_external_dt <- as.data.table(df_wbyz4)
X_external <- df_external_dt[, !c("label"), with = FALSE]

# 选择外部验证数据中的特定实例进行解释
instance_index_external <- 2  # 示例：选择第1个样本
single_observation_external <- df_wbyz4[instance_index_external, ]

# 确保特征和数据一致，移除标签列
X_single_external <- as.data.table(single_observation_external)[, !c("label"), with = FALSE]

# 创建预测函数
predict_function <- function(model, newdata) {
  pred <- as.data.table(model$predict_newdata(newdata))
  as.numeric(pred$prob.1)
}

# 使用背景数据计算基线值
bg_X_external <- X_external[1:50, ]  # 使用外部验证数据的前50个样本作为背景数据
baseline_external <- mean(predict_function(learner, bg_X_external))
print(paste("Baseline value using external background data:", baseline_external))

# 增加模拟次数以提高精度
nsim_value <- 1000

# 计算外部验证数据中单个实例的SHAP值，并使用背景数据
shap_values_external <- fastshap::explain(
  learner,
  X = X_external,
  pred_wrapper = predict_function,
  newdata = X_single_external,
  nsim = nsim_value,
  bg_X = bg_X_external,  # 使用外部验证数据的背景数据进行计算
  shap_only = FALSE
)

# 确认shap_values的数据结构
print(str(shap_values_external))

# 创建 shapviz 对象，包含特征值
shap_viz_external <- shapviz(shap_values_external, X = X_single_external)

# 生成瀑布图
sv_waterfall(shap_viz_external)

# 检查实际预测值
actual_pred_external <- predict_function(learner, X_single_external)
print(paste("Actual prediction:", actual_pred_external))

#实例解释3
library(kernelshap)
library(shapviz)
library(mlr3)
library(mlr3pipelines)

# 确保 df_train 和 learner 已经定义，并且模型 learner 已经训练好

#选择特定的实例进行解释
instance_index <- 1
single_observation <- df_train4[instance_index, ]

# 确保特征和数据一致，移除标签列
df_train_dt <- as.data.table(df_train4)
X_train <- df_train_dt[, !c("label"), with = FALSE]
X_single <- as.data.table(single_observation)[, !c("label"), with = FALSE]

# 创建预测函数
predict_function <- function(model, newdata) {
  pred <- as.data.table(model$predict_newdata(newdata))
  as.numeric(pred$prob.1)
}

# 使用KernelSHAP计算SHAP值
shap_values_ks <- kernelshap(
  object = learner,
  X = X_single,
  bg_X = X_train
)

# 创建 shapviz 对象，包含特征值
shap_viz_ks <- shapviz(shap_values_ks, X = X_single)

# 生成瀑布图
sv_waterfall(shap_viz_ks)

# 检查实际预测值
actual_pred <- predict_function(learner, X_single)
print(paste("Actual prediction:", actual_pred))




library(data.table)
library(mlr3)
library(mlr3pipelines)
library(kernelshap)
library(shapviz)

# 选择特定的实例进行解释
instance_index <- 33
single_observation <- df_train4[instance_index, ]

# 确保特征和数据一致，移除标签列
df_train_dt <- as.data.table(df_train4)
X_train <- df_train_dt[, !c("label"), with = FALSE]
X_single <- as.data.table(single_observation)[, !c("label"), with = FALSE]

# 重新添加标签列用于创建新任务
newdata_with_label <- cbind(X_single, label = single_observation$label)

# 创建预测函数
predict_function <- function(model, newdata) {
  new_task <- TaskClassif$new(id = "new_task", backend = newdata, target = "label")
  pred <- model$predict(new_task)
  as.numeric(pred$prob[, "1"])
}

# 确保X_single的数据格式正确，且不包含目标变量
X_single <- as.data.table(single_observation)[, !c("label"), with = FALSE]

# 打印模型和数据检查
print("Checking learner and data before SHAP calculation")
print(learner)
print(head(X_single))
print(head(X_train))

# 计算 SHAP 值
shap_values_ks <- kernelshap(
  object = learner,
  X = X_single,
  bg_X = X_train
)

# 检查shap_values_ks内容
print("shap_values_ks contents:")
print(shap_values_ks)

# 检查shap_values_ks的结构
print("shap_values_ks structure:")
print(str(shap_values_ks))

# 提取 S 列表中的 SHAP 值并创建矩阵
shap_values_list <- shap_values_ks$S
shap_values_matrix <- do.call(rbind, shap_values_list)
shap_values_matrix <- shap_values_matrix[1, , drop = FALSE] # 只保留第一行
colnames(shap_values_matrix) <- colnames(X_single)

print("SHAP Values Matrix:")
print(shap_values_matrix)

# 计算基线值（背景数据的平均预测值）
baseline_preds <- sapply(1:nrow(X_train), function(i) {
  newdata <- X_train[i, ]
  newdata <- as.data.table(newdata)
  newdata_with_label <- cbind(newdata, label = df_train4$label[i])
  predict_function(learner, newdata_with_label)
})
baseline <- mean(baseline_preds)
print("Baseline Value:")
print(baseline)

# 创建 shapviz 对象，包含特征值
sv <- shapviz(shap_values_matrix, X = X_single)

# 检查 SHAP 值是否为空
if (is.null(sv$shap_values)) {
  stop("SHAP values are NULL. Please check the shapviz object creation.")
}
print("SHAP Values:")
print(sv$shap_values)

# 生成瀑布图
sv_waterfall(sv)

# 计算预测值
shap_sum <- sum(sv$shap_values)
pred_value <- shap_sum + baseline

# 打印计算的预测值和实际预测值
print(paste("Calculated prediction:", pred_value))

actual_pred <- predict_function(learner, newdata_with_label)
print(paste("Actual prediction:", actual_pred))



library(data.table)
library(mlr3)
library(mlr3pipelines)
library(iml)

# 选择特定的实例进行解释
instance_index <- 112
single_observation <- df_train4[instance_index, ]

# 确保特征和数据一致，移除标签列
df_train_dt <- as.data.table(df_train4)
X_train <- df_train_dt[, !c("label"), with = FALSE]
X_single <- as.data.table(single_observation)[, !c("label"), with = FALSE]

# 将X_single转换为数据帧格式
X_single_df <- as.data.frame(X_single)

# 重新添加标签列用于创建新任务
newdata_with_label <- cbind(X_single_df, label = single_observation$label)

# 创建预测函数
predict_function <- function(model, newdata) {
  new_task <- TaskClassif$new(id = "new_task", backend = newdata, target = "label")
  pred <- model$predict(new_task)
  as.numeric(pred$prob[, "1"])
}

# 打印模型和数据检查
print("Checking learner and data before SHAP calculation")
print(learner)
print(head(X_single_df))
print(head(X_train))

# 定义解释器
predictor <- Predictor$new(model = learner, data = as.data.frame(X_train), y = df_train4$label)

# 计算 SHAP 值
shapley <- Shapley$new(predictor, x.interest = X_single_df)

# 打印 SHAP 值
print("SHAP Values:")
print(shapley$results)

# 可视化 SHAP 瀑布图
plot(shapley, type = "waterfall")




library(DALEX)
library(DALEXtra)
library(lime)
library(localModel)
library(iml)

# 假设 learner 是已经训练好的集成模型，df_train3 是训练集
# 使用 DALEX 包中的 explain() 函数为模型构建解释器
explainer <- DALEX::explain(model = learner,  
                            data = df_train4[, -1],  # 去掉目标变量列
                            y = df_train4$label=="1", 
                            label = "Ensemble Model")

# 创建待解释的新观测值
johnny_d <- df_train4[112, ]  # 假设 df_train3 的第一行是 Johnny D 的数据

model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

# 使用 predict_surrogate() 函数调用 lime 方法
set.seed(2024)
lime_johnny <- predict_surrogate(explainer = explainer, 
                                 new_observation = johnny_d, 
                                 n_features = 3, 
                                 n_permutations = 1000,
                                 type = "lime")

# 输出 lime 结果
(as.data.frame(lime_johnny)) %>% 
  DT::datatable()

# 图形表示 lime 结果
plot(lime_johnny)

# 使用 predict_surrogate() 函数调用 localModel 方法
locMod_johnny <- predict_surrogate(explainer = explainer, 
                                   new_observation = johnny_d, 
                                   size = 1000, 
                                   seed = 2024,
                                   type = "localModel")

# 输出 localModel 结果
DT::datatable(locMod_johnny)

# 图形表示 localModel 结果
plot(locMod_johnny)

DT::datatable(df_train4)
plot_interpretable_feature(locMod_johnny, "age")

# 使用 predict_surrogate() 函数调用 iml 方法
iml_johnny <- predict_surrogate(explainer = explainer, 
                                new_observation = johnny_d, 
                                k = 3, 
                                type = "iml")

# 输出 iml 结果
iml_johnny$results[,c(1:5,7)]

# 图形表示 iml 结果
plot(iml_johnny)

# 显示 session 信息
devtools::session_info()
