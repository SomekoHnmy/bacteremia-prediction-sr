# 必要なパッケージをインストール
install.packages(c(
  "metafor",      # メタアナリシスのメインパッケージ
  "readxl",       # Excelファイル読み込み
  "dplyr",        # データ操作
  "ggplot2",      # 可視化
  "tidyr",        # データ整形
  "meta",         # 追加的なメタアナリシス機能
  "forestplot",   # フォレストプロット作成
  "metamisc",     # 予測モデルのメタアナリシス専用
  "pROC",         # ROC曲線とAUC解析
  "gridExtra",    # 複数プロット配置
  "knitr",        # レポート作成
  "kableExtra"    # 表の整形
))
install.packages("forcats")
# パッケージの読み込み
library(metafor)
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(meta)
library(forestplot)
library(metamisc)
library(pROC)
library(gridExtra)
library(gtsummary)
library(lubridate)
library(purrr)
library(forcats)

# インストール確認
print("metaforパッケージのバージョン:")
packageVersion("metafor")

print("利用可能な関数の一部:")
print("- rma(): ランダム効果モデル")
print("- forest(): フォレストプロット")
print("- funnel(): ファンネルプロット") 
print("- metamisc::valmeta(): 予測モデル検証のメタアナリシス")

# データファイルのパス（バックスラッシュをスラッシュに変更）

file_path <- "your_file_path_here.xlsx"  # 実際のファイルパスに置き換えてください
# Excelファイルの読み込み
# まずシート名を確認
sheet_names <- excel_sheets(file_path)
  print("利用可能なシート名:")
print(sheet_names)

# メインデータをロード（通常は最初のシートまたは"data"という名前のシート）
# いったん全部文字列で読み込むと安全
data <- read_excel(file_path, sheet = 1, col_types = "text")

#日付型を先に処理（Excel内部数値になっているので日付型に、ただし年号はそのままにする、特殊な方は全部NA）
data <- data %>%
  mutate(
    # ---- start_recruitment の処理 ----
    start_serial = suppressWarnings(as.numeric(start_recruitment)),
    start_date = case_when(
      !is.na(start_serial) & start_serial >= 5000 ~ as.Date(start_serial, origin = "1899-12-30"),
      TRUE ~ NA_Date_
    ),

    # ---- end_recruitment の処理 ----
    end_serial = suppressWarnings(as.numeric(end_recruitment)),
    end_date = case_when(
      !is.na(end_serial) & end_serial >= 5000 ~ as.Date(end_serial, origin = "1899-12-30"),
      TRUE ~ NA_Date_
    )
  )

#確認用
#data$start_date %>% summary()
#data$end_date %>% summary()

# 患者リクルート期間を月単位で計算する
data <- data %>%
  mutate(
    start_date = suppressWarnings(parse_date_time(start_date, orders = c("Ymd", "Ym", "Y"))),
    end_date = suppressWarnings(parse_date_time(end_date, orders = c("Ymd", "Ym", "Y"))),
    recruitment_duration_months = case_when(
      is.na(start_date) | is.na(end_date) ~ NA_real_,
      start_date > end_date ~ NA_real_,  # 逆順は不正とみなしてNA
      TRUE ~ time_length(interval(start_date, end_date), "month")
    )
  ) %>%
  mutate(
    # 地域のラベル設定（アルファベット順、Othersは最後）
    Region = factor(Region, 
                   levels = c("Asia", "Europe", "Middle East", 
                             "North America", "Oceania", "South America", "Others"),
                   labels = c("Asia", "Europe", "Middle East", 
                             "North America", "Oceania", "South America", "Others")),
    
    # 研究デザインのラベル設定
    study_design = factor(study_design,
                         levels = c("1", "2", "3", "4"),
                         labels = c("Retrospective cohort", 
                                   "Prospective cohort",
                                   "Post-hoc analysis of RCT",
                                   "Other")),
    
    # 研究セッティングのラベル設定
    setting = factor(setting,
                    levels = c("1", "2", "3", "4", "5"),
                    labels = c("Emergency department",
                              "Inpatient (non-ICU)",
                              "Intensive care unit",
                              "Other",
                              "Not specified")),
    
    # 成人/小児のラベル設定
    Adult = factor(Adult,
                  levels = c(1, 0),
                  labels = c("Adult","Pediatric")),
    
    # 感染症科医関与のラベル設定
    specialist_involvement = factor(specialist_involvement,
                                  levels = c(0, 1),
                                  labels = c("No", "Yes")),
    
    # 基礎疾患のラベル設定
    presense_baseline_disease = factor(presense_baseline_disease,
                                     levels = c(0, 1),
                                     labels = c("No", "Yes")),
                                    
    #数値型へ変換
   
    n_episodes = suppressWarnings(as.numeric(n_episodes)),
    n_tb       = suppressWarnings(as.numeric(n_tb)),
    n_patients = suppressWarnings(as.numeric(n_patients)),
    auc_roc = suppressWarnings(as.numeric(auc_roc)),
    ll_auc_roc = suppressWarnings(as.numeric(ll_auc_roc)),
    ul_auc_roc = suppressWarnings(as.numeric(ul_auc_roc)),
    calibration_measure = suppressWarnings(as.numeric(calibration_measure)),
    calibration_statistics = suppressWarnings(as.numeric(calibration_statistics)),
    oe_ratio = suppressWarnings(as.numeric(oe_ratio)),
    ll_oe_ratio = suppressWarnings(as.numeric(ll_oe_ratio)),
    ul_oe_ratio = suppressWarnings(as.numeric(ul_oe_ratio)),
    o_event = suppressWarnings(as.numeric(o_event)),
    e_event = suppressWarnings(as.numeric(e_event)),
    citl = suppressWarnings(as.numeric(citl)),
    ll_citl = suppressWarnings(as.numeric(ll_citl)),
    ul_citl = suppressWarnings(as.numeric(ul_citl))
  ) %>%
  mutate(
    male_percent_clean = str_remove(male_percent, "%"),
    male_percent_num = suppressWarnings(as.numeric(male_percent_clean)),
    male_percent_num = if_else(
      !is.na(male_percent_num) & male_percent_num < 1,
      male_percent_num * 100,
      male_percent_num
    ),
    female_percent = if_else(!is.na(male_percent_num), 100 - male_percent_num, NA_real_)
  )
　


# データクリーニング：複数のモデルタイプに該当する行を分割
duplicate_rows_for_models <- function(data) {
  
  # 実際の列名を指定
  model_cols <- c("logistic regression", "tree-based models", "Support vector machine",
                  "K nearest neiborhood", "Neural network", "model_others", "model_unknown")
  
  # 列が実際に存在するかチェック
  missing_cols <- setdiff(model_cols, colnames(data))
  if(length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
  
  print("Using model columns:")
  print(model_cols)
  
  # 結果を格納するリスト
  result_list <- list()
  
  # 各行を処理
  for(i in 1:nrow(data)) {
    row_data <- data[i, ]
    
    # 現在の行でどのモデルタイプが1になっているかを確認
    model_flags <- row_data[model_cols]
    # NAを0として扱い、1のものだけを抽出
    active_models <- model_cols[!is.na(model_flags) & model_flags == 1]
    
    # アクティブなモデルが0個または1個の場合はそのまま
    if(length(active_models) <= 1) {
      result_list[[length(result_list) + 1]] <- row_data
    } else {
      # 複数のモデルがアクティブな場合、各モデルごとに行を作成
      for(model in active_models) {
        new_row <- row_data
        # 全てのモデル列を0にリセット
        new_row[model_cols] <- 0
        # 現在のモデルのみ1に設定
        new_row[model] <- 1
        
        result_list[[length(result_list) + 1]] <- new_row
      }
    }
  }
  
  # リストをデータフレームに結合
  result_df <- do.call(rbind, result_list)
  
  # 行名をリセット
  rownames(result_df) <- NULL
  
  return(result_df)
}

# 使用例
cleaned_data <- duplicate_rows_for_models(data) %>%
 mutate(
    model_development_method = pmap_chr(
      list(`logistic regression`, `tree-based models`, `Support vector machine`, 
           `K nearest neiborhood`, `Neural network`, model_others, model_unknown),
      function(lr, tree, svm, knn, nn, other, unknown) {
        methods <- c("lr", "tree", "svm", "knn", "nn", "other", "unknown")
        values <- c(lr, tree, svm, knn, nn, other, unknown)
        matched <- methods[which(values == 1)]
        if (length(matched) == 1) matched else NA_character_
      }
    ),
    model_development_method = recode_factor(
      model_development_method,
      lr = "Logistic regression",
      tree = "Tree-based models",
      svm = "Support vector machine",
      knn = "K-nearest neighborhood",
      nn = "Neural network",
      other = "Other methods",
      unknown = "Unknown"
    )
  ) %>%
  mutate(
    # モデル開発手法の変数
    logistic_regression = factor(`logistic regression`, levels = c(0, 1), labels = c("No", "Yes")),
    tree_based_models = factor(`tree-based models`, levels = c(0, 1), labels = c("No", "Yes")),
    support_vector_machine = factor(`Support vector machine`, levels = c(0, 1), labels = c("No", "Yes")),
    k_nearest_neighborhood = factor(`K nearest neiborhood`, levels = c(0, 1), labels = c("No", "Yes")),
    neural_network = factor(`Neural network`, levels = c(0, 1), labels = c("No", "Yes")),
    model_others = factor(model_others, levels = c(0, 1), labels = c("No", "Yes")),
    model_unknown = factor(model_unknown, levels = c(0, 1), labels = c("No", "Yes")),
    
    # 予測因子の変数
    predictor_age = factor(predictor_age, levels = c(0, 1), labels = c("No", "Yes")),
    predictor_sex = factor(predictor_sex, levels = c(0, 1), labels = c("No", "Yes")),
    predictor_vital = factor(predictor_vital, levels = c(0, 1), labels = c("No", "Yes")),
    predictor_lab = factor(predictor_lab, levels = c(0, 1), labels = c("No", "Yes")),
    predictor_other = factor(predictor_other, levels = c(0, 1), labels = c("No", "Yes")),
  )
 

# 結果の確認関数
check_duplication_results <- function(original_data, cleaned_data, study_id_col = "study_id") {
  cat("Original data rows:", nrow(original_data), "\n")
  cat("Cleaned data rows:", nrow(cleaned_data), "\n")
  
  # 複数モデルを持っていた研究を特定
  model_cols <- c("logistic regression", "tree-based models", "Support vector machine",
                  "K nearest neiborhood", "Neural network", "model_others", "model_unknown")
  
  original_multi_model <- original_data %>%
    mutate(model_count = rowSums(select(., all_of(model_cols)))) %>%
    filter(model_count > 1)
  
  if(nrow(original_multi_model) > 0) {
    cat("\nStudies with multiple models (before cleaning):\n")
    print(original_multi_model[c(study_id_col, model_cols, "model_count")])
    
    # クリーニング後の対応する行を表示
    cat("\nCorresponding rows after cleaning:\n")
    for(study in original_multi_model[[study_id_col]]) {
      study_rows <- cleaned_data[cleaned_data[[study_id_col]] == study, ]
      print(study_rows[c(study_id_col, model_cols)])
      cat("---\n")
    }
  } else {
    cat("\nNo studies with multiple models found.\n")
  }
}

# 使用例:
check_duplication_results(data, cleaned_data)

#Save as RDS file
cleaned_data %>% saveRDS(file = "your_cleaned_data_file.rds")  # 実際のファイルパスに置き換えてください

# 内的検証データのみを抽出
internal_validation <- cleaned_data %>%
  filter(validation_type == 1)
#data_for_summary <- internal_validation %>%
#  distinct(study_id, .keep_all = TRUE)

# メイン表の作成
table1a <- internal_validation %>%
  select(n_patients,n_episodes,n_tb, female_percent, recruitment_duration_months,Adult,study_design, setting, Region,  
  specialist_involvement, presense_baseline_disease,model_development_method
         ) %>%
  tbl_summary(
    label = list(
      Region ~ "Geographic region",
      study_design ~ "Study design",
      setting ~ "Clinical setting",
      n_patients ~ "Number of patients",
      n_episodes ~ "Number of episodes",
      n_tb ~ "Number of events of true bacteremia",
      Adult ~ "Patient population",
      female_percent ~ "Percentage of female patients",
      recruitment_duration_months ~ "Recruitment duration (months)",
      specialist_involvement ~ "Infectious disease specialist involvement in assessment of true bacteremia",
      presense_baseline_disease ~ "Presence of baseline disease",
      model_development_method ~ "Model development method"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
    add_n(statistic = "{N_miss} ({p_miss}%)", 
        col_label = "Not reported",
        last = TRUE) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_caption(paste0("Table 1 Characteristics of models with internal validation in ", 
                     n_distinct(internal_validation$study_id), 
                     " studies")) %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as median (IQR) for the following continuous variables: 
  Number of patients, Number of episodes, Number of events of true bacteremia, Percentage of female patients, 
  and Recruitment duration. All other values are presented as n (%). \"Not reported\" indicates the number of 
  studies for which data were not reported.")

# 表の表示
print(table1a)
table1a %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "your_output_path/table1a.docx")  # 実際の出力パスに置き換えてください

# 外的検証データのみを抽出----------------------------------------------------
external_validation <- cleaned_data %>%
  filter(validation_type == 2)

# モデル名ごとの検証研究数をカウント
model_counts <- external_validation %>%
  group_by(model_name) %>%
  summarise(
    n_validations = n(),
    .groups = 'drop'
  ) %>%
  arrange(desc(n_validations))

print("各モデルの外的検証研究数:")
print(model_counts)

# 3つ以上の外的検証があるモデルを特定 NRは除く
models_with_3plus <- model_counts %>%
  filter(n_validations >= 3,model_name != "NR") %>%
  pull(model_name)

print("3つ以上の外的検証があるモデル:")
print(models_with_3plus)

# 5つ以上の外的検証があるモデルを特定 NRは除く
models_with_5plus <- model_counts %>%
  filter(n_validations >= 5,model_name != "NR") %>%
  pull(model_name)

#多い順に順番をつける  
category_order <- external_validation %>%
    mutate(
      model_name_category = case_when(
        is.na(model_name) | str_trim(model_name) == "" | model_name == "NR" ~ "Others",
        model_name %in% models_with_5plus ~ model_name,
        TRUE ~ "Others"
      )
    ) %>%
    count(model_name_category, sort = TRUE) %>%
    pull(model_name_category)


#table1b characteriscs of models with external validation
table1b <- external_validation %>%
     mutate(
     model_name_category = case_when(
       is.na(model_name) | str_trim(model_name) == "" | model_name == "NR" ~ "Others",
       model_name %in% models_with_5plus ~ model_name,
       TRUE ~ "Others"
     ),
      model_name_category = factor(model_name_category, levels = category_order)
   ) %>%
  select(n_patients,n_episodes,n_tb, female_percent, recruitment_duration_months,Adult,study_design, setting, Region,  
  specialist_involvement, presense_baseline_disease, model_name_category
         ) %>%
  tbl_summary(
    label = list(
      Region ~ "Geographic region",
      study_design ~ "Study design",
      setting ~ "Clinical setting",
      n_patients ~ "Number of patients",
      n_episodes ~ "Number of episodes",
      n_tb ~ "Number of events of true bacteremia",
      Adult ~ "Patient population",
      female_percent ~ "Percentage of female patients",
      recruitment_duration_months ~ "Recruitment duration (months)",
      specialist_involvement ~ "Infectious disease specialist involvement in assessment of true bacteremia",
      presense_baseline_disease ~ "Presence of baseline disease",
      model_name_category ~ "Model name"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
    add_n(statistic = "{N_miss} ({p_miss}%)", 
        col_label = "Not reported",
        last = TRUE) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_caption(paste0("Table 1b Characteristics of external validation in ", 
                     n_distinct(external_validation$study_id), 
                     " studies for",
                     n_distinct(external_validation$model_name),
                     " models")) %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as median (IQR) for the following continuous variables:
  Number of patients, Number of episodes, Number of events of true bacteremia, Percentage of female patients, 
  and Recruitment duration. All other values are presented as n (%). \"Not reported\" indicates the number of 
  studies for which data were not reported."
  )

table1b %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "your_output_path/table1b.docx")  # 実際の出力パスに置き換えてください

# study_idごとにデータを集約
table1_data <- cleaned_data %>%
  group_by(study_id) %>%
  summarise(
    # 連続変数：最大値を取る（全てNAの場合はNA）
    n_patients = ifelse(all(is.na(n_patients)), NA_real_, max(n_patients, na.rm = TRUE)),
    n_episodes = ifelse(all(is.na(n_episodes)), NA_real_, max(n_episodes, na.rm = TRUE)),
    n_tb = ifelse(all(is.na(n_tb)), NA_real_, max(n_tb, na.rm = TRUE)),
    female_percent = ifelse(all(is.na(female_percent)), NA_real_, max(female_percent, na.rm = TRUE)),
    recruitment_duration_months = ifelse(all(is.na(recruitment_duration_months)), NA_real_, max(recruitment_duration_months, na.rm = TRUE)),
    
    # カテゴリカル変数：study_idごとで共通なので最初の行
    Adult = first(Adult),
    setting = first(setting),
    specialist_involvement = first(specialist_involvement),
    presense_baseline_disease = first(presense_baseline_disease),
    
    # study_design：すべて同じなら同じ値、混ざっていれば4（Other）
    study_design = case_when(
      all(study_design == "Retrospective cohort", na.rm = TRUE) ~ "Retrospective cohort",
      all(study_design == "Prospective cohort", na.rm = TRUE) ~ "Prospective cohort",
      all(study_design == "Post-hoc analysis of RCT", na.rm = TRUE) ~ "Post-hoc analysis of RCT",
      TRUE ~ "Other"
    ),
    
    # Region：すべて同じなら同じ値、混ざっていれば"Others"
    Region = case_when(
      all(Region == "Asia", na.rm = TRUE) ~ "Asia",
      all(Region == "Europe", na.rm = TRUE) ~ "Europe",
      all(Region == "Middle East", na.rm = TRUE) ~ "Middle East",
      all(Region == "North America", na.rm = TRUE) ~ "North America",
      all(Region == "Oceania", na.rm = TRUE) ~ "Oceania",
      all(Region == "South America", na.rm = TRUE) ~ "South America",
      TRUE ~ "Others"
    ),
    
    # validation_type：1=Internal validation, 2=External validation
    validation_type = case_when(
      all(validation_type == 1, na.rm = TRUE) ~ "Internal validation",
      all(validation_type == 2, na.rm = TRUE) ~ "External validation",
      TRUE ~ "Both"
    ),
    
    .groups = "drop"
  ) %>%
  # factorの順序を再設定
  mutate(
    Region = factor(Region,
                    levels = c("Asia", "Europe", "Middle East",
                              "North America", "Oceania", "South America", "Others")),
    study_design = factor(study_design,
                         levels = c("Retrospective cohort", "Prospective cohort",
                                   "Post-hoc analysis of RCT", "Other")),
    validation_type = factor(validation_type,
                            levels = c("Internal validation", "External validation", "Both"))
  )

# table1の作成
table1 <- table1_data %>%
  select(n_patients, n_episodes, n_tb, female_percent, recruitment_duration_months,
         Adult, study_design, setting, Region, specialist_involvement, 
         presense_baseline_disease, validation_type) %>%
  tbl_summary(
    label = list(
      Region ~ "Geographic region",
      study_design ~ "Study design",
      setting ~ "Clinical setting",
      n_patients ~ "Number of patients",
      n_episodes ~ "Number of episodes",
      n_tb ~ "Number of events of true bacteremia",
      Adult ~ "Patient population",
      female_percent ~ "Percentage of female patients",
      recruitment_duration_months ~ "Recruitment duration (months)",
      specialist_involvement ~ "Infectious disease specialist involvement in assessment of true bacteremia",
      presense_baseline_disease ~ "Presence of baseline disease",
      validation_type ~ "Validation type"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    missing = "no"
  ) %>%
  add_n(statistic = "{N_miss} ({p_miss}%)", 
        col_label = "Not reported",
        last = TRUE) %>%
  modify_header(label ~ "**Characteristic**") %>%
  modify_caption(paste0("Table 1 Characteristics of studies (n = ", 
                     n_distinct(table1_data$study_id), 
                     " studies)")) %>%
  modify_footnote(all_stat_cols() ~ "Data are presented as median (IQR) for the following continuous variables: Number of patients, Number of episodes, Number of events of true bacteremia, Percentage of female patients, and Recruitment duration. All other values are presented as n (%). \"Not reported\" indicates the number of studies for which data were not reported.")

# 表を保存
table1 %>% 
  as_flex_table() %>% 
  flextable::save_as_docx(path = "your_output_path/table1.docx")  # 実際の出力パスに置き換えてください

external_validation$model_name %>% table() %>% as_flex_table() %>% 
  flextable::save_as_docx(path = "your_output_path/model_name_counts.docx")  # 実際の出力パスに置き換えてください

# メタアナリシス対象データを抽出
meta_analysis_data <- external_validation %>%
  filter(model_name %in% models_with_3plus)

print(paste("メタアナリシス対象研究数:", nrow(meta_analysis_data)))
print(paste("対象モデル数:", length(models_with_3plus)))

# 各モデルの詳細を確認
for(model in models_with_3plus) {
  cat("\n=== モデル:", model, "===\n")
  model_data <- meta_analysis_data %>% filter(model_name == model)
  print(model_data %>% select(author_year, model_name, auc_roc, n_episodes))
}

qsofa_data <- meta_analysis_data %>%
  filter(model_name == "qSOFA") %>%
  # Remove rows where c-statistic is missing
  filter(!is.na(auc_roc)) %>%
  mutate(n_episodes = as.numeric(n_episodes),
         n_tb = as.numeric(n_tb)
  )

# Check the data structure
print("qSOFA data summary:")
print(qsofa_data)
print(paste("Number of studies:", nrow(qsofa_data)))

cstat_results <- ccalc(
  cstat = qsofa_data$auc_roc,
  cstat.cilb = qsofa_data$ll_auc_roc,
  cstat.ciub = qsofa_data$ul_auc_roc,
  N = qsofa_data$n_episodes,
  O = qsofa_data$n_tb,
  slab = qsofa_data$author_year,
  level = 0.95
)

# Display the processed results
print("Processed c-statistics results:")
print(cstat_results)

meta_cstat <- valmeta(
  measure = "cstat",
  cstat = cstat_results$theta,
  cstat.se = cstat_results$theta.se,
  cstat.cilb = cstat_results$theta.cilb,
  cstat.ciub = cstat_results$theta.ciub,
  N = qsofa_data$n_episodes,
  O = qsofa_data$n_tb,
  slab = qsofa_data$author_year,
  method = "REML"
)

# Print meta-analysis summary
print("Meta-analysis results:")
print(meta_cstat)

# Method 1: Using valmeta's built-in plot method (recommended)
forest_plot1 <- plot(meta_cstat, 
                     title = "qSOFA Model: Discriminative Performance")

print("Method 1 (built-in plot):")
print(forest_plot1)

forest_plot2 <- metamisc::forest(
  theta = cstat_results$theta,
  theta.ci.lb = cstat_results$theta.cilb,
  theta.ci.ub = cstat_results$theta.ciub,
  theta.slab = qsofa_data$author_year,
  theta.summary = meta_cstat$est,
  theta.summary.ci.lb = meta_cstat$ci.lb,
  theta.summary.ci.ub = meta_cstat$ci.ub,
  theta.summary.pi.lb = meta_cstat$pi.lb,
  theta.summary.pi.ub = meta_cstat$pi.ub,
  
  title = "qSOFA Model: Discriminative Performance",
  xlab = "C-statistic (95% Confidence Interval)",
  xlim = c(0.4, 1.0),
  refline = 0.7,  # Good discrimination threshold
  
  label.summary = "Random Effects Pooled Estimate (REML)",
  label.predint = "95% Prediction Interval",
  predint.linetype = 1,
  
  sort = "asc",
  study.digits = 3,
  nrows.before.summary = 1,
  
  # Visual customization
  col.diamond = "blue",
  col.predint = "darkred",
  size.study = 1.0,
  size.predint = 1.5,
  study.shape = 18,  # Diamond shape
  lty.ref = "longdash",
  
  # ggplot2 theme
  theme = theme_bw() + theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 14),
    text = element_text(size = 14)
  )
)

print(forest_plot2)

#各モデルの情報をまとめる

# 基本情報の表
basic_summary <- external_validation %>%
    filter(model_name %in% models_with_3plus) %>%
    group_by(model_name) %>%
    summarise(
      number_of_studies = n(),                           # 研究数
      total_n_episodes = sum(n_episodes, na.rm = TRUE), # 総エピソード数
      total_n_tb = sum(n_tb, na.rm = TRUE),             # 総イベント数
      .groups = "drop"
    ) %>%
    # 研究数の多い順に並べ替え
    arrange(desc(number_of_studies), desc(total_n_episodes))
  
View(basic_summary)

# 各モデルのC統計量メタアナリシス結果を計算-------------------------------------------
cstat_results_list <- list()

for(model in basic_summary$model_name) {
  
  cat("Processing C-statistics meta-analysis for:", model, "\n")
  
  # データフィルタリング
  model_data <- external_validation %>%
    filter(model_name == model, !is.na(auc_roc)) %>%
    mutate(
      n_episodes = as.numeric(n_episodes),
      n_tb = as.numeric(n_tb)
    )
  
  # データが2研究未満の場合はNA
  if(nrow(model_data) < 2) {
    cstat_results_list[[model]] <- data.frame(
      model_name = model,
      cstat_studies = nrow(model_data),
      pooled_cstat = NA,
      cstat_ci_lower = NA,
      cstat_ci_upper = NA,
      cstat_summary = "Insufficient data"
    )
    next
  }
  
  # メタアナリシス実行
  tryCatch({
    # ccalcで前処理
    cstat_calc <- ccalc(
      cstat = model_data$auc_roc,
      cstat.cilb = model_data$ll_auc_roc,
      cstat.ciub = model_data$ul_auc_roc,
      N = model_data$n_episodes,
      O = model_data$n_tb,
      slab = model_data$author_year,
      level = 0.95
    )
    
    # メタアナリシス実行
    meta_result <- valmeta(
      measure = "cstat",
      cstat = cstat_calc$theta,
      cstat.se = cstat_calc$theta.se,
      N = model_data$n_episodes,
      O = model_data$n_tb,
      slab = model_data$author_year,
      method = "REML"
    )
    
    # 結果を格納
    cstat_results_list[[model]] <- data.frame(
      model_name = model,
      cstat_studies = nrow(model_data),
      pooled_cstat = round(meta_result$est, 3),
      cstat_ci_lower = round(meta_result$ci.lb, 3),
      cstat_ci_upper = round(meta_result$ci.ub, 3),
      cstat_summary = paste0(round(meta_result$est, 3), " (", 
                            round(meta_result$ci.lb, 3), "-", 
                            round(meta_result$ci.ub, 3), ")")
    )
    
  }, error = function(e) {
    cat("Error in meta-analysis for", model, ":", e$message, "\n")
    cstat_results_list[[model]] <- data.frame(
      model_name = model,
      cstat_studies = nrow(model_data),
      pooled_cstat = NA,
      cstat_ci_lower = NA,
      cstat_ci_upper = NA,
      cstat_summary = "Analysis failed"
    )
  })
}

# 結果をデータフレームに結合
cstat_results_df <- do.call(rbind, cstat_results_list)

# basic_summaryに結果を追加
basic_summary_with_cstat <- basic_summary %>%
  left_join(cstat_results_df, by = "model_name") %>%
  select(model_name, number_of_studies, total_n_episodes, total_n_tb,
         cstat_studies, pooled_cstat, cstat_ci_lower, cstat_ci_upper, cstat_summary)

# 結果表示
print("=== Basic Summary with C-statistics Meta-analysis Results ===")
View(basic_summary_with_cstat)

# 成功した分析の数を確認
successful_analyses <- sum(!is.na(basic_summary_with_cstat$pooled_cstat))
cat("\nSuccessful C-statistics meta-analyses:", successful_analyses, "out of", nrow(basic_summary_with_cstat), "\n")

# データ不足で分析できなかったモデルを表示
insufficient_data <- basic_summary_with_cstat %>%
  filter(is.na(pooled_cstat) | cstat_studies < 2)

if(nrow(insufficient_data) > 0) {
  cat("\nModels with insufficient data for C-statistics meta-analysis:\n")
  print(insufficient_data %>% select(model_name, cstat_studies, cstat_summary))
}
# O:E比メタアナリシスのためのデータ準備と実行---------------------------------------
# 各モデルのO:E比メタアナリシス結果を計算
oe_results_list <- list()

for(model in basic_summary_with_cstat$model_name) {
  
  cat("Processing O:E ratio meta-analysis for:", model, "\n")
  
  # データフィルタリング（O:E比関連データがあるもの）
  model_data <- external_validation %>%
    filter(model_name == model) %>%
    # O:E比、CITL、またはo_event/e_eventのいずれかがあるものを選択
    filter(!is.na(oe_ratio) | !is.na(citl) | (!is.na(o_event) & !is.na(e_event))) %>%
    mutate(
      n_episodes = as.numeric(as.character(n_episodes)),
      o_event = as.numeric(as.character(n_tb)),
      e_event = as.numeric(as.character(e_event)),
      oe_ratio = as.numeric(as.character(oe_ratio)),
      ll_oe_ratio = as.numeric(as.character(ll_oe_ratio)),
      ul_oe_ratio = as.numeric(as.character(ul_oe_ratio)),
      citl = as.numeric(as.character(citl)),
      ll_citl = as.numeric(as.character(ll_citl)),
      ul_citl = as.numeric(as.character(ul_citl))
    )

  # データが2研究未満の場合はNA
  if(nrow(model_data) < 2) {
    oe_results_list[[model]] <- data.frame(
      model_name = model,
      oe_studies = nrow(model_data),
      pooled_oe = NA,
      oe_ci_lower = NA,
      oe_ci_upper = NA,
      oe_summary = "Insufficient data"
    )
    next
  }
  
  # メタアナリシス実行
  tryCatch({
    # oecalcで前処理（利用可能な情報を全て使用）
    oe_calc <- oecalc(
      OE = model_data$oe_ratio,
      OE.cilb = model_data$ll_oe_ratio,
      OE.ciub = model_data$ul_oe_ratio,
      citl = model_data$citl,
      N = model_data$n_episodes,
      O = model_data$n_tb,
      E = model_data$e_event,
      slab = model_data$author_year,
      level = 0.95
    )
    
    # メタアナリシス実行
    meta_result <- valmeta(
      measure = "OE",
      OE = oe_calc$theta,
      OE.se = oe_calc$theta.se,
      OE.cilb = oe_calc$theta.cilb,
      OE.ciub = oe_calc$theta.ciub,
      N = model_data$n_episodes,
      slab = model_data$author_year,
      method = "REML"
    )
    
    # 結果を格納
    oe_results_list[[model]] <- data.frame(
      model_name = model,
      oe_studies = nrow(model_data),
      pooled_oe = round(meta_result$est, 3),
      oe_ci_lower = round(meta_result$ci.lb, 3),
      oe_ci_upper = round(meta_result$ci.ub, 3),
      oe_summary = paste0(round(meta_result$est, 3), " (", 
                         round(meta_result$ci.lb, 3), "-", 
                         round(meta_result$ci.ub, 3), ")")
    )
    
  }, error = function(e) {
    cat("Error in O:E meta-analysis for", model, ":", e$message, "\n")
    
    # エラーが発生した場合、より簡単な方法を試行
    tryCatch({
      # O:E比が直接利用可能な場合のみを使用
      simple_data <- model_data %>%
        filter(!is.na(oe_ratio))
      
      if(nrow(simple_data) >= 2) {
        simple_calc <- oecalc(
          OE = simple_data$oe_ratio,
          OE.cilb = simple_data$ll_oe_ratio,
          OE.ciub = simple_data$ul_oe_ratio,
          N = simple_data$n_episodes,
          slab = simple_data$author_year
        )
        
        simple_meta <- valmeta(
          measure = "OE",
          OE = simple_calc$theta,
          OE.se = simple_calc$theta.se,
          N = simple_data$n_episodes,
          slab = simple_data$author_year,
          method = "REML"
        )
        
        oe_results_list[[model]] <- data.frame(
          model_name = model,
          oe_studies = nrow(simple_data),
          pooled_oe = round(simple_meta$est, 3),
          oe_ci_lower = round(simple_meta$ci.lb, 3),
          oe_ci_upper = round(simple_meta$ci.ub, 3),
          oe_summary = paste0(round(simple_meta$est, 3), " (", 
                             round(simple_meta$ci.lb, 3), "-", 
                             round(simple_meta$ci.ub, 3), ")")
        )
      } else {
        oe_results_list[[model]] <- data.frame(
          model_name = model,
          oe_studies = nrow(model_data),
          pooled_oe = NA,
          oe_ci_lower = NA,
          oe_ci_upper = NA,
          oe_summary = "Analysis failed"
        )
      }
    }, error = function(e2) {
      oe_results_list[[model]] <- data.frame(
        model_name = model,
        oe_studies = nrow(model_data),
        pooled_oe = NA,
        oe_ci_lower = NA,
        oe_ci_upper = NA,
        oe_summary = "Analysis failed"
      )
    })
  })
}

# 結果をデータフレームに結合
oe_results_df <- do.call(rbind, oe_results_list)

# basic_summary_with_cstatにO:E比の結果を追加
basic_summary_with_oe <- basic_summary_with_cstat %>%
  left_join(oe_results_df, by = "model_name") %>%
  select(model_name, number_of_studies, total_n_episodes, total_n_tb,
         cstat_studies, pooled_cstat, cstat_ci_lower, cstat_ci_upper, cstat_summary,
         oe_studies, pooled_oe, oe_ci_lower, oe_ci_upper, oe_summary)

# 結果表示
#print("=== Basic Summary with C-statistics and O:E Ratio Meta-analysis Results ===")
View(basic_summary_with_oe)

# 成功した分析の数を確認
successful_oe_analyses <- sum(!is.na(basic_summary_with_oe$pooled_oe))
cat("\nSuccessful O:E ratio meta-analyses:", successful_oe_analyses, "out of", nrow(basic_summary_with_oe), "\n")

# データ不足で分析できなかったモデルを表示
insufficient_oe_data <- basic_summary_with_oe %>%
  filter(is.na(pooled_oe) | oe_studies < 2)

if(nrow(insufficient_oe_data) > 0) {
  cat("\nModels with insufficient data for O:E ratio meta-analysis:\n")
  print(insufficient_oe_data %>% select(model_name, oe_studies, oe_summary))
}

# 利用可能なデータタイプの確認
cat("\n=== Data availability summary ===\n")
for(model in basic_summary_with_oe$model_name[1:5]) {  # 最初の5つのモデルを例として
  model_data <- external_validation %>% filter(model_name == model)
  cat("Model:", model, "\n")
  cat("  - Direct O:E ratio:", sum(!is.na(model_data$oe_ratio)), "studies\n")
  cat("  - CITL data:", sum(!is.na(model_data$citl)), "studies\n")
  cat("  - O/E events:", sum(!is.na(model_data$o_event) & !is.na(model_data$e_event)), "studies\n")
  cat("\n")
}

library(dplyr)

# PROBAST RoB評価の集計---------------------------------------------
probast_rob_summary <- external_validation %>%
  filter(model_name %in% basic_summary_with_oe$model_name) %>%
  filter(!is.na(probast_rob_overall)) %>%
  group_by(model_name) %>%
  summarise(
    rob_total_studies = n(),
    rob_low = sum(probast_rob_overall == "Low", na.rm = TRUE),
    rob_uncertain = sum(probast_rob_overall == "Uncertain", na.rm = TRUE),
    rob_high = sum(probast_rob_overall == "High", na.rm = TRUE),
    rob_low_pct = round(rob_low / rob_total_studies * 100, 1),
    rob_uncertain_pct = round(rob_uncertain / rob_total_studies * 100, 1),
    rob_high_pct = round(rob_high / rob_total_studies * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    probast_rob = paste0("Low: ", rob_low, " (", rob_low_pct, "%); ",
                        "Uncertain: ", rob_uncertain, " (", rob_uncertain_pct, "%); ",
                        "High: ", rob_high, " (", rob_high_pct, "%)")
  ) %>%
  select(model_name, rob_total_studies, probast_rob)

# PROBAST Applicability評価の集計
probast_app_summary <- external_validation %>%
  filter(model_name %in% basic_summary_with_oe$model_name) %>%
  filter(!is.na(probast_app_overall)) %>%
  group_by(model_name) %>%
  summarise(
    app_total_studies = n(),
    app_low = sum(probast_app_overall == "Low", na.rm = TRUE),
    app_uncertain = sum(probast_app_overall == "Uncertain", na.rm = TRUE),
    app_high = sum(probast_app_overall == "High", na.rm = TRUE),
    app_low_pct = round(app_low / app_total_studies * 100, 1),
    app_uncertain_pct = round(app_uncertain / app_total_studies * 100, 1),
    app_high_pct = round(app_high / app_total_studies * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    probast_app = paste0("Low: ", app_low, " (", app_low_pct, "%); ",
                        "Uncertain: ", app_uncertain, " (", app_uncertain_pct, "%); ",
                        "High: ", app_high, " (", app_high_pct, "%)")
  ) %>%
  select(model_name, app_total_studies, probast_app)

# basic_summary_with_oeにPROBAST結果を追加
basic_summary_with_probast <- basic_summary_with_oe %>%
  left_join(probast_rob_summary, by = "model_name") %>%
  left_join(probast_app_summary, by = "model_name") %>%
  select(model_name, number_of_studies, total_n_episodes, total_n_tb,
         cstat_studies, pooled_cstat, cstat_ci_lower, cstat_ci_upper, cstat_summary,
         oe_studies, pooled_oe, oe_ci_lower, oe_ci_upper, oe_summary,
         rob_total_studies, probast_rob,
         app_total_studies, probast_app)

# 結果表示
#print("=== Summary with PROBAST Evaluations ===")
View(basic_summary_with_probast)

# CSVファイルとして保存
basic_summary_with_probast %>%
  select(model_name, number_of_studies, total_n_episodes, total_n_tb,
         cstat_studies, cstat_summary,
         oe_studies, oe_summary,
         probast_rob,
         probast_app) %>%
  rename(
    "Model name" = model_name,
    "Number of studies" = number_of_studies,
    "Total sample size" = total_n_episodes,
    "Total events" = total_n_tb,
    "Studies included in synthesis of C-statistics" = cstat_studies,
    "Summary of C-statistics" = cstat_summary,
    "Studies included in synthesis of O:E ratio" = oe_studies,
    "Summary of O:E ratio" = oe_summary,
    "Risk of bias" = probast_rob,
    "Applicability" = probast_app
  ) %>%
  write.csv(file = "basic_summary_with_probast.csv", 
            row.names = FALSE,
            na = "",
            fileEncoding = "UTF-8")

# PROBAST評価の概要統計
cat("\n=== PROBAST Evaluation Summary ===\n")
cat("Models with RoB evaluations:", sum(!is.na(basic_summary_with_probast$probast_rob)), "\n")
cat("Models with Applicability evaluations:", sum(!is.na(basic_summary_with_probast$probast_app)), "\n")

# PROBAST評価がないモデルを確認
missing_rob <- basic_summary_with_probast %>%
  filter(is.na(probast_rob)) %>%
  select(model_name, number_of_studies)

missing_app <- basic_summary_with_probast %>%
  filter(is.na(probast_app)) %>%
  select(model_name, number_of_studies)

if(nrow(missing_rob) > 0) {
  cat("\nModels without RoB evaluations:\n")
  print(missing_rob)
}

if(nrow(missing_app) > 0) {
  cat("\nModels without Applicability evaluations:\n")
  print(missing_app)
}

# サンプル結果の表示
cat("\n=== Sample PROBAST Results ===\n")
sample_probast <- basic_summary_with_probast %>%
  filter(!is.na(probast_rob) | !is.na(probast_app)) %>%
  head(3) %>%
  select(model_name, probast_rob, probast_app)

print(sample_probast)

# 1. Risk of Bias (RoB) Stacked Bar Chart
create_rob_chart <- function(data) {
  
  # RoBデータを準備
  rob_data <- data %>%
    select(probast_rob_participants, probast_rob_predictor, 
           probast_rob_outcome, probast_rob_analysis, probast_rob_overall) %>%
    rename(
      "Participants" = probast_rob_participants,
      "Predictors" = probast_rob_predictor,
      "Outcome" = probast_rob_outcome,
      "Analysis" = probast_rob_analysis,
      "Overall" = probast_rob_overall
    ) %>%
    # データを長い形式に変換
    pivot_longer(everything(), names_to = "Domain", values_to = "Rating") %>%
    filter(!is.na(Rating)) %>%
    # カウント
    count(Domain, Rating) %>%
    # ドメインの順序を指定
    mutate(Domain = factor(Domain, levels = c("Participants", "Predictors", "Outcome", "Analysis", "Overall"))) %>%
    # 評価レベルの順序を指定（下からLow, Uncertain, High）
    mutate(Rating = factor(Rating, levels = c("High", "Uncertain", "Low")))
  
  # パーセンテージを計算
  rob_summary <- rob_data %>%
    group_by(Domain) %>%
    mutate(
      total = sum(n),
      percentage = round(n / total * 100, 1)
    ) %>%
    ungroup()
  
  # グラフ作成
  rob_plot <- ggplot(rob_summary, aes(x = Domain, y = percentage, fill = Rating)) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c("Low" = "#4CAF50", "Uncertain" = "#FFC107", "High" = "#F44336"),
      name = "Risk of bias",
      breaks = c("Low", "Uncertain", "High")  # 凡例の順序を明示的に指定
    ) +
    scale_y_continuous(
      limits = c(0, 102),  # 上限を少し上げる
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = "Risk of Bias Assessment (PROBAST+AI)",
      subtitle = "All studies (n=178)",
      x = "",
      y = "Percentage of studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 22),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),
      axis.text.y = element_text(size = 22),
      axis.title.y = element_text(size = 18),
      legend.position = "top",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      panel.grid.minor = element_blank()
    )
  
  return(list(plot = rob_plot, data = rob_summary))
}

# 2. Applicability Stacked Bar Chart
create_applicability_chart <- function(data) {
  
  # Applicabilityデータを準備
  app_data <- data %>%
    select(probast_app_participants, probast_app_predictor, 
           probast_app_outcome, probast_app_overall) %>%
    rename(
      "Participants" = probast_app_participants,
      "Predictors" = probast_app_predictor,
      "Outcome" = probast_app_outcome,
      "Overall" = probast_app_overall
    ) %>%
    # データを長い形式に変換
    pivot_longer(everything(), names_to = "Domain", values_to = "Rating") %>%
    filter(!is.na(Rating)) %>%
    # カウント
    count(Domain, Rating) %>%
    # ドメインの順序を指定（Analysisドメインなし）
    mutate(Domain = factor(Domain, levels = c("Participants", "Predictors", "Outcome", "Overall"))) %>%
    # 評価レベルの順序を指定（下からLow, Uncertain, High）
    mutate(Rating = factor(Rating, levels = c("High", "Uncertain", "Low")))
  
  # パーセンテージを計算
  app_summary <- app_data %>%
    group_by(Domain) %>%
    mutate(
      total = sum(n),
      percentage = round(n / total * 100, 1)
    ) %>%
    ungroup()
  
  # グラフ作成
  app_plot <- ggplot(app_summary, aes(x = Domain, y = percentage, fill = Rating)) +
    geom_col(position = "stack", width = 0.7) +
    scale_fill_manual(
      values = c("Low" = "#4CAF50", "Uncertain" = "#FFC107", "High" = "#F44336"),
      name = "Applicability",
      breaks = c("Low", "Uncertain", "High")  # 凡例の順序を明示的に指定
    ) +
    scale_y_continuous(
      limits = c(0, 102),  # 上限を少し上げる
      breaks = seq(0, 100, 25),
      labels = function(x) paste0(x, "%")
    ) +
    labs(
      title = "Applicability Assessment (PROBAST+AI)",
      subtitle = "All studies (n=178)",
      x = "",
      y = "Percentage of studies"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 24, face = "bold"),
      plot.subtitle = element_text(size = 22),
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 18),
      axis.text.y = element_text(size = 22),
      axis.title.y = element_text(size = 18),
      legend.position = "top",
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      panel.grid.minor = element_blank()
    )
  
  return(list(plot = app_plot, data = app_summary))
}

# グラフを作成
rob_chart <- create_rob_chart(external_validation)
applicability_chart <- create_applicability_chart(external_validation)

# グラフを表示
print("Risk of Bias Chart:")
print(rob_chart$plot)

print("Applicability Chart:")
print(applicability_chart$plot)

#Extract the data
rob_chart$data
applicability_chart$data
# 両方のグラフを並べて表示（オプション）
library(gridExtra)
combined_plot <- grid.arrange(rob_chart, applicability_chart, ncol = 2)
ggsave("your_path", combined_plot, width = 20, height = 10, dpi = 300)

# グラフを保存（オプション）
ggsave("probast_rob_chart.png", rob_chart, width = 10, height = 6, dpi = 300)
ggsave("probast_applicability_chart.png", applicability_chart, width = 8, height = 6, dpi = 300)
ggsave("probast_combined_chart.png", combined_plot, width = 10, height = 12, dpi = 300)

# Function to create forest plot for any model
create_forest_plot <- function(model_name, meta_analysis_data, method = "both") {
  
  # Filter and prepare data for the specified model
  model_data <- meta_analysis_data %>%
    filter(model_name == !!model_name) %>%
    # Remove rows where c-statistic is missing
    filter(!is.na(auc_roc)) %>%
    mutate(n_episodes = as.numeric(n_episodes),
           n_tb = as.numeric(n_tb))
  
  # Check if data exists for this model
  if (nrow(model_data) == 0) {
    warning(paste("No data found for model:", model_name))
    return(NULL)
  }
  
  # Print data summary
  cat("=== Data summary for", model_name, "===\n")
  print(model_data)
  cat("Number of studies:", nrow(model_data), "\n\n")
  
  # Calculate c-statistics using ccalc
  cstat_results <- ccalc(
    cstat = model_data$auc_roc,
    cstat.cilb = model_data$ll_auc_roc,
    cstat.ciub = model_data$ul_auc_roc,
    N = model_data$n_episodes,
    O = model_data$n_tb,
    slab = model_data$author_year,
    level = 0.95
  )
  
  # Display the processed results
  cat("=== Processed c-statistics results ===\n")
  print(cstat_results)
  cat("\n")
  
  # Perform meta-analysis
  meta_cstat <- valmeta(
    measure = "cstat",
    cstat = cstat_results$theta,
    cstat.se = cstat_results$theta.se,
    cstat.cilb = cstat_results$theta.cilb,
    cstat.ciub = cstat_results$theta.ciub,
    N = model_data$n_episodes,
    O = model_data$n_tb,
    slab = model_data$author_year,
    method = "REML"
  )
  
  # Print meta-analysis summary
  cat("=== Meta-analysis results ===\n")
  print(meta_cstat)
  cat("\n")
  
  # Create plots based on method parameter
  plots <- list()
  
  if (method %in% c("both", "builtin")) {
    # Method 1: Using valmeta's built-in plot method
    plots$builtin <- plot(meta_cstat, 
                         title = "")
    
    cat("=== Built-in plot created ===\n")
  }
  
  if (method %in% c("both", "custom")) {
    # Method 2: Using metamisc::forest for more customization
    plots$custom <- metamisc::forest(
      theta = cstat_results$theta,
      theta.ci.lb = cstat_results$theta.cilb,
      theta.ci.ub = cstat_results$theta.ciub,
      theta.slab = model_data$author_year,
      theta.summary = meta_cstat$est,
      theta.summary.ci.lb = meta_cstat$ci.lb,
      theta.summary.ci.ub = meta_cstat$ci.ub,
      theta.summary.pi.lb = meta_cstat$pi.lb,
      theta.summary.pi.ub = meta_cstat$pi.ub,
      
      title = "",
      xlab = "C-statistic (95% Confidence Interval)",
      xlim = c(0, 1.0),
      xlim.truncate = FALSE,
      
      label.summary = "Random Effects Pooled Estimate (REML)",
      label.predint = "95% Prediction Interval",
      predint.linetype = 1,
      
      sort = "asc",
      study.digits = 3,
      nrows.before.summary = 1,
      
      # Visual customization
      col.diamond = "blue",
      col.predint = "darkred",
      size.study = 1.0,
      size.predint = 1.5,
      study.shape = 18,  # Diamond shape
      lty.ref = "longdash",
      
      # ggplot2 theme
      theme = theme_bw() + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 14)
      )
    )
    
    cat("=== Custom forest plot created ===\n")
  }
  
  # Return results
  result <- list(
    model_name = model_name,
    data = model_data,
    cstat_results = cstat_results,
    meta_analysis = meta_cstat,
    plots = plots
  )
  
  return(result)
}

# Define your models list
models_list <- c("qSOFA", "Original Shapiro", "SIRS", "5MPB-Toledo", "EsVan", 
                "EsVan 3a", "EsVan 3b", "Boerman2022", "EsVan 2b", "Sepsis finder I", 
                "Bates 1990", "Chang_2023_LGBM", "Chang_2023_catboost", "ED model", 
                "EsVan 2a", "ID-bactER", "Lee 2014", "Leibovici 1991", "Lien_2022_LR", 
                "Lien_2022_RF", "NEWS", "SOFA", "Yale observation scale", "inpatient model")

# Single model usage:
result <- create_forest_plot("qSOFA", meta_analysis_data)
print(result$plots$custom)
result <- create_forest_plot("Original Shapiro", meta_analysis_data)
result <- create_forest_plot("SIRS", meta_analysis_data)
result <- create_forest_plot("5MPB-Toledo", meta_analysis_data)
result <- create_forest_plot("EsVan", meta_analysis_data)
result <- create_forest_plot("EsVan 3a", meta_analysis_data)
result <- create_forest_plot("EsVan 3b", meta_analysis_data)
result <- create_forest_plot("Boerman2022", meta_analysis_data)
result <- create_forest_plot("EsVan 2b", meta_analysis_data)
result <- create_forest_plot("SepsisFinder", meta_analysis_data)
result <- create_forest_plot("Bates 1990", meta_analysis_data)
result <- create_forest_plot("Chang_2023_LGBM", meta_analysis_data)
result <- create_forest_plot("Chang_2023_catboost", meta_analysis_data)
result <- create_forest_plot("ED model", meta_analysis_data)
result <- create_forest_plot("inpatient model", meta_analysis_data)
result <- create_forest_plot("EsVan 2a", meta_analysis_data)
result <- create_forest_plot("ID-bactER", meta_analysis_data)
result <- create_forest_plot("Lee 2014", meta_analysis_data)
result <- create_forest_plot("Leibovici 1991", meta_analysis_data)
#result <- create_forest_plot("Lien_2022_LR", meta_analysis_data)
#result <- create_forest_plot("Lien_2022_RF", meta_analysis_data)
result <- create_forest_plot("NEWS", meta_analysis_data)
result <- create_forest_plot("SOFA", meta_analysis_data)
result <- create_forest_plot("Yale observation scale", meta_analysis_data)

# 保存先フォルダの設定
output_folder <- "forest_plots"

# 統一されたサイズでforest plotを保存する関数（フォルダ指定版）
save_forest_plot_to_folder <- function(model_name, meta_analysis_data, output_folder,
                                      base_height = 2, height_per_study = 0.4, 
                                      width = 10, dpi = 300) {
  
  # Forest plotを作成
  result <- create_forest_plot(model_name, meta_analysis_data)
  
  if (is.null(result)) {
    warning(paste("Could not create plot for model:", model_name))
    return(NULL)
  }
  
  # 研究数を取得
  n_studies <- nrow(result$data)
  
  # 高さを計算（基本の高さ + 研究数 × 研究あたりの高さ）
  plot_height <- base_height + (n_studies * height_per_study)
  
  # ファイル名を作成（特殊文字をアンダースコアに変換）
  safe_name <- gsub("[^A-Za-z0-9]", "_", model_name)
  filename <- file.path(output_folder, paste0("forest_plot_", safe_name, ".png"))
  
  # プロットを保存
  ggsave(
    filename = filename,
    plot = result$plots$custom,
    width = width,
    height = plot_height,
    dpi = dpi,
    units = "in"
  )
  
  cat("Plot saved as:", filename, "\n")
  cat("Dimensions: width =", width, "in, height =", plot_height, "in\n")
  cat("Number of studies:", n_studies, "\n\n")
  
  return(result)
}

# 対象のモデル名リスト
model_names <- c("qSOFA", "Original Shapiro", "SIRS", "5MPB-Toledo", "EsVan", 
                "EsVan 3a", "EsVan 3b", "Boerman2022", "EsVan 2b", "Sepsis finder I", 
                "Bates 1990", "Chang_2023_LGBM", "Chang_2023_catboost", "ED model", 
                "EsVan 2a", "ID-bactER", "Lee 2014", "Leibovici 1991", "NEWS", "SOFA", 
                "Yale observation scale", "inpatient model")
length(model_names)
# 全てのモデルのforest plotを保存
cat("Starting forest plot generation...\n")
cat("Output folder:", output_folder, "\n\n")
save_forest_plot_to_folder("Sepsis finder I", meta_analysis_data, output_folder)

results <- list()
for (model in model_names) {
  cat("Processing model:", model, "\n")
  results[[model]] <- save_forest_plot_to_folder(model, meta_analysis_data, output_folder)
}

cat("All forest plots have been saved to:", output_folder, "\n")
# All models usage:
# all_results <- create_all_forest_plots(models_list, meta_analysis_data)

# Access specific results:
# qsofa_plot <- all_results[["qSOFA"]]$plots$custom
# print(qsofa_plot)

# Function to create O/E ratio forest plot for any model
create_oe_forest_plot <- function(model_name, meta_analysis_data, method = "both") {
  
  # Filter and prepare data for the specified model
  model_data <- meta_analysis_data %>%
    filter(model_name == !!model_name)
  
  # Check if data exists for this model
  if (nrow(model_data) == 0) {
    warning(paste("No O/E data found for model:", model_name))
    return(NULL)
  }
  
  # Print data summary
  cat("=== O/E Data summary for", model_name, "===\n")
  print(model_data)
  cat("Number of studies:", nrow(model_data), "\n\n")
  
  # Calculate O/E statistics using oecalc
  oe_results <- oecalc(
    OE = model_data$oe_ratio,
    OE.cilb = model_data$ll_oe_ratio,
    OE.ciub = model_data$ul_oe_ratio,
    citl = model_data$citl,
    N = model_data$n_episodes,
    O = model_data$n_tb,
    E = model_data$e_event,
    slab = model_data$author_year,
    level = 0.95
  )
  
  # Display the processed results
  cat("=== Processed O/E results ===\n")
  print(oe_results)
  cat("\n")
  
  # Perform meta-analysis
  meta_oe <- valmeta(
    measure = "OE",
    OE = oe_results$theta,
    OE.se = oe_results$theta.se,
    N = model_data$n_episodes,
    slab = model_data$author_year,
    method = "REML"
  )
  
  # Print meta-analysis summary
  cat("=== Meta-analysis results ===\n")
  print(meta_oe)
  cat("\n")
  
  # Create plots based on method parameter
  plots <- list()
  
  if (method %in% c("both", "builtin")) {
    # Method 1: Using valmeta's built-in plot method
    plots$builtin <- plot(meta_oe, 
                         title = "")
    
    cat("=== Built-in plot created ===\n")
  }
  
  if (method %in% c("both", "custom")) {
    # Method 2: Using metamisc::forest for more customization
    plots$custom <- metamisc::forest(
      theta = oe_results$theta,
      theta.ci.lb = oe_results$theta.cilb,
      theta.ci.ub = oe_results$theta.ciub,
      theta.slab = model_data$author_year,
      theta.summary = meta_oe$est,
      theta.summary.ci.lb = meta_oe$ci.lb,
      theta.summary.ci.ub = meta_oe$ci.ub,
      theta.summary.pi.lb = meta_oe$pi.lb,
      theta.summary.pi.ub = meta_oe$pi.ub,
      
      title = "",
      xlab = "O:E ratio (95% Confidence Interval)",
      #xlim = c(0.3, 3),
      xlim = c(0.1, 10),
      xlim.truncate = FALSE,
      
      label.summary = "Random Effects Pooled Estimate (REML)",
      label.predint = "95% Prediction Interval",
      predint.linetype = 1,
      
      sort = "asc",
      study.digits = 3,
      nrows.before.summary = 1,
      
      # Visual customization
      col.diamond = "blue",
      col.predint = "darkred",
      size.study = 1.0,
      size.predint = 1.5,
      study.shape = 18,  # Diamond shape
      lty.ref = "longdash",
      
      # ggplot2 theme
      theme = theme_bw() + theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 14),
        text = element_text(size = 14)
      )
    )
    
    cat("=== Custom O/E forest plot created ===\n")
  }
  
  # Return results
  result <- list(
    model_name = model_name,
    data = model_data,
    oe_results = oe_results,
    meta_analysis = meta_oe,
    plots = plots
  )
  
  return(result)
}

# Example usage:
# Single model O/E forest plot:
oe_result <- create_oe_forest_plot("5MPB-Toledo", meta_analysis_data)
print(oe_result$plots$custom)
oe_result <- create_oe_forest_plot("EsVan", meta_analysis_data)
oe_result <- create_oe_forest_plot("EsVan 3a", meta_analysis_data)
oe_result <- create_oe_forest_plot("EsVan 3b", meta_analysis_data)
#oe_result <- create_oe_forest_plot("Boerman 2022", meta_analysis_data)
oe_result <- create_oe_forest_plot("EsVan 2b", meta_analysis_data)
oe_result <- create_oe_forest_plot("ID-bactER", meta_analysis_data)

# Function to create O/E forest plots for all models
output_folder <- "your/output/folder/path"  # Set your output folder path here

save_oe_forest_plot_to_folder <- function(model_name, meta_analysis_data, output_folder,
                                      base_height = 2, height_per_study = 0.4, 
                                      width = 10, dpi = 300) {
  
  # Forest plotを作成
  result <- create_oe_forest_plot(model_name, meta_analysis_data)
  
  if (is.null(result)) {
    warning(paste("Could not create plot for model:", model_name))
    return(NULL)
  }
  
  # 研究数を取得
  n_studies <- nrow(result$data)
  
  # 高さを計算（基本の高さ + 研究数 × 研究あたりの高さ）
  plot_height <- base_height + (n_studies * height_per_study)
  
  # ファイル名を作成（特殊文字をアンダースコアに変換）
  safe_name <- gsub("[^A-Za-z0-9]", "_", model_name)
  filename <- file.path(output_folder, paste0("forest_plot_", safe_name, ".png"))
  
  # プロットを保存
  ggsave(
    filename = filename,
    plot = result$plots$custom,
    width = width,
    height = plot_height,
    dpi = dpi,
    units = "in"
  )
  
  cat("Plot saved as:", filename, "\n")
  cat("Dimensions: width =", width, "in, height =", plot_height, "in\n")
  cat("Number of studies:", n_studies, "\n\n")
  
  return(result)
}

# 対象のモデル名リスト
model_names <- c("5MPB-Toledo","EsVan", "EsVan 3a", "EsVan 3b","ID-bactER")
# 対象モデルのO/E forest plotを保存
results <- list()
for (model in model_names) {
  cat("Processing model:", model, "\n")
  results[[model]] <- save_oe_forest_plot_to_folder(model, meta_analysis_data, output_folder)
}
#xlim設定変えた後で実施
save_oe_forest_plot_to_folder("EsVan 2b", meta_analysis_data, output_folder)

#Results地の文で使用
internal_validation$predictor_other %>% table()
external_validation$model_name %>% 
  table() %>% 
  sort(decreasing = TRUE) %>% 
  as.data.frame() %>%
  View()
n_distinct(external_validation$model_name)
