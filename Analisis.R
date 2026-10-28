# ==============================================================================
# ANALISIS DATA DAN UJI NORMALITAS SKRIPSI
# Judul: ANALISIS KOMPARATIF PERFORMA DIRECT STATE UPDATE MVI DAN 
#        REDUCER-BASED MVI PADA APLIKASI ANDROID BERBASIS JETPACK COMPOSE
# Peneliti: Audy Fitrah Habibie (NIM: 235150400111018)
# ==============================================================================

# 1. Inisialisasi Pustaka
suppressPackageStartupMessages({
  library(jsonlite)
  library(dplyr)
  library(ggplot2)
})

# Pastikan direktori output tersedia
dir.create("output", showWarnings = FALSE)
dir.create("output/figures", showWarnings = FALSE)

# 2. Fungsi Ekstraksi Data dari JSON Macrobenchmark
extract_macrobenchmark_data <- function(file_path, app_name, arch_name) {
  if (!file.exists(file_path)) {
    stop(paste("File tidak ditemukan:", file_path))
  }
  
  raw_data <- fromJSON(file_path, simplifyVector = FALSE)
  bm <- raw_data$benchmarks[[1]]
  metrics <- bm$metrics
  sampled <- bm$sampledMetrics
  
  # Ekstraksi CPU Usage (30 runs)
  cpu_runs <- unlist(metrics$cpuUsagePercent$runs)
  n_runs <- length(cpu_runs)
  
  # Cari nama key untuk ProcessIntent
  metric_keys <- names(metrics)
  sum_key <- metric_keys[grepl("ProcessIntentSumMs$", metric_keys)]
  cnt_key <- metric_keys[grepl("ProcessIntentCount$", metric_keys)]
  
  if (length(sum_key) == 0 || length(cnt_key) == 0) {
    stop("Metrik ProcessIntentSumMs atau ProcessIntentCount tidak ditemukan pada ", file_path)
  }
  
  intent_sum_runs <- unlist(metrics[[sum_key]]$runs)
  intent_cnt_runs <- unlist(metrics[[cnt_key]]$runs)
  
  # Logika State: durasi rata-rata per intent (ms)
  t_logic_avg <- intent_sum_runs / intent_cnt_runs
  
  # Ekstraksi frame duration CPU dari sampledMetrics (30 runs)
  frame_runs <- sampled$frameDurationCpuMs$runs
  
  t_frame_p50 <- numeric(n_runs)
  t_frame_p95 <- numeric(n_runs)
  t_frame_mean <- numeric(n_runs)
  
  for (i in seq_len(n_runs)) {
    f_vals <- as.numeric(frame_runs[[i]])
    t_frame_p50[i] <- as.numeric(quantile(f_vals, probs = 0.50, names = FALSE))
    t_frame_p95[i] <- as.numeric(quantile(f_vals, probs = 0.95, names = FALSE))
    t_frame_mean[i] <- mean(f_vals)
  }
  
  # Responsivitas = T_logic + T_frame
  responsiveness_p50 <- t_logic_avg + t_frame_p50
  responsiveness_p95 <- t_logic_avg + t_frame_p95
  responsiveness_mean <- t_logic_avg + t_frame_mean
  
  data.frame(
    iteration = 1:n_runs,
    app = app_name,
    architecture = arch_name,
    cpu_usage = cpu_runs,
    t_logic_avg = t_logic_avg,
    t_logic_sum = intent_sum_runs,
    intent_count = intent_cnt_runs,
    t_frame_p50 = t_frame_p50,
    t_frame_p95 = t_frame_p95,
    t_frame_mean = t_frame_mean,
    responsiveness_p50 = responsiveness_p50,
    responsiveness_p95 = responsiveness_p95,
    responsiveness_mean = responsiveness_mean,
    stringsAsFactors = FALSE
  )
}

# 3. Muat Semua Berkas Data
cat("--> Membaca data Macrobenchmark...\n")
df_mp_direct  <- extract_macrobenchmark_data("data/marketplace_direct.json", "Marketplace", "Direct MVI")
df_mp_reducer <- extract_macrobenchmark_data("data/marketplace_reducer.json", "Marketplace", "Reducer MVI")
df_sm_direct  <- extract_macrobenchmark_data("data/socmed_direct.json", "Sosial Media", "Direct MVI")
df_sm_reducer <- extract_macrobenchmark_data("data/socmed_reducer.json", "Sosial Media", "Reducer MVI")

df_all <- bind_rows(df_mp_direct, df_mp_reducer, df_sm_direct, df_sm_reducer)

# Simpan dataset bersih
write.csv(df_all, "output/clean_metrics.csv", row.names = FALSE)
cat("    Tersimpan: output/clean_metrics.csv\n\n")

# 4. Analisis Statistik Deskriptif (Proposal Bab 3.6)
cat("--> Menghitung Statistik Deskriptif...\n")
desc_stats <- df_all %>%
  group_by(app, architecture) %>%
  summarise(
    N = n(),
    # CPU Usage
    cpu_mean = mean(cpu_usage),
    cpu_sd   = sd(cpu_usage),
    cpu_median = median(cpu_usage),
    cpu_min  = min(cpu_usage),
    cpu_max  = max(cpu_usage),
    cpu_p95  = quantile(cpu_usage, 0.95),
    
    # Responsiveness P50
    resp_p50_mean = mean(responsiveness_p50),
    resp_p50_sd   = sd(responsiveness_p50),
    resp_p50_median = median(responsiveness_p50),
    resp_p50_min  = min(responsiveness_p50),
    resp_p50_max  = max(responsiveness_p50),
    
    # Responsiveness P95
    resp_p95_mean = mean(responsiveness_p95),
    resp_p95_sd   = sd(responsiveness_p95),
    resp_p95_median = median(responsiveness_p95),
    resp_p95_min  = min(responsiveness_p95),
    resp_p95_max  = max(responsiveness_p95),
    
    # T_logic avg
    t_logic_mean = mean(t_logic_avg),
    t_logic_sd   = sd(t_logic_avg),
    t_logic_median = median(t_logic_avg),
    
    # T_frame P50
    t_frame_p50_mean = mean(t_frame_p50),
    t_frame_p50_sd   = sd(t_frame_p50),
    t_frame_p50_median = median(t_frame_p50),
    
    .groups = "drop"
  )

write.csv(desc_stats, "output/descriptive_statistics.csv", row.names = FALSE)
cat("    Tersimpan: output/descriptive_statistics.csv\n\n")

# 5. Uji Normalitas Shapiro-Wilk: Kelompok Mandiri (Per App & Arch)
cat("--> Melakukan Uji Normalitas Shapiro-Wilk (Kelompok Mandiri)...\n")
metrics_to_test <- c(
  "cpu_usage", 
  "responsiveness_p50", 
  "responsiveness_p95", 
  "t_logic_avg", 
  "t_frame_p50"
)

metric_labels <- c(
  "cpu_usage" = "Penggunaan CPU (%)",
  "responsiveness_p50" = "Responsivitas P50 (ms)",
  "responsiveness_p95" = "Responsivitas P95 (ms)",
  "t_logic_avg" = "T_logic (ms/intent)",
  "t_frame_p50" = "T_frame P50 (ms)"
)

normality_groups_list <- list()

for (a in unique(df_all$app)) {
  for (arch in unique(df_all$architecture)) {
    sub_df <- df_all %>% filter(app == a, architecture == arch)
    for (m in metrics_to_test) {
      vals <- sub_df[[m]]
      st <- shapiro.test(vals)
      normality_groups_list[[length(normality_groups_list) + 1]] <- data.frame(
        App = a,
        Architecture = arch,
        Metric = metric_labels[[m]],
        Metric_Var = m,
        Sample_Size = length(vals),
        W_Statistic = round(st$statistic, 4),
        p_value = round(st$p.value, 5),
        Is_Normal = ifelse(st$p.value >= 0.05, "Normal (p >= 0.05)", "Tidak Normal (p < 0.05)"),
        stringsAsFactors = FALSE
      )
    }
  }
}

df_normality_groups <- bind_rows(normality_groups_list)
write.csv(df_normality_groups, "output/shapiro_wilk_groups.csv", row.names = FALSE)
cat("    Tersimpan: output/shapiro_wilk_groups.csv\n\n")

# 6. Uji Normalitas Shapiro-Wilk: Selisih Berpasangan (Paired Differences)
# Syarat utama Paired Sample t-Test adalah selisih d = Direct - Reducer berdistribusi normal
cat("--> Melakukan Uji Normalitas Shapiro-Wilk (Selisih Berpasangan Delta)...\n")
diff_records <- list()

test_apps <- list(
  list(app = "Marketplace", direct = df_mp_direct, reducer = df_mp_reducer),
  list(app = "Sosial Media", direct = df_sm_direct, reducer = df_sm_reducer)
)

for (item in test_apps) {
  cur_app <- item$app
  d_df <- item$direct
  r_df <- item$reducer
  
  for (m in metrics_to_test) {
    diff_vals <- d_df[[m]] - r_df[[m]]
    st <- shapiro.test(diff_vals)
    diff_records[[length(diff_records) + 1]] <- data.frame(
      App = cur_app,
      Metric = metric_labels[[m]],
      Metric_Var = m,
      Comparison = "Direct MVI - Reducer MVI",
      Sample_Size = length(diff_vals),
      Mean_Diff = round(mean(diff_vals), 4),
      SD_Diff = round(sd(diff_vals), 4),
      W_Statistic = round(st$statistic, 4),
      p_value = round(st$p.value, 5),
      Is_Normal = ifelse(st$p.value >= 0.05, "Normal (p >= 0.05)", "Tidak Normal (p < 0.05)"),
      Recommended_Test = ifelse(st$p.value >= 0.05, "Paired Sample t-Test", "Wilcoxon Signed-Rank Test"),
      stringsAsFactors = FALSE
    )
  }
}

df_normality_paired <- bind_rows(diff_records)
write.csv(df_normality_paired, "output/shapiro_wilk_paired_diffs.csv", row.names = FALSE)
cat("    Tersimpan: output/shapiro_wilk_paired_diffs.csv\n\n")

# 7. Membuat Visualisasi Diagnostik Normalitas (Q-Q Plots)
cat("--> Menghasilkan Gambar Q-Q Plot dan Distribusi...\n")

# 7a. Q-Q Plot Responsivitas P50 (Direct vs Reducer untuk Marketplace & Sosial Media)
p_qq_resp <- ggplot(df_all, aes(sample = responsiveness_p50, color = architecture)) +
  stat_qq() +
  stat_qq_line(linetype = "dashed") +
  facet_grid(app ~ architecture, scales = "free") +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plot: Responsivitas P50 (ms)",
    subtitle = "Uji Asumsi Normalitas Kelompok Mandiri (N = 30)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles (ms)"
  ) +
  theme(legend.position = "none")

ggsave("output/figures/qq_plot_responsiveness_p50.png", p_qq_resp, width = 7, height = 6, dpi = 300)

# 7b. Q-Q Plot Penggunaan CPU
p_qq_cpu <- ggplot(df_all, aes(sample = cpu_usage, color = architecture)) +
  stat_qq() +
  stat_qq_line(linetype = "dashed") +
  facet_grid(app ~ architecture, scales = "free") +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plot: Penggunaan CPU (%)",
    subtitle = "Uji Asumsi Normalitas Kelompok Mandiri (N = 30)",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles (%)"
  ) +
  theme(legend.position = "none")

ggsave("output/figures/qq_plot_cpu_usage.png", p_qq_cpu, width = 7, height = 6, dpi = 300)

# 7c. Q-Q Plot Selisih Berpasangan (Paired Differences)
diff_plot_df <- bind_rows(
  data.frame(
    App = "Marketplace",
    Metric = "Penggunaan CPU (%)",
    diff = df_mp_direct$cpu_usage - df_mp_reducer$cpu_usage
  ),
  data.frame(
    App = "Marketplace",
    Metric = "Responsivitas P50 (ms)",
    diff = df_mp_direct$responsiveness_p50 - df_mp_reducer$responsiveness_p50
  ),
  data.frame(
    App = "Sosial Media",
    Metric = "Penggunaan CPU (%)",
    diff = df_sm_direct$cpu_usage - df_sm_reducer$cpu_usage
  ),
  data.frame(
    App = "Sosial Media",
    Metric = "Responsivitas P50 (ms)",
    diff = df_sm_direct$responsiveness_p50 - df_sm_reducer$responsiveness_p50
  )
)

p_qq_diff <- ggplot(diff_plot_df, aes(sample = diff, color = App)) +
  stat_qq() +
  stat_qq_line(linetype = "dashed") +
  facet_grid(App ~ Metric, scales = "free") +
  theme_bw() +
  labs(
    title = "Normal Q-Q Plot: Selisih Berpasangan (Direct - Reducer)",
    subtitle = "Asumsi Penentu Pemilihan Paired t-Test vs Wilcoxon Signed-Rank Test",
    x = "Theoretical Quantiles",
    y = "Sample Difference Quantiles"
  ) +
  theme(legend.position = "none")

ggsave("output/figures/qq_plot_paired_differences.png", p_qq_diff, width = 8, height = 6, dpi = 300)

cat("    Tersimpan semua plot di direktori: output/figures/\n\n")

# 8. Cetak Ringkasan Hasil ke Konsol
cat("========================================================================================\n")
cat("                       RINGKASAN HASIL UJI NORMALITAS SHAPIRO-WILK                      \n")
cat("========================================================================================\n")
cat("\nA. KELOMPOK MANDIRI (Marginal Distribution):\n")
print(as.data.frame(df_normality_groups %>% select(App, Architecture, Metric, W_Statistic, p_value, Is_Normal)))

cat("\nB. SELISIH BERPASANGAN (Paired Differences Delta = Direct - Reducer):\n")
print(as.data.frame(df_normality_paired %>% select(App, Metric, Mean_Diff, W_Statistic, p_value, Is_Normal, Recommended_Test)))
cat("========================================================================================\n")
cat("Analisis selesai dengan sukses!\n")
