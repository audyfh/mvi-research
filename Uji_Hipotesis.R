# ==============================================================================
# UJI HIPOTESIS STATISTIK INFERENSIAL (INDEPENDENT SAMPLES) DAN EFFECT SIZE
# Judul: ANALISIS KOMPARATIF PERFORMA DIRECT STATE UPDATE MVI DAN 
#        REDUCER-BASED MVI PADA APLIKASI ANDROID BERBASIS JETPACK COMPOSE
# Peneliti: Audy Fitrah Habibie (NIM: 235150400111018)
# Desain: Dua Kelompok Pengukuran Independen (N1 = 30, N2 = 30, Total N = 60)
# ==============================================================================

# 1. Pustaka
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

cat("--> Memuat data bersih (output/clean_metrics.csv)...\n")
if (!file.exists("output/clean_metrics.csv")) {
  stop("File output/clean_metrics.csv tidak ditemukan. Jalankan Analisis.R terlebih dahulu.")
}

df_all <- read.csv("output/clean_metrics.csv", stringsAsFactors = FALSE)

# Pisahkan per skenario dan arsitektur (N = 30 per kelompok)
mp_direct  <- df_all %>% filter(app == "Marketplace", architecture == "Direct MVI") %>% arrange(iteration)
mp_reducer <- df_all %>% filter(app == "Marketplace", architecture == "Reducer MVI") %>% arrange(iteration)
sm_direct  <- df_all %>% filter(app == "Sosial Media", architecture == "Direct MVI") %>% arrange(iteration)
sm_reducer <- df_all %>% filter(app == "Sosial Media", architecture == "Reducer MVI") %>% arrange(iteration)

# Helper untuk interpretasi Cohen's d (Cohen, 1988)
interpret_cohens_d <- function(d) {
  abs_d <- abs(d)
  if (abs_d < 0.2) return("Negligible (< 0.2)")
  if (abs_d < 0.5) return("Small (0.2 - 0.5)")
  if (abs_d < 0.8) return("Medium (0.5 - 0.8)")
  return("Large (>= 0.8)")
}

# Helper untuk interpretasi Mann-Whitney Effect Size r
interpret_mw_r <- function(r) {
  abs_r <- abs(r)
  if (abs_r < 0.1) return("Negligible (< 0.1)")
  if (abs_r < 0.3) return("Small (0.1 - 0.3)")
  if (abs_r < 0.5) return("Medium (0.3 - 0.5)")
  return("Large (>= 0.5)")
}

# 2. Fungsi Eksekusi Independent Sample t-Test Lengkap (Equal Variance)
run_independent_ttest <- function(direct_vec, reducer_vec, app_name, metric_name, var_name, hypothesis_code) {
  n1 <- length(direct_vec)
  n2 <- length(reducer_vec)
  mean_direct  <- mean(direct_vec)
  mean_reducer <- mean(reducer_vec)
  mean_diff    <- mean_direct - mean_reducer
  var1 <- var(direct_vec)
  var2 <- var(reducer_vec)
  
  # Cek kesamaan varians via F-test
  ft <- var.test(direct_vec, reducer_vec)
  is_equal_var <- (ft$p.value >= 0.05)
  
  # Independent Sample t-Test
  t_res <- t.test(direct_vec, reducer_vec, paired = FALSE, var.equal = is_equal_var)
  t_stat  <- as.numeric(t_res$statistic)
  df_val  <- as.numeric(t_res$parameter)
  p_val   <- as.numeric(t_res$p.value)
  ci_low  <- t_res$conf.int[1]
  ci_high <- t_res$conf.int[2]
  
  # Pooled Standard Deviation & Pooled Cohen's d
  s_pooled <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
  cohens_d <- (mean_direct - mean_reducer) / s_pooled
  effect_mag <- interpret_cohens_d(cohens_d)
  
  # Persentase selisih relatif: (Reducer - Direct) / Direct * 100%
  rel_pct <- ((mean_reducer - mean_direct) / mean_direct) * 100
  
  data.frame(
    Hypothesis = hypothesis_code,
    App = app_name,
    Metric = metric_name,
    Metric_Var = var_name,
    Test_Type = ifelse(is_equal_var, "Independent Sample t-Test (Equal Var)", "Welch's Independent t-Test"),
    Sample_Size = paste0(n1, " vs ", n2),
    Mean_Direct = round(mean_direct, 4),
    Mean_Reducer = round(mean_reducer, 4),
    Mean_Diff = round(mean_diff, 4),
    Rel_Diff_Pct = round(rel_pct, 2),
    Test_Statistic = paste0("t(", round(df_val, 1), ") = ", round(t_stat, 4)),
    Statistic_Val = round(t_stat, 4),
    p_value = round(p_val, 6),
    p_formatted = ifelse(p_val < 0.001, "< 0.001", sprintf("%.4f", p_val)),
    CI_95 = paste0("[", round(ci_low, 4), ", ", round(ci_high, 4), "]"),
    Effect_Size_Val = round(cohens_d, 4),
    Effect_Size_Type = "Pooled Cohen's d",
    Effect_Magnitude = effect_mag,
    Conclusion = ifelse(p_val < 0.05, "Signifikan (p < 0.05)", "Tidak Signifikan (p >= 0.05)"),
    stringsAsFactors = FALSE
  )
}

# 3. Fungsi Eksekusi Mann-Whitney U Test (Wilcoxon Rank-Sum Test) Lengkap
run_mann_whitney_test <- function(direct_vec, reducer_vec, app_name, metric_name, var_name, hypothesis_code) {
  n1 <- length(direct_vec)
  n2 <- length(reducer_vec)
  n_total <- n1 + n2
  
  mean_direct    <- mean(direct_vec)
  mean_reducer   <- mean(reducer_vec)
  mean_diff      <- mean_direct - mean_reducer
  median_direct  <- median(direct_vec)
  median_reducer <- median(reducer_vec)
  median_diff    <- median_direct - median_reducer
  
  # Mann-Whitney U Test (paired = FALSE)
  mw_res <- wilcox.test(direct_vec, reducer_vec, paired = FALSE, exact = FALSE, correct = TRUE)
  u_stat <- as.numeric(mw_res$statistic)
  p_val  <- as.numeric(mw_res$p.value)
  
  # Nilai Ekspektasi & Standar Deviasi U untuk pendekatan normal Z
  # E[U] = (n1 * n2) / 2 ; SD[U] = sqrt(n1 * n2 * (n1 + n2 + 1) / 12)
  e_u  <- (n1 * n2) / 2
  sd_u <- sqrt((n1 * n2 * (n_total + 1)) / 12)
  z_stat <- (u_stat - e_u) / sd_u
  
  # Effect Size r = |Z| / sqrt(N_total) dan Rank-Biserial Correlation r_rb = 2U/(n1*n2) - 1
  mw_r <- z_stat / sqrt(n_total)
  r_rb <- (2 * u_stat / (n1 * n2)) - 1
  effect_mag <- interpret_mw_r(mw_r)
  
  rel_pct <- ((mean_reducer - mean_direct) / mean_direct) * 100
  
  data.frame(
    Hypothesis = hypothesis_code,
    App = app_name,
    Metric = metric_name,
    Metric_Var = var_name,
    Test_Type = "Mann-Whitney U Test",
    Sample_Size = paste0(n1, " vs ", n2),
    Mean_Direct = round(mean_direct, 4),
    Mean_Reducer = round(mean_reducer, 4),
    Mean_Diff = round(mean_diff, 4),
    Rel_Diff_Pct = round(rel_pct, 2),
    Test_Statistic = paste0("U = ", round(u_stat, 1), " (Z = ", round(z_stat, 4), ")"),
    Statistic_Val = round(u_stat, 4),
    p_value = round(p_val, 6),
    p_formatted = ifelse(p_val < 0.001, "< 0.001", sprintf("%.4f", p_val)),
    CI_95 = paste0("Median Direct: ", round(median_direct, 4), " vs Reducer: ", round(median_reducer, 4)),
    Effect_Size_Val = round(mw_r, 4),
    Effect_Size_Type = paste0("Mann-Whitney r (r_rb = ", round(r_rb, 4), ")"),
    Effect_Magnitude = effect_mag,
    Conclusion = ifelse(p_val < 0.05, "Signifikan (p < 0.05)", "Tidak Signifikan (p >= 0.05)"),
    stringsAsFactors = FALSE
  )
}

# 4. Pelaksanaan Pengujian Hipotesis
cat("--> Menjalankan Pengujian Hipotesis Inferensial (Independent Samples)...\n")

test_results <- list()

# ==================== MARKETPLACE (Normal & Varians Homogen -> Independent t-Test) ====================
# H1: Responsivitas P50 (Utama)
test_results[[1]] <- run_independent_ttest(
  mp_direct$responsiveness_p50, mp_reducer$responsiveness_p50,
  "Marketplace", "Responsivitas P50 (ms)", "responsiveness_p50", "H1"
)

# H1: Responsivitas P95 (Beban Tinggi)
test_results[[2]] <- run_independent_ttest(
  mp_direct$responsiveness_p95, mp_reducer$responsiveness_p95,
  "Marketplace", "Responsivitas P95 (ms)", "responsiveness_p95", "H1"
)

# H2: Penggunaan CPU (%)
test_results[[3]] <- run_independent_ttest(
  mp_direct$cpu_usage, mp_reducer$cpu_usage,
  "Marketplace", "Penggunaan CPU (%)", "cpu_usage", "H2"
)

# Dekomposisi H1: T_logic (ms/intent)
test_results[[4]] <- run_independent_ttest(
  mp_direct$t_logic_avg, mp_reducer$t_logic_avg,
  "Marketplace", "T_logic (ms/intent)", "t_logic_avg", "Dekomposisi H1"
)

# Dekomposisi H1: T_frame P50 (ms)
test_results[[5]] <- run_independent_ttest(
  mp_direct$t_frame_p50, mp_reducer$t_frame_p50,
  "Marketplace", "T_frame P50 (ms)", "t_frame_p50", "Dekomposisi H1"
)

# ==================== SOSIAL MEDIA (Tidak Normal -> Mann-Whitney U Test) ====================
# H1: Responsivitas P50 (Utama)
test_results[[6]] <- run_mann_whitney_test(
  sm_direct$responsiveness_p50, sm_reducer$responsiveness_p50,
  "Sosial Media", "Responsivitas P50 (ms)", "responsiveness_p50", "H1"
)

# H1: Responsivitas P95 (Beban Tinggi)
test_results[[7]] <- run_mann_whitney_test(
  sm_direct$responsiveness_p95, sm_reducer$responsiveness_p95,
  "Sosial Media", "Responsivitas P95 (ms)", "responsiveness_p95", "H1"
)

# H2: Penggunaan CPU (%)
test_results[[8]] <- run_mann_whitney_test(
  sm_direct$cpu_usage, sm_reducer$cpu_usage,
  "Sosial Media", "Penggunaan CPU (%)", "cpu_usage", "H2"
)

# Dekomposisi H1: T_logic (ms/intent)
test_results[[9]] <- run_mann_whitney_test(
  sm_direct$t_logic_avg, sm_reducer$t_logic_avg,
  "Sosial Media", "T_logic (ms/intent)", "t_logic_avg", "Dekomposisi H1"
)

# Dekomposisi H1: T_frame P50 (ms)
test_results[[10]] <- run_mann_whitney_test(
  sm_direct$t_frame_p50, sm_reducer$t_frame_p50,
  "Sosial Media", "T_frame P50 (ms)", "t_frame_p50", "Dekomposisi H1"
)

# ==================== ROBUSTNESS CHECK SOSIAL MEDIA (Independent t-Test) ====================
test_results[[11]] <- run_independent_ttest(
  sm_direct$responsiveness_p50, sm_reducer$responsiveness_p50,
  "Sosial Media", "Responsivitas P50 (ms) [Robustness Check]", "responsiveness_p50_check", "H1 (Check)"
)

test_results[[12]] <- run_independent_ttest(
  sm_direct$cpu_usage, sm_reducer$cpu_usage,
  "Sosial Media", "Penggunaan CPU (%) [Robustness Check]", "cpu_usage_check", "H2 (Check)"
)

df_hyp_results <- bind_rows(test_results)

# Simpan ke CSV
write.csv(df_hyp_results, "output/hypothesis_test_results.csv", row.names = FALSE)
cat("    Tersimpan: output/hypothesis_test_results.csv\n\n")

# 5. Visualisasi Publikasi / Bab IV Skripsi
cat("--> Menghasilkan Grafik Publikasi Komparatif...\n")

color_palette <- c("Direct MVI" = "#1976D2", "Reducer MVI" = "#E64A19")

# 5a. Bar Chart Komparasi Responsivitas P50 (Mean ± 95% CI)
resp_summary <- df_all %>%
  group_by(app, architecture) %>%
  summarise(
    Mean_P50 = mean(responsiveness_p50),
    SE_P50 = sd(responsiveness_p50) / sqrt(n()),
    CI_P50 = qt(0.975, df = n() - 1) * SE_P50,
    .groups = "drop"
  )

p_resp_p50 <- ggplot(resp_summary, aes(x = app, y = Mean_P50, fill = architecture)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Mean_P50 - CI_P50, ymax = Mean_P50 + CI_P50),
                position = position_dodge(0.8), width = 0.25, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f ms", Mean_P50)),
            position = position_dodge(0.8), vjust = -1.2, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = color_palette) +
  theme_bw(base_size = 12) +
  labs(
    title = "Perbandingan Responsivitas P50 (Kondisi Tipikal)",
    subtitle = "Nilai Rata-rata ± 95% Confidence Interval (N = 30 per kelompok independen)",
    x = "Skenario Aplikasi",
    y = "Durasi Latensi (ms)",
    fill = "Arsitektur MVI"
  ) +
  coord_cartesian(ylim = c(0, max(resp_summary$Mean_P50) * 1.25)) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("output/figures/comparative_responsiveness_p50.png", p_resp_p50, width = 7.5, height = 5.5, dpi = 300)

# 5b. Bar Chart Komparasi Penggunaan CPU (Mean ± 95% CI)
cpu_summary <- df_all %>%
  group_by(app, architecture) %>%
  summarise(
    Mean_CPU = mean(cpu_usage),
    SE_CPU = sd(cpu_usage) / sqrt(n()),
    CI_CPU = qt(0.975, df = n() - 1) * SE_CPU,
    .groups = "drop"
  )

p_cpu <- ggplot(cpu_summary, aes(x = app, y = Mean_CPU, fill = architecture)) +
  geom_bar(stat = "identity", position = position_dodge(0.8), width = 0.7, color = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Mean_CPU - CI_CPU, ymax = Mean_CPU + CI_CPU),
                position = position_dodge(0.8), width = 0.25, linewidth = 0.7) +
  geom_text(aes(label = sprintf("%.2f%%", Mean_CPU)),
            position = position_dodge(0.8), vjust = -1.2, size = 3.5, fontface = "bold") +
  scale_fill_manual(values = color_palette) +
  theme_bw(base_size = 12) +
  labs(
    title = "Perbandingan Penggunaan CPU (%)",
    subtitle = "Nilai Rata-rata ± 95% Confidence Interval (N = 30 per kelompok independen)",
    x = "Skenario Aplikasi",
    y = "Penggunaan CPU (%)",
    fill = "Arsitektur MVI"
  ) +
  coord_cartesian(ylim = c(0, max(cpu_summary$Mean_CPU) * 1.25)) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

ggsave("output/figures/comparative_cpu_usage.png", p_cpu, width = 7.5, height = 5.5, dpi = 300)

# 5c. Boxplot Distribusi Lengkap Responsivitas & CPU
p_box_resp <- ggplot(df_all, aes(x = architecture, y = responsiveness_p50, fill = architecture)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  facet_wrap(~ app, scales = "free_y") +
  scale_fill_manual(values = color_palette) +
  theme_bw(base_size = 12) +
  labs(
    title = "Distribusi Boxplot: Responsivitas P50 (ms)",
    subtitle = "Penyebaran Data 30 Iterasi Independen per Pendekatan",
    x = "Arsitektur MVI",
    y = "Responsivitas P50 (ms)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave("output/figures/boxplot_responsiveness.png", p_box_resp, width = 8, height = 5, dpi = 300)

p_box_cpu <- ggplot(df_all, aes(x = architecture, y = cpu_usage, fill = architecture)) +
  geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.fill = "red") +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  facet_wrap(~ app, scales = "free_y") +
  scale_fill_manual(values = color_palette) +
  theme_bw(base_size = 12) +
  labs(
    title = "Distribusi Boxplot: Penggunaan CPU (%)",
    subtitle = "Penyebaran Data 30 Iterasi Independen per Pendekatan",
    x = "Arsitektur MVI",
    y = "Penggunaan CPU (%)"
  ) +
  theme(legend.position = "none", plot.title = element_text(face = "bold"))

ggsave("output/figures/boxplot_cpu_usage.png", p_box_cpu, width = 8, height = 5, dpi = 300)

cat("    Tersimpan seluruh grafik publikasi di: output/figures/\n\n")

# 6. Cetak Ringkasan Hasil Pengujian ke Konsol
cat("=================================================================================================================\n")
cat("                          RINGKASAN HASIL PENGUJIAN HIPOTESIS (INDEPENDENT SAMPLES)                              \n")
cat("=================================================================================================================\n")
print_df <- df_hyp_results %>%
  select(Hypothesis, App, Metric, Test_Type, Mean_Direct, Mean_Reducer, Rel_Diff_Pct, Test_Statistic, p_formatted, Effect_Size_Val, Effect_Magnitude, Conclusion)
print(as.data.frame(print_df), row.names = FALSE)
cat("=================================================================================================================\n")
cat("Pengujian hipotesis inferensial independen selesai dengan sukses!\n")
