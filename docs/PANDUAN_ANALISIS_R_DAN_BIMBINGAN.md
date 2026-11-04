# PANDUAN LENGKAP KODE R & PERSIAPAN BIMBINGAN SKRIPSI (UJI SAMPEL INDEPENDEN)

**Judul Penelitian:** ANALISIS KOMPARATIF PERFORMA DIRECT STATE UPDATE MVI DAN REDUCER-BASED MVI PADA APLIKASI ANDROID BERBASIS JETPACK COMPOSE  
**Peneliti:** Audy Fitrah Habibie (NIM: 235150400111018)  
**Branch Git:** `independent-test` (Desain Dua Sampel Independen / Tidak Berpasangan)  
**Tujuan Dokumen:** 
1. Menjelaskan logika kode R (`Analisis.R` dan `Uji_Hipotesis.R`) secara bertahap dan mudah dipahami bagi pemula.
2. Mempersiapkan Anda menghadapi pertanyaan kritis dari dosen pembimbing/penguji saat sesi bimbingan dan sidang.

---

## DAFTAR ISI
1. [Alur Kerja Analisis Data Skripsi (Desain Independen)](#1-alur-kerja-analisis-data-skripsi-desain-independen)
2. [Bedah Kode: `Analisis.R` (Ekstraksi, Normalitas, & Homogenitas Varians)](#2-bedah-kode-analisisr-ekstraksi-normalitas--homogenitas-varians)
3. [Bedah Kode: `Uji_Hipotesis.R` (Independent t-Test, Mann-Whitney U, & Visualisasi)](#3-bedah-kode-uji_hipotesisr-independent-t-test-mann-whitney-u--visualisasi)
4. [Struktur Output Data untuk Bab IV](#4-struktur-output-data-untuk-bab-iv)
5. [Bank Pertanyaan & Jawaban Bimbingan Dosen (FAQ Skripsi)](#5-bank-pertanyaan--jawaban-bimbingan-dosen-faq-skripsi)

---

## 1. ALUR KERJA ANALISIS DATA SKRIPSI (DESAIN INDEPENDEN)

Sesuai Bab 3.2 dan Gambar 3.1 proposal Anda, pengambilan data dilakukan pada **dua aplikasi terpisah secara sekuensial** (30 iterasi *Direct MVI* selesai terlebih dahulu, kemudian dilanjutkan 30 iterasi *Reducer MVI*). Oleh karena itu, perbandingan dilakukan sebagai **Dua Kelompok Pengukuran Independen ($N_1 = 30, N_2 = 30$)**.

```
[Raw Data: 4 Berkas JSON di /data]
  │ (marketplace_direct, marketplace_reducer, socmed_direct, socmed_reducer)
  ▼
[Analisis.R]
  ├─ 1. Membaca JSON & Mengekstrak Metrik (CPU, T_logic, T_frame, Responsivitas)
  ├─ 2. Menyimpan Dataset Bersih -> output/clean_metrics.csv
  ├─ 3. Menghitung Statistik Deskriptif (Mean, SD, Varians, Median, Min, Max)
  ├─ 4. Uji Prasyarat 1: Uji Normalitas Shapiro-Wilk per Kelompok Mandiri
  ├─ 5. Uji Prasyarat 2: Uji Homogenitas Varians (F-Test & Levene's Test)
  └─ 6. Menggambar Q-Q Plot Diagnostik -> output/figures/
  ▼
[Uji_Hipotesis.R]
  ├─ 1. Memuat clean_metrics.csv
  ├─ 2. Skenario Marketplace (Kedua Kelompok Normal & Varians Homogen):
  │      └─ Diuji dengan Independent Sample t-Test (Equal Variance, df = 58)
  │         + Pooled Cohen's d
  ├─ 3. Skenario Sosial Media (Kelompok Tidak Normal):
  │      └─ Diuji dengan Mann-Whitney U Test (Wilcoxon Rank-Sum)
  │         + Effect Size r & Rank-Biserial Correlation (r_rb)
  ├─ 4. Melakukan Dekomposisi Waktu (T_logic vs T_frame)
  └─ 5. Membuat Grafik Siap Publikasi Bab IV -> output/figures/
```

---

## 2. BEDAH KODE: `Analisis.R` (EKSTRAKSI, NORMALITAS, & HOMOGENITAS VARIANS)

### A. Fungsi Ekstraksi: `extract_macrobenchmark_data()`
* Membaca berkas JSON Macrobenchmark menggunakan `jsonlite::fromJSON()`.
* **`cpu_usage`**: Mengambil 30 nilai `cpuUsagePercent$runs`.
* **`t_logic_avg`**: Menghitung rata-rata durasi pemrosesan intent di ViewModel per interaksi (`ProcessIntentSumMs / ProcessIntentCount`).
* **`t_frame_p50` & `t_frame_p95`**: Dalam 1 iterasi pengujian, layar merender ~1.370 frame. Fungsi `quantile(f_vals, probs = 0.50)` dan `0.95` mereduksi ~1.370 frame tersebut menjadi 1 angka representatif $P_{50}$ (kondisi tipikal) dan $P_{95}$ (kondisi beban tinggi) untuk iterasi ke-$i$.
* **`responsiveness_p50` & `responsiveness_p95`**: Sesuai Persamaan 2.1 Proposal, dihitung dari $T_{\text{logic}} + T_{\text{frame}}$.

---

### B. Uji Prasyarat 1: Uji Normalitas Shapiro-Wilk per Kelompok Mandiri
```r
sw_d <- shapiro.test(v_direct)
sw_r <- shapiro.test(v_reducer)
both_normal <- (sw_d$p.value >= 0.05) && (sw_r$p.value >= 0.05)
```
* Karena kedua kelompok (*Direct MVI* dan *Reducer MVI*) bersifat **independen**, uji normalitas dilakukan langsung pada masing-masing kelompok ($N=30$ untuk Direct, dan $N=30$ untuk Reducer).
* **Syarat lolos ke uji parametrik (*Independent t-Test*)**: **Kedua kelompok** harus sama-sama berdistribusi normal ($p \ge 0,05$).
* **Hasil pada Data Anda**:
  * **Marketplace**: Kelompok Direct ($p > 0,36$) dan Kelompok Reducer ($p > 0,20$) **keduanya Normal** pada seluruh metrik!
  * **Sosial Media**: Kelompok Direct ($p < 0,01$) dan Kelompok Reducer ($p < 0,02$) **keduanya Tidak Normal** pada seluruh metrik!

---

### C. Uji Prasyarat 2: Uji Homogenitas Varians (Kesamaan Varians)
Selain normalitas, **Student's Independent Sample t-Test** mensyaratkan bahwa varians kedua kelompok adalah **sama/homogen** ($\sigma_1^2 = \sigma_2^2$). Di dalam `Analisis.R`, kita mengujinya dengan dua metode sekaligus:

```r
# 1. F-Test for Equality of Two Variances (Bawaan Base R)
ft <- var.test(v_direct, v_reducer)

# 2. Levene's Test (Brown-Forsythe Median-Centered)
lt <- levene_test_median(v_direct, v_reducer)
```
* **Hipotesis Homogenitas Varians**:
  * $H_0$: Varians kedua kelompok sama/homogen ($\sigma_{\text{Direct}}^2 = \sigma_{\text{Reducer}}^2$).
  * $H_1$: Varians kedua kelompok berbeda ($\sigma_{\text{Direct}}^2 \ne \sigma_{\text{Reducer}}^2$).
* **Kriteria Keputusan ($\alpha = 0,05$)**:
  * Jika **$p\text{-value} \ge 0,05$** $\rightarrow$ Gagal tolak $H_0$ $\rightarrow$ **Varians Homogen (Sama)**.
* **Hasil pada Data Anda**:
  * Seluruh metrik pada Marketplace dan Sosial Media menghasilkan **$p > 0,05$** (baik pada F-Test maupun Levene's Test), sehingga **100% varians kedua kelompok adalah homogen/sama**!

---

## 3. BEDAH KODE: `Uji_Hipotesis.R` (INDEPENDENT T-TEST, MANN-WHITNEY U, & VISUALISASI)

### A. Uji Parametrik: `run_independent_ttest()` (Untuk Skenario Marketplace)
```r
t_res <- t.test(direct_vec, reducer_vec, paired = FALSE, var.equal = TRUE)
```
* **`paired = FALSE`**: Menandakan kedua kelompok berasal dari pengukuran aplikasi yang terpisah/independen.
* **`var.equal = TRUE`**: Menggunakan rumus *Student's t-Test* dengan varians gabungan (*pooled variance*) karena syarat homogenitas varians telah terbukti terpenuhi ($p > 0,05$).
* **Derajat Kebebasan ($df$)**: $df = n_1 + n_2 - 2 = 30 + 30 - 2 = \mathbf{58}$.

#### Perhitungan Pooled Cohen's $d$ (Effect Size Independen):
```r
s_pooled <- sqrt(((n1 - 1) * var1 + (n2 - 1) * var2) / (n1 + n2 - 2))
cohens_d <- (mean_direct - mean_reducer) / s_pooled
```
* Mengukur besar perbedaan rata-rata dibagi standar deviasi gabungan kedua kelompok.
* Kriteria Cohen (1988): $|d| < 0,2$ (*Negligible*), $0,2 - 0,5$ (*Small*), $0,5 - 0,8$ (*Medium*), $\ge 0,8$ (*Large*).

---

### B. Uji Non-Parametrik: `run_mann_whitney_test()` (Untuk Skenario Sosial Media)
```r
mw_res <- wilcox.test(direct_vec, reducer_vec, paired = FALSE, exact = FALSE, correct = TRUE)
```
* Di dalam bahasa R, fungsi `wilcox.test(..., paired = FALSE)` adalah nama fungsi untuk menjalankan **Mann-Whitney U Test** (disebut juga *Wilcoxon Rank-Sum Test*).
* Menggabungkan $30 + 30 = 60$ data dari kedua kelompok, mengurutkannya dari terkecil ke terbesar, lalu membandingkan jumlah peringkat (*rank sum*) antara Direct MVI dan Reducer MVI.
* **Effect Size Non-Parametrik**:
  1. **Mann-Whitney $r$**: $r = \frac{|Z|}{\sqrt{N_{\text{total}}}}$ (dengan $N_{\text{total}} = 60$).
  2. **Rank-Biserial Correlation ($r_{rb}$)**: $r_{rb} = \frac{2U}{n_1 n_2} - 1$.

---

## 4. STRUKTUR OUTPUT DATA UNTUK BAB IV

Semua tabel hasil berada di folder [`output/`](file:///C:/Users/AUDYFH/Skripsi/output/):

| Nama Berkas | Kegunaan di Naskah Skripsi Bab IV |
| :--- | :--- |
| `clean_metrics.csv` | Data bersih 120 baris ($30 \times 4$ skenario) untuk **Lampiran Skripsi**. |
| `descriptive_statistics.csv` | Nilai Mean, SD, Varians, Median, Min, Max, P95 untuk **Tabel Statistik Deskriptif Bab IV**. |
| `shapiro_wilk_groups.csv` | Detail hasil uji normalitas per kelompok mandiri ($N = 30$). |
| `prerequisite_tests.csv` | **Tabel Ringkasan Uji Prasyarat Bab IV** (berisi Uji Normalitas Shapiro-Wilk + Uji Homogenitas Varians F-test & Levene + Keputusan Alat Uji). |
| `hypothesis_test_results.csv` | **Tabel Hasil Uji Hipotesis Utama Bab IV** (berisi nilai $t(58)$ atau $U$, $p$-value, 95% CI, Pooled Cohen's $d$ / Mann-Whitney $r$, dan kesimpulan hipotesis). |

---

## 5. BANK PERTANYAAN & JAWABAN BIMBINGAN DOSEN (FAQ SKRIPSI)

Berikut adalah daftar pertanyaan yang paling sering ditanyakan dosen pembimbing/penguji beserta jawaban yang tepat:

---

### Q1: "Bagaimana cara kamu mengambil data, dan kenapa memilih Independent Sample t-Test serta Mann-Whitney U Test?"
> **Cara Menjawab:**
> *"Izin menjelaskan Pak/Bu. Sesuai rancangan eksperimen pada Bab 3.2 dan Gambar 3.1, kedua arsitektur (Direct MVI dan Reducer MVI) dibangun sebagai dua codebase dan aplikasi terpisah. Pengambilan data dilakukan secara sekuensial, yaitu menjalankan 30 iterasi pengujian penuh pada aplikasi Direct MVI terlebih dahulu, kemudian dilanjutkan 30 iterasi pada aplikasi Reducer MVI.*
> 
> *Karena kedua kelompok pengukuran berasal dari proses eksekusi yang terpisah dan tidak berpasangan secara waktu per iterasi, maka desain statistik yang tepat adalah **Uji Dua Sampel Independen (Two Independent Samples)**. Jika kedua kelompok berdistribusi normal dan homogen digunakan **Independent Sample t-Test**, sedangkan jika tidak normal digunakan uji non-parametrik **Mann-Whitney U Test**."*

---

### Q2: "Di Bab 3.6 proposalmu sempat tertulis Paired t-Test dan Wilcoxon, sementara di Bab 3.2 dan Gambar 3.1 tertulis Independen dan Mann-Whitney. Mana yang benar?"
> **Cara Menjawab:**
> *"Yang benar dan sesuai dengan pelaksanaan pengambilan data di lapangan adalah yang tertulis pada **Bab 3.2 dan Gambar 3.1**, yaitu kelompok pengukuran **independen** menggunakan **Independent Sample t-Test dan Mann-Whitney U Test**. Pada naskah Bab 3.6 terdapat kekeliruan penulisan istilah, dan akan segera saya selaraskan agar konsisten dengan Bab 3.2 dan Gambar 3.1."*

---

### Q3: "Apa saja syarat sebelum menggunakan Independent Sample t-Test, dan apakah datamu memenuhinya?"
> **Cara Menjawab:**
> *"Ada dua uji prasyarat yang saya lakukan pada taraf signifikansi $\alpha = 0,05$:*
> 1. ***Uji Normalitas (Shapiro-Wilk)***: *Menguji apakah masing-masing kelompok ($N=30$) berdistribusi normal ($p \ge 0,05$).*
> 2. ***Uji Homogenitas Varians (F-Test & Levene's Test)***: *Menguji apakah kedua kelompok memiliki varians yang sama/homogen ($p \ge 0,05$).*
> 
> *Hasilnya:*
> * *Pada aplikasi **Marketplace**, kedua kelompok terbukti **berdistribusi normal ($p > 0,20$)** dan memiliki **varians yang homogen ($p > 0,26$)** untuk seluruh metrik. Oleh karena itu, seluruh pengujian pada Marketplace sah menggunakan **Independent Sample t-Test dengan asumsi Equal Variance ($df = 58$)**.*
> * *Sedangkan pada aplikasi **Sosial Media**, meskipun variansnya homogen ($p > 0,15$), uji normalitas Shapiro-Wilk menunjukkan nilai **$p < 0,05$** (tidak normal), sehingga seluruh pengujian pada Sosial Media dialihkan menggunakan **Mann-Whitney U Test**."*

---

### Q4: "Kenapa pada Sosial Media datanya tidak berdistribusi normal, sedangkan pada Marketplace normal?"
> **Cara Menjawab:**
> *"Pada skenario Sosial Media (Medium State Complexity dengan 26 variabel state), interaksi pengujian melibatkan pengguliran feed unggahan, membuka modal komentar, menyukai komentar, dan memfilter unggahan yang memicu alokasi memori dan Garbage Collection (GC) secara periodik. Pada beberapa iterasi saat GC tidak terpicu, CPU berada di kisaran ~4,2%, sedangkan pada iterasi standar berada di kisaran ~5,9%. Hal ini menyebabkan distribusi data agak condong (skewed) sehingga terdeteksi tidak normal oleh Shapiro-Wilk. Itulah sebabnya penggunaan Mann-Whitney U Test yang berbasis median/peringkat menjadi sangat tepat untuk Sosial Media."*

---

### Q5: "Apa kesimpulan utama dari hasil uji hipotesis H1 (Responsivitas) dan H2 (Penggunaan CPU)?"
> **Cara Menjawab:**
> *"Hasil pengujian menunjukkan pola yang konsisten:*
> 1. *Pada **Marketplace (Low State Complexity - 12 variabel state)**:*
>    * **H1 (Responsivitas) Diterima**: *Direct MVI (9,08 ms) secara signifikan lebih cepat dibandingkan Reducer MVI (9,33 ms) dengan nilai $t(58) = -6,25, p < 0,001$ dan ukuran efek besar (Pooled Cohen's $d = -1,61$).*
>    * **H2 (Penggunaan CPU) Diterima**: *Direct MVI (8,56%) secara signifikan lebih hemat CPU sebesar 3,19% dibandingkan Reducer MVI (8,83%) dengan nilai $t(58) = -5,74, p < 0,001$ dan ukuran efek besar ($d = -1,48$).*
> 2. *Pada **Sosial Media (Medium State Complexity - 26 variabel state)**:*
>    * **H1 (Responsivitas)**: *Pada kondisi tipikal (P50), terdapat perbedaan signifikan ($U = 689, p = 0,0004$) dengan selisih rata-rata sangat tipis 0,05 ms (0,78%), namun pada beban tinggi (P95) tidak terdapat perbedaan signifikan ($U = 524, p = 0,2772$).*
>    * **H2 (Penggunaan CPU) Ditolak**: *Tidak terdapat perbedaan penggunaan CPU yang signifikan antara Direct MVI (5,51%) dan Reducer MVI (5,65%) berdasarkan uji Mann-Whitney ($U = 426, p = 0,7283$)."*

---

### Q6: "Pada Marketplace, kenapa waktu logika ($T_{logic}$) tidak berbeda signifikan ($p = 0,268$), padahal responsivitas totalnya berbeda signifikan ($p < 0,001$)?"
> **Cara Menjawab:**
> *"Dari hasil dekomposisi waktu responsivitas ($T_{\text{responsivitas}} = T_{\text{logic}} + T_{\text{frame}}$):*
> * *Durasi eksekusi logika di CPU ViewModel ($T_{\text{logic}}$) antara Direct (0,137 ms) dan Reducer (0,144 ms) hanya berselisih 0,007 ms dan tidak signifikan secara statistik ($p = 0,2683$).*
> * *Selisih utama justru berasal dari **waktu perenderan frame antarmuka Jetpack Compose ($T_{\text{frame}}$)** yang berbeda sangat signifikan ($t(58) = -6,36, p < 0,001$).*
> * *Hal ini membuktikan bahwa overhead utama Reducer-Based MVI bukan terletak pada kalkulasi fungsi reducer di ViewModel, melainkan pada instansiasi objek state/mutation perantara yang memengaruhi beban kerja rekomposisi dan rendering frame pada subsistem grafis Jetpack Compose."*

---

*Dokumen ini diperbarui pada branch `independent-test` untuk proyek analisis skripsi Audy Fitrah Habibie.*
