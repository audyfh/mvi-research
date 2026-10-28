# PANDUAN LENGKAP KODE R & PERSIAPAN BIMBINGAN SKRIPSI

**Judul Penelitian:** ANALISIS KOMPARATIF PERFORMA DIRECT STATE UPDATE MVI DAN REDUCER-BASED MVI PADA APLIKASI ANDROID BERBASIS JETPACK COMPOSE  
**Peneliti:** Audy Fitrah Habibie (NIM: 235150400111018)  
**Tujuan Dokumen:** 
1. Menjelaskan logika kode R (`Analisis.R` dan `Uji_Hipotesis.R`) secara bertahap dan mudah dipahami bagi pemula.
2. Mempersiapkan Anda menghadapi pertanyaan kritis dari dosen pembimbing/penguji saat sesi bimbingan.

---

## DAFTAR ISI
1. [Alur Kerja Analisis Data Skripsi](#1-alur-kerja-analisis-data-skripsi)
2. [Bedah Kode: `Analisis.R` (Ekstraksi & Normalitas)](#2-bedah-kode-analisisr-ekstraksi--normalitas)
3. [Bedah Kode: `Uji_Hipotesis.R` (Inferensial & Visualisasi)](#3-bedah-kode-uji_hipotesisr-inferensial--visualisasi)
4. [Struktur Output Data untuk Bab IV](#4-struktur-output-data-untuk-bab-iv)
5. [Bank Pertanyaan & Jawaban Bimbingan Dosen (FAQ Skripsi)](#5-bank-pertanyaan--jawaban-bimbingan-dosen-faq-skripsi)

---

## 1. ALUR KERJA ANALISIS DATA SKRIPSI

Proses analisis dibagi menjadi dua tahap yang saling terhubung:

```
[Raw Data: 4 Berkas JSON di /data]
  │ (marketplace_direct, marketplace_reducer, socmed_direct, socmed_reducer)
  ▼
[Analisis.R]
  ├─ 1. Membaca JSON & Mengekstrak Metrik (CPU, T_logic, T_frame, Responsivitas)
  ├─ 2. Menyimpan Dataset Bersih -> output/clean_metrics.csv
  ├─ 3. Menghitung Statistik Deskriptif (Mean, SD, Median, Min, Max)
  ├─ 4. Menjalankan Uji Normalitas Shapiro-Wilk (Kelompok & Selisih Berpasangan)
  └─ 5. Menggambar Q-Q Plot Diagnostik -> output/figures/
  ▼
[Uji_Hipotesis.R]
  ├─ 1. Memuat clean_metrics.csv
  ├─ 2. Menjalankan Uji Hipotesis H1 (Responsivitas) via Paired Sample t-Test
  ├─ 3. Menjalankan Uji Hipotesis H2 (CPU):
  │      ├─ Marketplace: Paired Sample t-Test (karena normal)
  │      └─ Sosial Media: Wilcoxon Signed-Rank Test (karena tidak normal)
  ├─ 4. Menghitung Effect Size (Cohen's d & Wilcoxon r) dan Selisih Relatif (%)
  ├─ 5. Melakukan Dekomposisi Waktu (T_logic vs T_frame)
  └─ 6. Membuat Grafik Siap Publikasi Bab IV -> output/figures/
```

---

## 2. BEDAH KODE: `Analisis.R` (EKSTRAKSI & NORMALITAS)

Berkas ini bertugas mengubah data mentah JSON hasil AndroidX Macrobenchmark menjadi angka-angka yang siap diuji statistik.

### A. Bagian Pustaka (Library)
```r
suppressPackageStartupMessages({
  library(jsonlite)  # Alat untuk membaca file JSON
  library(dplyr)     # Alat untuk memanipulasi, menyaring, dan mengelompokkan tabel data
  library(ggplot2)   # Alat untuk menggambar grafik dan visualisasi statistik
})
```
* **`jsonlite`**: Format data Macrobenchmark bawaan Android Studio berupa teks `.json`. Library ini mengubah teks hierarki tersebut menjadi struktur data R.
* **`dplyr`**: Menggunakan sintaks modern R seperti tanda pipa `%>%` (artinya *"lalu lakukan..."*).
* **`ggplot2`**: Standar emas pembuatan grafik akademik di R.

---

### B. Fungsi Ekstraksi: `extract_macrobenchmark_data()`
Fungsi ini dipanggil 4 kali untuk masing-masing berkas JSON.

```r
raw_data <- fromJSON(file_path, simplifyVector = FALSE)
bm <- raw_data$benchmarks[[1]]
metrics <- bm$metrics
sampled <- bm$sampledMetrics
```
* Macrobenchmark menyimpan data dalam objek `benchmarks`.
* Di dalamnya terdapat `metrics` (metrik teragregasi per iterasi) dan `sampledMetrics` (data durasi tiap frame tampilan yang jumlahnya ribuan).

#### Mengapa $T_{logic}$ Dihitung sebagai `ProcessIntentSumMs / ProcessIntentCount`?
```r
intent_sum_runs <- unlist(metrics[[sum_key]]$runs)
intent_cnt_runs <- unlist(metrics[[cnt_key]]$runs)
t_logic_avg <- intent_sum_runs / intent_cnt_runs
```
* Di dalam Macrobenchmark, setiap tombol diklik atau di-*scroll*, ViewModel memproses sebuah **Intent**.
* `ProcessIntentSumMs` adalah total waktu (ms) yang dihabiskan untuk memproses seluruh intent dalam 1 sesi pengujian (sekitar 40-60 intent).
* `ProcessIntentCount` adalah total banyaknya intent yang diproses.
* Dengan membaginya, kita mendapatkan **durasi rata-rata per 1 kali pemrosesan intent** (sekitar $0,13 - 0,14$ milidetik).

#### Dari Mana $T_{frame}$ Diperoleh?
```r
for (i in seq_len(n_runs)) {
  f_vals <- as.numeric(frame_runs[[i]])
  t_frame_p50[i] <- as.numeric(quantile(f_vals, probs = 0.50, names = FALSE))
  t_frame_p95[i] <- as.numeric(quantile(f_vals, probs = 0.95, names = FALSE))
}
```
* Dalam 1 kali run (misal selama scrolling/interaksi), layar Android merender sekitar $1.300 - 1.400$ frame grafis Jetpack Compose.
* `quantile(..., probs = 0.50)` mengambil nilai **median ($P_{50}$)**, yaitu durasi frame yang mewakili **kondisi tipikal/normal** aplikasi.
* `quantile(..., probs = 0.95)` mengambil nilai **persentil ke-95 ($P_{95}$)**, yaitu kondisi saat beban rendering sedang berat (frame-frame yang lambat).

#### Menghitung Responsivitas Total Sesuai Persamaan 2.1 di Proposal:
```r
# Responsivitas = T_logic + T_frame
responsiveness_p50 <- t_logic_avg + t_frame_p50
responsiveness_p95 <- t_logic_avg + t_frame_p95
```
* Waktu total respons pengguna adalah gabungan sejak logika di ViewModel selesai diproses ($T_{logic}$) ditambah waktu antarmuka Compose menggambar perubahan ke layar ($T_{frame}$).

---

### C. Uji Normalitas Shapiro-Wilk
```r
# Uji pada selisih berpasangan
diff_vals <- d_df[[m]] - r_df[[m]]
st <- shapiro.test(diff_vals)
```
* `shapiro.test()` menghasilkan dua hal:
  1. **Statistik $W$**: Mengukur kemiripan data dengan kurva normal teoretis (semakin mendekati 1.0, data semakin sempurna normalnya).
  2. **$p$-value**: Peluang data menyimpang dari kurva normal.
* Logika keputusan:
  ```r
  ifelse(st$p.value >= 0.05, "Paired Sample t-Test", "Wilcoxon Signed-Rank Test")
  ```
  Jika $p \ge 0,05$ artinya data selisih aman (normal), maka kita berhak menggunakan **Paired t-Test**. Jika $p < 0,05$, data tidak normal sehingga harus dialihkan ke **Wilcoxon**.

---

## 3. BEDAH KODE: `Uji_Hipotesis.R` (INFERENSIAL & VISUALISASI)

Berkas ini menguji apakah perbedaan antara Direct MVI dan Reducer MVI itu **nyata (signifikan secara statistik)** atau hanya kebetulan/noise.

### A. Uji Paired Sample t-Test: `run_paired_ttest()`
```r
t_res <- t.test(direct_vec, reducer_vec, paired = TRUE)
```
* `paired = TRUE` adalah parameter kunci: ini memberitahu R bahwa data baris ke-1 Direct berpasangan dengan baris ke-1 Reducer (karena diambil pada iterasi benchmark yang sama pada perangkat yang sama).
* `t_res$statistic`: Nilai statistik $t$.
* `t_res$p.value`: Nilai signifikansi. Jika $p < 0,05$, perbedaan kedua arsitektur terbukti nyata.

#### Menghitung Ukuran Efek (Cohen's $d_z$):
```r
cohens_d <- mean_diff / sd_diff
```
* Nilai $p$ hanya memberi tahu *apakah ada perbedaan*.
* Cohen's $d$ memberi tahu ***seberapa besar perbedaan tersebut di dunia nyata***:
  * $|d| < 0,2$ = Negligible (perbedaan sangat kecil/bisa diabaikan)
  * $0,2 \le |d| < 0,5$ = Small (kecil)
  * $0,5 \le |d| < 0,8$ = Medium (sedang)
  * $|d| \ge 0,8$ = **Large (besar / berdampak nyata)**

---

### B. Uji Wilcoxon Signed-Rank Test: `run_wilcoxon_test()`
Digunakan khusus untuk **Penggunaan CPU pada Sosial Media** karena data selisihnya melanggar asumsi normalitas ($p = 0,0045$).

```r
w_res <- wilcox.test(direct_vec, reducer_vec, paired = TRUE, exact = FALSE, correct = TRUE)
```
* Mengubah selisih data menjadi peringkat (*rank*), sehingga kebal terhadap nilai ekstrem (*outlier*).
* Menghitung nilai uji $V$ dan menghitung pendekatan nilai $Z$ untuk memperoleh effect size non-parametrik:
  $$r = \frac{|Z|}{\sqrt{N}}$$

---

### C. Dekomposisi Waktu: Mengapa Sangat Kritis?
Di dalam `Uji_Hipotesis.R`, kita tidak hanya menguji total responsivitas, tetapi juga menguji masing-masing komponen pembentuknya:
1. Uji t-Test pada $T_{logic}$
2. Uji t-Test pada $T_{frame}$

**Hasil temuan:**
Pada Marketplace, selisih $T_{logic}$ **tidak signifikan** ($p = 0,3209$), sedangkan $T_{frame}$ **sangat signifikan** ($p < 0,001$). Artinya, waktu CPU di ViewModel untuk memproses intent hampir sama cepatnya, tetapi Reducer menyebabkan beban rekomposisi/rendering antarmuka Compose menjadi lebih berat! Ini adalah temuan analisis bernilai tinggi untuk Bab V skripsi Anda.

---

## 4. STRUKTUR OUTPUT DATA UNTUK BAB IV

Semua data telah diekspor ke folder [`output/`](file:///C:/Users/AUDYFH/Skripsi/output/):

| Nama Berkas | Isi / Kegunaan di Skripsi |
| :--- | :--- |
| `clean_metrics.csv` | Tabel data mentah yang rapi per iterasi (Iterasi 1 s.d. 30). Bisa dilampirkan di **Lampiran Skripsi**. |
| `descriptive_statistics.csv` | Nilai Mean, SD, Median, Min, Max, P95. Langsung disalin ke **Tabel Statistik Deskriptif Bab IV**. |
| `shapiro_wilk_paired_diffs.csv` | Nilai statistik $W$ dan $p$-value normalitas. Disalin ke **Tabel Uji Prasyarat Analisis Bab IV**. |
| `hypothesis_test_results.csv` | Nilai $t$, $df$, $p$-value, Cohen's $d$, dan kesimpulan. Disalin ke **Tabel Hasil Uji Hipotesis Bab IV**. |
| `output/figures/*.png` | Grafik batang 95% CI dan Boxplot resolusi tinggi (300 DPI) untuk **Gambar pada Bab IV**. |

---

## 5. BANK PERTANYAAN & JAWABAN BIMBINGAN DOSEN (FAQ SKRIPSI)

Berikut adalah pertanyaan-pertanyaan yang paling sering dilontarkan oleh dosen pembimbing atau penguji beserta panduan cara menjawabnya:

---

### Q1: "Kenapa kamu menggunakan uji Shapiro-Wilk untuk uji normalitas, bukan Kolmogorov-Smirnov?"
> **Cara Menjawab:**
> *"Izin menjelaskan Pak/Bu. Dalam metodologi penelitian ini, jumlah sampel pengujian pada tiap perlakuan adalah **$N = 30$ iterasi**. Berdasarkan literatur statistik, uji Shapiro-Wilk terbukti memiliki kekuatan uji (statistical power) yang jauh lebih tinggi dan sensitif untuk mendeteksi ketidaknormalan pada sampel berukuran kecil hingga menengah ($N < 50$), dibandingkan uji Kolmogorov-Smirnov yang lebih cocok untuk sampel besar di atas 100."*

---

### Q2: "Kenapa uji normalitas kamu hitung pada 'selisih berpasangan' ($\Delta = Direct - Reducer$), bukan hanya pada masing-masing kelompok data?"
> **Cara Menjawab:**
> *"Karena desain eksperimen skripsi saya adalah **rancangan berpasangan (paired experimental design)**, di mana iterasi ke-$i$ Direct MVI dan Reducer MVI dijalankan pada perangkat fisik yang sama, urutan interaksi yang identik, dan kondisi latar belakang yang sama.*
> 
> *Secara teori matematis statistik, uji Paired Sample t-Test pada dasarnya adalah One-Sample t-Test terhadap nilai selisihnya ($d_i = X_{\text{Direct}, i} - X_{\text{Reducer}, i}$) dengan hipotesis nol $\mu_d = 0$. Oleh karena itu, **asumsi normalitas yang diwajibkan oleh Paired t-Test adalah normalitas dari nilai selisih berpasangan tersebut**.*
> 
> *Selain itu, dengan menghitung selisih per iterasi, kita meniadakan faktor pengganggu (noise) seperti fluktuasi suhu perangkat atau background process Android yang dialami bersama oleh kedua pengujian. Namun demikian, untuk melengkapi transparansi data, saya juga tetap menyajikan tabel normalitas kelompok mandiri di lampiran."*

---

### Q3: "Kenapa pada modul Marketplace kamu menggunakan Paired t-Test, tapi untuk CPU di Sosial Media beralih ke Wilcoxon Signed-Rank Test?"
> **Cara Menjawab:**
> *"Hal tersebut dilakukan untuk mematuhi kaidah metodologi penelitian yang telah saya tetapkan di Bab 3 Subbab 3.6:*
> * *Pada Marketplace, selisih penggunaan CPU berdistribusi normal dengan nilai **$p = 0,6407 \ge 0,05$**, sehingga asumsi parametrik terpenuhi dan digunakan **Paired Sample t-Test**.*
> * *Namun pada Sosial Media, selisih penggunaan CPU menghasilkan nilai **$p = 0,0045 < 0,05$**, yang berarti asumsi normalitas dilanggar. Oleh karena itu, analisis dialihkan ke uji non-parametrik **Wilcoxon Signed-Rank Test** agar kesimpulan statistik tetap valid dan tidak bias.*
> * *Sebagai verifikasi tambahan (robustness check), saya juga tetap menjalankan Paired t-Test pada CPU Sosial Media dan hasilnya konsisten: sama-sama menunjukkan tidak ada perbedaan yang signifikan ($p = 0,436$)."*

---

### Q4: "Apa kesimpulan utama dari uji hipotesis H1 (Responsivitas) dan H2 (Penggunaan CPU)?"
> **Cara Menjawab:**
> *"Kesimpulan utamanya menunjukkan karakteristik yang berbeda tergantung tingkat kompleksitas state:*
> 1. *Pada **Marketplace (Low State Complexity / 12 variabel state)**: **Direct State Update MVI unggul signifikan atas Reducer-Based MVI**. Direct MVI lebih responsif ($p < 0,001$) dengan ukuran efek besar ($d = -1,32$), dan lebih hemat pemakaian CPU sebesar 3,19% ($p < 0,001, d = -1,09$).*
> 2. *Pada **Sosial Media (Medium State Complexity / 26 variabel state)**: Kedua arsitektur memiliki performa yang relatif setara. Penggunaan CPU **tidak berbeda signifikan** ($p = 0,8210$). Sedangkan pada responsivitas kondisi tipikal (P50), Reducer sedikit lebih cepat 0,05 ms ($p = 0,0253$), namun pada beban tinggi (P95) perbedaan tersebut tidak lagi signifikan ($p = 0,1968$)."*

---

### Q5: "Pada Marketplace, responsivitasnya beda signifikan, tapi kenapa waktu pemrosesan logikanya ($T_{logic}$) tidak berbeda signifikan? Lalu dari mana selisih waktu itu muncul?"
> **Cara Menjawab:**
> *(Ini pertanyaan emas yang menunjukkan Anda sangat paham sistem Android Jetpack Compose)*
> 
> *"Izin menjelaskan Pak/Bu, ini adalah salah satu temuan dekomposisi paling menarik dalam skripsi saya:*
> * *Waktu pemrosesan logika di ViewModel ($T_{logic}$) pada Direct MVI adalah $0,137$ ms dan Reducer MVI adalah $0,144$ ms. Selisih ini secara statistik **tidak signifikan ($p = 0,3209$)** karena eksekusi fungsi reducer di memori CPU berlangsung sangat cepat (orde mikrodetik).*
> * *Namun, selisih responsivitas total menjadi sangat signifikan karena disumbang oleh **waktu perenderan antarmuka ($T_{frame}$, $p < 0,001$)**.*
> * *Penyebabnya adalah: pada Reducer-Based MVI, pembuatan objek state baru secara terpusat dan pengiriman mutation object memicu siklus **rekomposisi antarmuka Jetpack Compose** yang sedikit lebih sering/lama dibandingkan Direct MVI yang hanya memperbarui atribut state yang ditargetkan secara langsung."*

---

### Q6: "Apa arti dari nilai Cohen's $d$ yang kamu cantumkan di tabel?"
> **Cara Menjawab:**
> *"Cohen's $d$ adalah ukuran kekuatan pengaruh (**Effect Size**). Nilai $p$-value hanya memberi tahu kita apakah perbedaan itu nyata atau kebetulan semata, tetapi tidak memberi tahu seberapa besar dampaknya.*
> 
> *Pada Marketplace, nilai Cohen's $d$ untuk responsivitas adalah **$-1,32$** dan CPU adalah **$-1,09$**. Menurut kriteria Cohen (1988), nilai $|d| \ge 0,8$ dikategorikan sebagai **Large Effect (efek besar)**. Artinya, perbedaan arsitektur MVI memberikan dampak performa nyata yang cukup substansial pada aplikasi dengan state sederhana."*

---

### Q7: "Bagaimana kamu memastikan eksperimen ini terkontrol (*fair*) dan tidak bias antar kedua varian?"
> **Cara Menjawab:**
> *"Sesuai dengan rancangan di Bab 3 Subbab 3.2 dan 3.5, seluruh faktor non-perlakuan dikontrol secara ketat:*
> 1. *Kedua varian dibangun dengan komponen tata letak UI, logika bisnis, dan dataset katalog awal yang identik 100%.*
> 2. *Pengujian dijalankan pada satu unit smartphone fisik yang sama (Samsung Galaxy A24, RAM 8 GB, Android 15).*
> 3. *Interaksi pengguna digerakkan secara otomatis menggunakan script UI Automator yang deterministik.*
> 4. *Macrobenchmark dijalankan dengan mode kompilasi penuh (`CompilationMode.Full`) dan `StartupMode.WARM` agar kondisi eksekusi stabil dan tidak terpengaruh oleh proses kompilasi JIT di tengah pengujian."*

---

*Dokumen ini dibuat otomatis untuk proyek analisis skripsi Audy Fitrah Habibie.*  
*File skrip terkait: [`Analisis.R`](file:///C:/Users/AUDYFH/Skripsi/Analisis.R) & [`Uji_Hipotesis.R`](file:///C:/Users/AUDYFH/Skripsi/Uji_Hipotesis.R).*
