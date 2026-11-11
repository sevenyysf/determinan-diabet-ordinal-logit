# Analisis Determinan Status Diabetes Menggunakan Regresi Logistik Ordinal

Proyek akhir mata kuliah **Analisis Data Kategori**, Universitas Padjadjaran.

**Penulis:** Pretri Seveny Yasyaf dan Mutiara Meisya Fadila

## Ringkasan

Proyek ini mengidentifikasi faktor-faktor yang memengaruhi status diabetes
(tidak diabetes, prediabetes, diabetes) menggunakan **regresi logistik ordinal**
(proportional odds model) pada data *Diabetes Health Indicators BRFSS 2015*
dari CDC (253.680 observasi).

Prediktor: hipertensi, kolesterol tinggi, BMI, status merokok, konsumsi alkohol
berat, aktivitas fisik, kepemilikan layanan kesehatan, usia, jenis kelamin,
tingkat pendapatan, dan jumlah hari gangguan kesehatan mental.

## Isi Repositori

```
├── README.md
├── paper/
│   ├── Full Paper.pdf                     # naskah paper
│   └── Rpubs Paper.html                   # laporan lengkap (sama dengan versi RPubs)
├── R/
│   └── Analisis_Determinan_Diabetes.R     # seluruh kode analisis
└── data/
    ├── README.md                          # cara mendapatkan data
    └── diabetes_012_health_indicators_BRFSS2015.csv
```

Laporan online: https://rpubs.com/mutiaramsyaf/ProjectAkhirADK

## Cara Menjalankan

1. Pastikan file data ada di `data/diabetes_012_health_indicators_BRFSS2015.csv`
   (lihat `data/README.md`).
2. Buka RStudio dan set working directory ke folder utama repositori ini.
3. Install package yang dibutuhkan:
   ```r
   install.packages(c("dplyr","tidyr","MASS","ordinal","brant","car",
                      "knitr","kableExtra","ggplot2","scales","tibble"))
   ```
4. Jalankan `R/Analisis_Determinan_Diabetes.R`.

## Metode

- Model logit kumulatif (proportional odds), estimasi dengan `MASS::polr`
- Uji multikolinearitas (VIF)
- Uji asumsi proportional odds (plot parallel lines, Brant test, nominal test)
- Uji simultan (Likelihood Ratio Test) dan uji parsial (Wald)
- Odds ratio dengan interval kepercayaan 95%
- Evaluasi klasifikasi (confusion matrix, akurasi, sensitivitas, presisi)

## Sumber Data

CDC Diabetes Health Indicators, UCI Machine Learning Repository:
https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators
