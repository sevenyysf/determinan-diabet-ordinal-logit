# ============================================================
# REGRESI LOGISTIK ORDINAL
# Dataset  : Diabetes Health Indicators BRFSS 2015
# Respons  : Diabetes_012 (0 = Tidak Diabetes, 1 = Prediabetes,
#                          2 = Diabetes)
# Prediktor: HighBP, HighChol, BMI, Smoker, HvyAlcoholConsump,
#            PhysActivity, Sex, Age, Income, MentHlth, AnyHealthcare
# ============================================================

# CARA MENJALANKAN
#   1. Download dataset dari UCI (lihat data/README.md) dan simpan sebagai
#      data/diabetes_012_health_indicators_BRFSS2015.csv
#   2. Buka RStudio, set working directory ke folder utama repo
#      (Session > Set Working Directory > Choose Directory),
#      atau buka lewat file .Rproj.
#   3. Install package yang belum ada:
#      install.packages(c("dplyr","tidyr","MASS","ordinal","brant","car",
#                         "knitr","kableExtra","ggplot2","scales","tibble"))
# ============================================================

# ============================================================
# PACKAGE
# ============================================================
library(dplyr)
library(tidyr)
library(MASS)
library(ordinal)
library(brant)
library(car)
library(knitr)
library(kableExtra)
library(ggplot2)
library(scales)

# ============================================================
# BLOK 1: IMPORT DATA
# ============================================================
data_path <- "data/diabetes_012_health_indicators_BRFSS2015.csv"
if (!file.exists(data_path)) {
  stop("File data tidak ditemukan: ", data_path,
       "\nDownload dari https://archive.ics.uci.edu/dataset/891/cdc+diabetes+health+indicators",
       "\ndan pastikan working directory = folder utama repo.")
}
d2 <- read.csv(data_path, sep = ',', dec = '.')
head(d2)
str(d2)
dim(d2)

# ============================================================
# BLOK 2: UBAH STRUKTUR DATA
# ============================================================
# Variabel biner → factor
d2$HighBP             <- factor(d2$HighBP,             levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$HighChol           <- factor(d2$HighChol,           levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$Smoker             <- factor(d2$Smoker,             levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$HvyAlcoholConsump  <- factor(d2$HvyAlcoholConsump,  levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$PhysActivity       <- factor(d2$PhysActivity,       levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$AnyHealthcare      <- factor(d2$AnyHealthcare,      levels = c(0,1),
                                labels = c("Tidak","Ya"))
d2$Sex                <- factor(d2$Sex,                levels = c(0,1),
                                labels = c("Perempuan","Laki-laki"))

# Variabel kategorik bertingkat -> unordered factor (dummy contrast)
# Diperlakukan sebagai nominal agar menghasilkan koefisien per level
# (Age2, Age3, ..., Income2, Income3, ...) yang mudah diinterpretasi
# dan kompatibel dengan car::vif() tanpa GVIF polynomial
d2$Age    <- factor(d2$Age,    levels = 1:13)
d2$Income <- factor(d2$Income, levels = 1:8)

# Variabel respons → factor ordered
# 0 = Tidak Diabetes | 1 = Prediabetes | 2 = Diabetes
d2$diabetes <- factor(d2$Diabetes_012,
                      levels  = c(0, 1, 2),
                      labels  = c("Tidak_Diabetes", "Prediabetes", "Diabetes"),
                      ordered = TRUE)

# Cek ulang
str(d2)

# ============================================================
# BAB 3.1. STATISTIK DESKRIPTIF
# ============================================================

# ------------------------------------------------------------
# 3.1.1. DISTRIBUSI STATUS DIABETES
# ------------------------------------------------------------

# --- Tabel distribusi ---
d2 %>%
  count(diabetes) %>%
  mutate(Proporsi = n / sum(n)) %>%
  kable(
    digits  = 3,
    col.names = c("Status Diabetes", "Frekuensi", "Proporsi"),
    caption = "Distribusi status diabetes responden (Diabetes_012)"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed","responsive"),
    full_width = FALSE
  )

# --- Visualisasi distribusi ---
d2 %>%
  count(diabetes) %>%
  mutate(proporsi = n / sum(n)) %>%
  ggplot(aes(x = diabetes, y = proporsi, fill = diabetes)) +
  geom_col(width = 0.55, alpha = 0.94) +
  geom_text(aes(label = percent(proporsi, accuracy = 0.1)),
            vjust = -0.4, fontface = "bold", color = "#243142") +
  scale_y_continuous(labels = percent_format(), limits = c(0, 0.90)) +
  scale_fill_manual(values = c("#2f7f73","#5568b8","#c0392b")) +
  labs(
    title    = "Distribusi Status Diabetes Responden",
    subtitle = "Respons ordinal: Tidak_Diabetes < Prediabetes < Diabetes.",
    x        = NULL,
    y        = "Proporsi"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ------------------------------------------------------------
# 3.1.2. KARAKTERISTIK VARIABEL PREDIKTOR
# ------------------------------------------------------------

# --- Ringkasan statistik deskriptif seluruh variabel ---
summary(d2[, c("diabetes","HighBP","HighChol","BMI","Smoker",
               "HvyAlcoholConsump","PhysActivity","AnyHealthcare",
               "Age","Sex","Income","MentHlth")])

# --- Cek missing values ---
colSums(is.na(d2[, c("diabetes","HighBP","HighChol","BMI","Smoker",
                     "HvyAlcoholConsump","PhysActivity","AnyHealthcare",
                     "Age","Sex","Income","MentHlth")]))

# --- Tabel frekuensi variabel kategorik ---
cat_vars <- c("HighBP","HighChol","Smoker","HvyAlcoholConsump",
              "PhysActivity","AnyHealthcare","Sex")

for (v in cat_vars) {
  cat("\n===", v, "===\n")
  print(
    d2 %>%
      count(.data[[v]]) %>%
      mutate(Proporsi = round(n / sum(n), 3)) %>%
      kable(caption = paste("Distribusi", v)) %>%
      kable_styling(bootstrap_options = c("striped","hover","condensed"),
                    full_width = FALSE)
  )
}

# --- Statistik deskriptif variabel kontinu ---
d2 %>%
  summarise(
    BMI_mean  = round(mean(BMI), 2),    BMI_sd  = round(sd(BMI), 2),
    BMI_min   = min(BMI),               BMI_max = max(BMI),
    MHlth_mean= round(mean(MentHlth),2),MHlth_sd= round(sd(MentHlth),2),
    MHlth_min = min(MentHlth),          MHlth_max= max(MentHlth)
  ) %>%
  tidyr::pivot_longer(everything(), names_to = "Statistik", values_to = "Nilai") %>%
  kable(digits = 2, caption = "Statistik deskriptif variabel kontinu") %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed"),
                full_width = FALSE)

# ============================================================
# BAB 3.2. UJI MULTIKOLINEARITAS
# ============================================================
# Fit model linier dummy untuk VIF (car::vif memerlukan lm/glm)
# Age dan Income sudah unordered factor di d2 — langsung pakai d2

vif_model <- glm(
  as.numeric(diabetes) ~ HighBP + HighChol + BMI + Smoker +
    HvyAlcoholConsump + PhysActivity + AnyHealthcare +
    Age + Sex + Income + MentHlth,
  data   = d2,
  family = gaussian()
)

vif_raw <- car::vif(vif_model)

# car::vif mengembalikan matrix (GVIF, df, GVIF^(1/(2*df))) untuk faktor
# Ambil kolom GVIF^(1/(2*df)) yang setara dengan VIF skalar
if (is.matrix(vif_raw)) {
  vif_result <- data.frame(
    Variabel   = rownames(vif_raw),
    GVIF       = round(vif_raw[, 1], 4),
    Df         = vif_raw[, 2],
    VIF_adj    = round(vif_raw[, 3]^2, 4),   # (GVIF^(1/(2*df)))^2 ≈ VIF
    Keterangan = ifelse(vif_raw[, 3]^2 > 10, "Multikolinearitas Tinggi",
                        ifelse(vif_raw[, 3]^2 > 5, "Multikolinearitas Sedang",
                               "Tidak Multikolinear"))
  )
} else {
  vif_result <- data.frame(
    Variabel   = names(vif_raw),
    VIF        = round(vif_raw, 4),
    Keterangan = ifelse(vif_raw > 10, "Multikolinearitas Tinggi",
                        ifelse(vif_raw > 5, "Multikolinearitas Sedang",
                               "Tidak Multikolinear"))
  )
}

vif_result %>%
  kable(
    digits  = 4,
    caption = "Nilai VIF untuk deteksi multikolinearitas"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed","responsive"),
    full_width = FALSE
  )

# ============================================================
# BAB 3.3. MODEL REGRESI LOGISTIK ORDINAL
# ============================================================

# ------------------------------------------------------------
# 3.3.1. SPESIFIKASI MODEL
# Asumsi proportional odds (parallel lines)
# Baseline (kategori terendah): Tidak_Diabetes
# Link function: logit (cumulative logit model)
# ------------------------------------------------------------

fit_ord2 <- MASS::polr(
  diabetes ~ HighBP + HighChol + BMI + Smoker + HvyAlcoholConsump +
    PhysActivity + AnyHealthcare + Age + Sex + Income + MentHlth,
  data   = d2,
  method = "logistic",
  Hess   = TRUE
)

summary(fit_ord2)

# ------------------------------------------------------------
# 3.3.2. HASIL ESTIMASI PARAMETER
# ------------------------------------------------------------
ord_coef2 <- coef(summary(fit_ord2))

result_ord2 <- as.data.frame(ord_coef2) %>%
  tibble::rownames_to_column("parameter") %>%
  dplyr::rename(
    estimate  = Value,
    std_error = `Std. Error`,
    t_value   = `t value`
  ) %>%
  mutate(
    p_value        = 2 * (1 - pnorm(abs(t_value))),
    jenis          = ifelse(grepl("\\|", parameter), "Cutpoint", "Koefisien"),
    estimate_slide = ifelse(jenis == "Koefisien", -estimate, estimate),
    OR_polr        = ifelse(jenis == "Koefisien",
                            exp(estimate), NA_real_),
    OR_slide       = ifelse(jenis == "Koefisien",
                            exp(estimate_slide), NA_real_),
    CI_low_polr    = ifelse(jenis == "Koefisien",
                            exp(estimate - 1.96 * std_error), NA_real_),
    CI_high_polr   = ifelse(jenis == "Koefisien",
                            exp(estimate + 1.96 * std_error), NA_real_)
  )

result_ord2 %>%
  mutate(
    across(
      c(estimate, estimate_slide, std_error, t_value, p_value,
        OR_polr, OR_slide, CI_low_polr, CI_high_polr),
      ~ round(.x, 4)
    )
  ) %>%
  kable(
    caption   = "Ringkasan hasil regresi logistik ordinal (Baseline: Tidak_Diabetes)",
    col.names = c("Parameter","Estimate polr","SE","z/t-value","p-value",
                  "Jenis","Estimate slide","OR polr","OR slide",
                  "CI polr 2.5%","CI polr 97.5%")
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed","responsive"),
    full_width = TRUE
  )

# ============================================================
# BAB 3.4. UJI ASUMSI PROPORTIONAL ODDS
# ============================================================

# ------------------------------------------------------------
# 3.4.1. VISUAL ASSESSMENT — PARALLEL LINES
# ------------------------------------------------------------
grid_ord2 <- expand.grid(
  BMI              = seq(min(d2$BMI), max(d2$BMI), length.out = 120),
  HighBP           = "Ya",
  HighChol         = "Ya",
  Smoker           = "Tidak",
  HvyAlcoholConsump= "Tidak",
  PhysActivity     = "Ya",
  AnyHealthcare    = "Ya",
  Age              = factor(7, levels = 1:13),
  Sex              = "Laki-laki",
  Income           = factor(6, levels = 1:8),
  MentHlth         = mean(d2$MentHlth)
)

grid_prob_ord2 <- predict(fit_ord2, newdata = grid_ord2, type = "probs")

eps <- 1e-6

grid_ord2_cumlogit <- grid_ord2 %>%
  bind_cols(as.data.frame(grid_prob_ord2)) %>%
  mutate(
    `P(Y <= Tidak_Diabetes)` = Tidak_Diabetes,
    `P(Y <= Prediabetes)`    = Tidak_Diabetes + Prediabetes
  ) %>%
  # Catatan: P(Y <= Diabetes) selalu = 1 (logit tak hingga), sehingga
  # hanya J-1 = 2 batas kumulatif yang diplot.
  tidyr::pivot_longer(
    cols      = c(`P(Y <= Tidak_Diabetes)`,
                  `P(Y <= Prediabetes)`),
    names_to  = "batas_kumulatif",
    values_to = "prob_kumulatif"
  ) %>%
  mutate(
    prob_kumulatif   = pmin(pmax(prob_kumulatif, eps), 1 - eps),
    cumulative_logit = qlogis(prob_kumulatif)
  )

ggplot(grid_ord2_cumlogit,
       aes(x = BMI, y = cumulative_logit, color = batas_kumulatif)) +
  geom_line(linewidth = 1.25) +
  scale_color_manual(values = c(
    "P(Y <= Tidak_Diabetes)" = "#2f7f73",
    "P(Y <= Prediabetes)"    = "#5568b8"
  )) +
  labs(
    title    = "Parallel Lines pada Model Ordinal",
    subtitle = "Garis yang sejajar mengindikasikan asumsi proportional odds terpenuhi.",
    x        = "BMI",
    y        = "Logit peluang kumulatif",
    color    = "Batas kumulatif"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3.4.2. BRANT TEST
# Uji formal proportional odds per variabel
# H0: asumsi proportional odds terpenuhi
# ------------------------------------------------------------
brant::brant(fit_ord2)

# ------------------------------------------------------------
# 3.4.3. NOMINAL TEST (ordinal::clm)
# Uji global proportional odds via likelihood ratio
# H0: proportional odds terpenuhi untuk semua prediktor
# ------------------------------------------------------------
fit_clm2 <- ordinal::clm(
  diabetes ~ HighBP + HighChol + BMI + Smoker + HvyAlcoholConsump +
    PhysActivity + AnyHealthcare + Age + Sex + Income + MentHlth,
  data = d2,
  link = "logit"
)

summary(fit_clm2)
ordinal::nominal_test(fit_clm2)

# ============================================================
# BAB 3.5. UJI SIGNIFIKANSI MODEL
# ============================================================

# ------------------------------------------------------------
# 3.5.1. LIKELIHOOD RATIO TEST (LRT)
# Bandingkan model penuh vs model null (intersep saja)
# H0: semua koefisien prediktor = 0
# ------------------------------------------------------------
fit_null <- MASS::polr(
  diabetes ~ 1,
  data   = d2,
  method = "logistic",
  Hess   = TRUE
)

lrt_result <- anova(fit_null, fit_ord2)
lrt_result

# Tampilkan nilai LRT secara eksplisit
lrt_stat <- -2 * (logLik(fit_null) - logLik(fit_ord2))
lrt_df   <- fit_ord2$edf - fit_null$edf
lrt_pval <- pchisq(as.numeric(lrt_stat), df = lrt_df, lower.tail = FALSE)

cat("\n=== Likelihood Ratio Test ===\n")
cat("Chi-square =", round(as.numeric(lrt_stat), 4), "\n")
cat("df         =", lrt_df, "\n")
cat("p-value    =", format(lrt_pval, scientific = TRUE), "\n")

# ------------------------------------------------------------
# 3.5.2. WALD TEST
# Uji signifikansi tiap koefisien prediktor
# H0: koefisien_i = 0
# ------------------------------------------------------------
wald_table <- result_ord2 %>%
  dplyr::filter(jenis == "Koefisien") %>%
  dplyr::mutate(
    Wald_stat  = round(t_value^2, 4),
    Signifikan = ifelse(p_value < 0.05, "Ya *", "Tidak")
  ) %>%
  dplyr::select(
    parameter,
    estimate,
    std_error,
    Wald_stat,
    p_value,
    Signifikan
  )
wald_table %>%
  kable(
    digits = 4,
    caption = "Hasil Wald Test untuk setiap prediktor",
    col.names = c(
      "Prediktor",
      "Koefisien",
      "SE",
      "Wald Stat",
      "p-value",
      "Signifikan"
    )
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed","responsive"),
    full_width = FALSE
  )
result_ord2 %>%
  dplyr::select(parameter, estimate, estimate_slide) %>%
  head(10)

# ============================================================
# BAB 3.6. ANALISIS ODDS RATIO
# ============================================================
or_table <- result_ord2 %>%
  dplyr::filter(jenis == "Koefisien") %>%
  dplyr::mutate(
    OR      = round(OR_polr, 4),
    CI_low  = round(CI_low_polr, 4),
    CI_high = round(CI_high_polr, 4),
    p_value = ifelse(p_value < 0.001,
                     "<0.001",
                     sprintf("%.4f", p_value)),
    Signifikan = ifelse(t_value^2 > qchisq(0.95,1),
                        "Ya *","Tidak")
  ) %>%
  dplyr::select(
    parameter,
    OR,
    CI_low,
    CI_high,
    p_value,
    Signifikan
  )

or_table
or_table %>%
  kableExtra::kable(
    caption = "Odds Ratio dan Interval Kepercayaan 95%",
    col.names = c(
      "Prediktor",
      "OR",
      "CI Lower",
      "CI Upper",
      "p-value",
      "Signifikan"
    )
  ) %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped","hover","condensed","responsive"),
    full_width = FALSE
  )

# --- Plot OR + CI ---
or_table %>%
  ggplot(aes(x = reorder(parameter, OR), y = OR)) +
  geom_point(size = 3, color = "#2f7f73") +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high),
                width = 0.25, color = "#2f7f73") +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  coord_flip() +
  labs(
    title    = "Odds Ratio dan Confidence Interval 95%",
    subtitle = "OR > 1 meningkatkan peluang kategori lebih parah; OR < 1 menurunkan.",
    x        = NULL,
    y        = "Odds Ratio"
  ) +
  theme_minimal()

# ============================================================
# BAB 3.7. EVALUASI PERFORMA KLASIFIKASI
# ============================================================

# ------------------------------------------------------------
# 3.7.1. PREDIKSI PROBABILITAS
# ------------------------------------------------------------
pred_prob_ord2 <- predict(fit_ord2, type = "probs")
head(pred_prob_ord2)

d2_pred_ord <- d2 %>%
  bind_cols(as.data.frame(pred_prob_ord2)) %>%
  mutate(prediksi = predict(fit_ord2, type = "class"))

head(d2_pred_ord[, c("diabetes","Tidak_Diabetes","Prediabetes","Diabetes","prediksi")])

# --- Visualisasi prediksi probabilitas (variasi BMI) ---
grid_ord2_plot <- grid_ord2 %>%
  bind_cols(as.data.frame(grid_prob_ord2)) %>%
  tidyr::pivot_longer(
    cols      = c("Tidak_Diabetes", "Prediabetes", "Diabetes"),
    names_to  = "diabetes",
    values_to = "probabilitas"
  )

ggplot(grid_ord2_plot, aes(x = BMI, y = probabilitas, color = diabetes)) +
  geom_line(linewidth = 1.25) +
  scale_y_continuous(labels = percent_format()) +
  scale_color_manual(values = c("#2f7f73","#5568b8","#c0392b")) +
  labs(
    title    = "Prediksi Probabilitas Status Diabetes",
    subtitle = "Profil: HighBP = Ya, HighChol = Ya, PhysActivity = Ya, Sex = Laki-laki, Age = 7, Income = 6.",
    x        = "BMI",
    y        = "Probabilitas prediksi",
    color    = "Status Diabetes"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# 3.7.2. CONFUSION MATRIX
# ------------------------------------------------------------
conf_ord2 <- table(
  Aktual   = d2_pred_ord$diabetes,
  Prediksi = d2_pred_ord$prediksi
)

conf_ord2

# Tampilkan sebagai tabel yang lebih rapi
as.data.frame.matrix(conf_ord2) %>%
  tibble::rownames_to_column("Aktual \\ Prediksi") %>%
  kable(caption = "Confusion Matrix: Aktual vs Prediksi") %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = FALSE
  )

# ------------------------------------------------------------
# 3.7.3. AKURASI KLASIFIKASI
# ------------------------------------------------------------
accuracy_ord2 <- sum(diag(conf_ord2)) / sum(conf_ord2)

# Akurasi per kelas
sensitivity_perclass <- diag(conf_ord2) / rowSums(conf_ord2)
precision_perclass   <- diag(conf_ord2) / colSums(conf_ord2)

cat("\n=== Evaluasi Performa Klasifikasi ===\n")
cat("Akurasi keseluruhan :", round(accuracy_ord2, 4), "\n\n")
cat("Sensitivitas per kelas:\n")
print(round(sensitivity_perclass, 4))
cat("\nPresisi per kelas:\n")
print(round(precision_perclass, 4))

# Tabel ringkasan performa
data.frame(
  Kelas       = names(sensitivity_perclass),
  Sensitivitas= round(sensitivity_perclass, 4),
  Presisi     = round(precision_perclass, 4)
) %>%
  kable(
    caption   = paste0("Performa klasifikasi per kelas (Akurasi = ",
                       round(accuracy_ord2*100, 2), "%)"),
    row.names = FALSE
  ) %>%
  kable_styling(
    bootstrap_options = c("striped","hover","condensed"),
    full_width = FALSE
  )

