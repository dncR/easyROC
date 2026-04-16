# easyROC Baseline (Sprint 0)

Last updated: 2026-04-17
Owner: team
Related Sprint Issues: EASY-001, EASY-002, EASY-003, EASY-004, EASY-005

## 1) Scope

Bu belge Sprint 0 baseline ciktilarini toplar:

- Kritik kullanici akis envanteri
- Referans cikti seti
- Basit performans baz olcumu
- Teknik borc envanteri referansi
- Branch/PR calisma kurallari referansi

## 2) Kritik Kullanici Akislari (EASY-001)

1. Veri yukleme: ornek dataset secimi (`mayo`, `pbc`) ve dosya yukleme
2. ROC analizi: nonparametric ROC istatistiklerinin hesaplanmasi
3. ROC koordinatlari ve coklu marker karsilastirma tablosu
4. Partial AUC hesaplamasi (marker bazli)
5. Cut-point belirleme (OptimalCutpoints/Youden dahil)
6. Sample size hesaplamalari (3 farkli yontem)
7. Cikti indirme: ROC stats/coordinates, ROC plot, cut-off raporu

## 3) Referans Cikti Seti (EASY-002)

Konum: `docs/baseline/reference_outputs/`

- `mayo_roc_stats.tsv`
- `mayo_roc_coordinates.tsv`
- `mayo_pauc.tsv`
- `mayo_cutoff_youden.tsv`
- `mayo_parametric_roc_stats.tsv`
- `pbc_roc_stats.tsv`
- `sample_size_single_test.txt`
- `sample_size_two_tests.txt`
- `sample_size_noninferiority.txt`
- `performance_baseline.tsv`

Yeniden uretim scripti:

- `docs/baseline/generate_reference_outputs.R`

## 4) Performans Baz Olcumu (EASY-003)

Kaynak dosya: `docs/baseline/reference_outputs/performance_baseline.tsv`

Ornek sonuclar:

- `mayo_nonparam_roc_stats`: `43.47 ms/iter` (100 tekrar)
- `mayo_pauc`: `0.854 ms/iter` (500 tekrar)
- `mayo_cutoff_youden`: `6.77 ms/iter` (100 tekrar)
- `mayo_parametric_roc`: `0.448 ms/iter` (500 tekrar)

Not:

- Bunlar local environment baz olcumleridir, CI/production ile birebir kiyaslanmaz.

## 5) Teknik Borc Envanteri (EASY-004)

Detay dosya:

- `docs/baseline/technical_debt_inventory.md`

## 6) Branch/PR Kurallari (EASY-005)

Detay dosya:

- `docs/process/branch_pr_rules.md`

## 7) Ortam Bilgisi

- R: `4.5.2`
- Paketler: `shiny 1.12.1`, `plyr 1.8.9`, `dplyr 1.1.4`, `pROC 1.19.0.1`, `OptimalCutpoints 1.1-5`
- Yerel kutuphane yolu: `.Rlib/`

## 8) Bilinen Uyari Notlari

- `R/rocdata.R` hesaplama adimlarinda vektor/array geri donusum uyari mesajlari gorulebiliyor.
- `OptimalCutpoints` tarafinda bazi CI yontemleri icin "Exact method may not be valid" uyarilari cikabiliyor.
- Bu noktalar teknik borc listesinde takip edilmektedir.
