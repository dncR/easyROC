# easyROC Migration Phase 2 Notes

Last updated: 2026-04-17  
Owner: team  
Status: completed  
Related Sprint Issues: EASY-020, EASY-021, EASY-022, EASY-023, EASY-024, EASY-025, EASY-026

## 1) Scope

Bu dokuman, Phase 2 (Mimari Refactor) kapsaminda yapilan gecisi, kalan teknik noktalarini ve Sprint 4 giris kosullarini kayit altina alir.

## 2) Goal and Outcome

Phase 2 hedefi:

- `ui.R/server.R` icindeki ana islevleri moduler kontratlara tasimak
- Domain fonksiyonlarini UI akisindan ayristirmak
- Tekrarlayan kodlari servis seviyesinde merkezilestirmek

Phase 2 cikti ozeti:

- Ana analiz akislarinin hesaplama kontratlari `mod_*` birimlerine tasindi
- `server.R` icindeki dogrudan legacy source/library bagimliliklari azaltildi
- Davranis esdegerligi baseline + kontrat testleriyle korundu

## 3) Migration Summary

| Issue | Tasinan Sorumluluk | Hedef Birim | Sonuc |
|---|---|---|---|
| EASY-020 | ROC stats/coordinates/comparison hesaplamalari | `R/mod_roc_analysis.R` | Tamamlandi |
| EASY-021 | Partial AUC hesaplamalari | `R/mod_partial_auc.R` | Tamamlandi |
| EASY-022 | Cut-point hesaplama kontrati | `R/mod_cut_points.R` | Tamamlandi |
| EASY-023 | Sample size hesaplama kontrati | `R/mod_sample_size.R` | Tamamlandi |
| EASY-024 | ROC + cut-point plot options builder'lari | `R/plot_options_service.R` | Tamamlandi |
| EASY-025 | Monolitik bootstrap bagimlilik temizligi | `server.R` + modul ic bagimliliklari | Tamamlandi |

## 4) Current Contract Map

- `mod_data_upload`: veri yukleme + status/event secimi + shared state baglama
- `mod_roc_analysis`: ROC istatistik/koordinat/karsilastirma
- `mod_partial_auc`: partial AUC hesaplari
- `mod_cut_points`: optimal cut-point + cut-point ROC koordinat ciktilari
- `mod_sample_size`: uc farkli sample size yontemi
- `mod_downloads`: download spec registry
- `plot_options_service`: ROC/cut-point plot option ureticileri

## 5) Compatibility and Evidence

Referanslar:

- `docs/compatibility.md`
- `docs/architecture.md`
- `docs/baseline/reference_outputs/*`

Kanit:

- `Rscript tests/testthat.R` ile tum test bloklari yesil
- Baseline esdegerlik testleri:
  - ROC/pAUC/cut-point smoke: `test-refactor-smoke-baseline.R`
  - Sample size baseline: `test-mod-sample-size-contract.R`
  - Cut-point baseline: `test-mod-cut-points-contract.R`
- Legacy bootstrap temizligi statik kontrolu:
  - `test-server-bootstrap-deps.R`

## 6) Remaining Legacy Footprint

Phase 2 sonunda halen korunmus (bilincli) noktalar:

- `ui.R` buyuk olcude monolitik (module UI extraction sonraki fazlara birakildi)
- `server.R` icinde observer/render orchestration bloklari halen yogun
- Bazi gorsellestirme kodlari (`cutPointsPlot`, `downloadCutOffPlotPDF`) fonksiyonel ama uzun

Bu noktalar mevcut davranisi korumak icin bilincli olarak Sprint 4+ kapsaminda ele alinacaktir.

## 7) Risks and Guardrails

- Risk: Moduler kontratlar genisledikce drift olusmasi  
  Guardrail: her yeni kontrat icin test + dokumantasyon guncellemesi zorunlu

- Risk: Plot/render kodunda gizli regresyon  
  Guardrail: smoke testlerin korunmasi, kritik akislarda shinytest2 plani (Sprint 4)

- Risk: Bagimlilik yonetimi daginikligi  
  Guardrail: `renv` lock ve CI pipeline onceligi (EASY-027/EASY-031)

## 8) Sprint 4 Entry Checklist

- [x] Ana analiz kontratlari modullere tasindi
- [x] Baseline esdegerligi testlerle korunuyor
- [x] Legacy bootstrap bagimliliklari temizlendi
- [x] `renv.lock` olusturuldu (EASY-027)
- [x] CI/Test pipeline zorunlu hale getirildi (lint kapisi: EASY-030, pipeline: EASY-031)

## 9) Next Actions

1. EASY-042: Loglama ve hata izleme iyilestirmelerini deployment topolojisine gore uygula
