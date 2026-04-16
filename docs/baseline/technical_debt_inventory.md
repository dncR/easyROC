# easyROC Teknik Borc Envanteri (Sprint 0)

Last updated: 2026-04-17
Owner: team

## P0

| ID | Konu | Etki | Kanit |
| --- | --- | --- | --- |
| TD-P0-001 | `tagHealthy` icinde `as.numeric(input$valueStatus)` kullanimi karakter/faktor statulerde bos veya hatali sonuc uretebilir. | Cut-point sonucunun yanlis/eksik olmasi | `server.R:764-767` |
| TD-P0-002 | `mROC` icindeki marker kontrolu ters ve uyari mesaji yanlis; olasi null marker durumunda korunaksiz. | Hata yonetimi ve veri dogrulama zafiyeti | `R/mROC.R:10-18` |
| TD-P0-003 | `pROC`, `dplyr`, `plyr` icin `:::` kullanimlari var; non-exported API riski. | Paket guncellemelerinde kirilma riski | `R/pAUC.R:8,14`, `R/mROC.R:46`, `server.R:559,596,599` |
| TD-P0-004 | `rocdata` hesaplamalarinda deprecation warning ureten vektor/array islemleri var. | Gelecek R surumlerinde davranis kirilmasi riski | `R/rocdata.R` (AUC/SE hesap bloklari; baseline calistirma uyarilari) |
| TD-P0-005 | Test/CI altyapisi yok (unit, integration, regression). | Regresyonlarin gec fark edilmesi | Repo genel durum (tests/ci dosyasi yok) |

## P1

| ID | Konu | Etki | Kanit |
| --- | --- | --- | --- |
| TD-P1-001 | `source(\"R/rocdata.R\")` iki kez cagriliyor. | Gereksiz yukleme ve bakim karmasasi | `server.R:6`, `server.R:11` |
| TD-P1-002 | ROC plot indirmede boyut icin `myheightCutoff/mywidthCutoff` kullaniliyor. | ROC plot indirme boyutlarinin baglamsal tutarsizligi | `server.R:481` |
| TD-P1-003 | UI'da `dataInput == '3'` paneli mevcut ama secimlerde yok; server tarafi da yorum satiri. | Oglu kod/yarim akis | `ui.R:50`, `ui.R:20`; `server.R:52-67` |
| TD-P1-004 | README kurulum anlatimi eski versiyon odakli (`shiny 0.10.1`, `runGitHub`). | Onboarding ve tekrarlanabilirlik riski | `README.md:33`, `README.md:41` |
| TD-P1-005 | `require(dplyr)` ve global paket yuklemelerine bagli cagrilar yaygin. | Fonksiyon seviyesinde bagimlilik izolasyonu zayif | `R/mROC.R:8`, `server.R:16-18` |

## P2

| ID | Konu | Etki | Kanit |
| --- | --- | --- | --- |
| TD-P2-001 | Eski Shiny UI paradigmasi (`pageWithSidebar`) ve buyuk tek parca dosyalar. | Gelistirme hizi ve bakim maliyeti | `ui.R` (~1200 satir), `server.R` (~1240 satir) |
| TD-P2-002 | Sabit metinler/yardim metinleri ve UI icerigi merkezilesmis degil. | Icerik bakim zorlugu | `ui.R` genel |

## Onerilen Siralama

1. P0 bugfix + test iskeleti (Sprint 1)
2. P1 stabilizasyon temizlikleri (Sprint 1/2)
3. P2 mimari ve UI modernizasyonu (Sprint 2+)
