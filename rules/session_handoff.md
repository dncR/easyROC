# Oturum Devri - easyROC

Last updated: 2026-04-17
Status: active

Bu dosya oturumlar arasi baglam devri icin kullanilir.

## Guncel Odak

- R-first modernizasyon planinin (workplan + sprint plani) kural tabanli yurutulmesi.
- Monolitik yapiyi kontrollu ve testli sekilde moduler hedef mimariye tasima.
- Sprint 7 `EASY-047` post-release izleme penceresinin tamamlanmasi (60 dk health/log tarama + incident/hotfix karari).

## Son Tamamlananlar

- Modernizasyon ana plani olusturuldu.
- Sprint ve issue bazli uygulama plani cikartildi.
- easyROC icin yaln kural protokolu kurulumu baslatildi.
- "Simdi cerceve sonra icerik" yaklasimi resmi kural haline getirildi (OP-009).
- Sprint 0 baseline artefaktlari olusturuldu (`docs/baseline.md`, referans cikti seti, teknik borc envanteri, branch/PR kurallari).
- Sprint 1 P0 adiminda `EASY-006/007/008` kapsami uygulandi: status/event uyumlulugu duzeltildi, `:::` kullanimlari temizlendi, temel input dogrulama kontrolleri ve birim testler eklendi.
- Sprint 1 `EASY-010` tamamlandi: dosya yukleme dogrulama katmani eklendi, UI uzerinden anlamli hata mesaji yansitildi ve ilgili birim testleri yazildi.
- Sprint 1 `EASY-009` tamamlandi: artik UI'da sunulmayan "paste data" akisina ait erisilemeyen kod bloklari temizlendi.
- Sprint 1 `EASY-012` tamamlandi: geriye uyumluluk etkileri `docs/compatibility.md` dosyasinda resmi olarak dokumante edildi.
- Sprint 2 `EASY-013` tamamlandi: `app.R` giris noktasi eklendi ve uygulamanin `app.R` uzerinden baslatilabilirligi dogrulandi.
- Sprint 2 `EASY-014` tamamlandi: `R/` altinda hedef moduller icin `mod_*` iskelet dosyalari olusturuldu.
- Sprint 2 `EASY-015` tamamlandi: veri yukleme akisi `mod_data_upload` modulune tasindi; UI+server tarafinda moduler baglanti kuruldu.
- Sprint 2 `EASY-016` tamamlandi: paylasilan reactive state kontrati (`createSharedState`/`validateSharedState`) tanimlandi ve data upload modulu ile ana server bu kontrat uzerinden baglandi.
- Sprint 2 `EASY-017` tamamlandi: indirme handler'lari `mod_downloads` yardimcilari ile tek kayıt noktasinda orkestre edildi (`create_download_handler_spec` + `register_download_handlers`).
- Sprint 2 `EASY-018` tamamlandi: `docs/architecture.md` taslagi olusturuldu; mevcut snapshot + hedef mimari + modul kontratlari yazili hale getirildi.
- Sprint 2 `EASY-019` tamamlandi: baseline referans ciktisina dayali refactor smoke testleri eklendi; `app.R` boot + ROC/pAUC/cutoff ana akislarinin davranis esdegerligi testle guvenceye alindi.
- Sprint 3 `EASY-020` tamamlandi: ROC hesaplama/akıs katmani `mod_roc_analysis` modülüne taşındı; `server.R` ROC stats/coordinates/comparison ve ilgili download/pAUC akışları modül çıktıları üzerinden çalışacak şekilde güncellendi.
- Sprint 3 `EASY-021` tamamlandi: pAUC sorumlulugu `mod_roc_analysis` modülünden ayrıştırılıp `mod_partial_auc` modülüne taşındı; `resultPAuc` çıktısı yeni modül üzerinden servis edilmeye başlandı ve baseline eşdeğerliği testle doğrulandı.
- Sprint 3 `EASY-022` tamamlandi: cut-point hesaplama kontratı `mod_cut_points` modülüne taşındı; `server.R` cut-off sonuç üretimi ve ilgili download/plot akışlarında modül reaktifleri kullanılmaya başlandı. Cut-point baseline eşdeğerliği yeni kontrat testleriyle doğrulandı.
- Sprint 3 `EASY-023` tamamlandi: sample size hesaplama akışı `mod_sample_size` modülüne taşındı; UI çıktısı ve indirme akışı modül kontratı üzerinden çalışacak şekilde güncellendi. Üç sample size modu için baseline eşdeğerliği testle doğrulandı.
- Sprint 3 `EASY-024` tamamlandi: ROC ve cut-point plot option üretimi `R/plot_options_service.R` altında ortak servis fonksiyonlarına taşındı; `server.R` tarafındaki tekrarlı option builder blokları sadeleştirildi ve servis kontrat testleri eklendi.
- Sprint 3 `EASY-025` tamamlandi: legacy `server.R` icindeki modulerlesmis alanlara ait dogrudan domain source/library bagimliliklari temizlendi; cut-point ROC koordinat akisi modul kontrati uzerinden beslendi. Monolitik bagimlilik temizligi icin statik test (`test-server-bootstrap-deps.R`) eklendi.
- Sprint 3 `EASY-026` tamamlandi: Faz-2 migration notu `docs/migration-phase2.md` olarak olusturuldu; gecis ozeti, kanitlar, kalan legacy footprint ve Sprint 4 giris checklist'i yazili hale getirildi.
- Sprint 4 `EASY-027` tamamlandi: `renv` proje aktivasyonu (`.Rprofile`, `renv/activate.R`) ve `renv.lock` olusturuldu; eksik runtime paketleri (`pROC`, `plyr`, `OptimalCutpoints`) lockfile'a dahil edildi. Test suiti tekrar calistirilip yesil dogrulandi.
- Sprint 4 `EASY-028` tamamlandi: domain test kapsami `tests/testthat/test-domain-core-functions.R` ile genisletildi. `rocdata` yon/advanced dallari, `compute_roc_comparisons` 3-marker adj. p-value dali, `parametricROC`, sample size input guardlari ve `printCutOff2` format kontrati test altina alindi.
- Sprint 4 `EASY-029` tamamlandi: `shinytest2` ile kritik UI smoke otomasyonu eklendi (`tests/testthat/test-shiny-critical-flows.R`). Data upload, ROC stats, Partial AUC, Cut points ve Sample size akislari AppDriver uzerinden dogrulandi. `tests/shinytest2/app.R` fixture launcher ile app.R/server.R cift-entrypoint kisiti asildi. `renv.lock` shinytest2 + chromote zinciri ile guncellendi.
- Sprint 4 `EASY-030` tamamlandi: lint + statik kontrol kapilari eklendi (`scripts/lint.R`, `scripts/static_checks.R`). Lint adimi icin dusuk-gurultulu migration profili tanimlandi, static check kurallari netlestirildi ve kalite kapilari dokumante edildi (`docs/process/quality_gates.md`).
- Sprint 4 `EASY-031` tamamlandi: GitHub Actions tek pipeline yapisi `.github/workflows/ci.yml` altinda birlestirildi; `lint` (lint + static checks) ve `test` (`tests/testthat.R`) job'lari PR/push tetiklerinde calisacak sekilde tanimlandi. Required check adlari (`CI / lint`, `CI / test`) dokumante edildi.
- Sprint 4 `EASY-032` tamamlandi: test raporu `docs/test-report.md` olarak yayimlandi. Lint/static/test komut ciktilari, test envanteri, kritik UI akis kaniti ve residual riskler rapora islenerek Sprint 4 kalite kapanisi yazili hale getirildi.
- Sprint 5 `EASY-033` tamamlandi: `ui.R` icinde `bslib::bs_theme()` tabanli tema katmani (`easyroc_theme`) devreye alindi ve uygulama kabugu `fluidPage(theme = ...)` yapisina tasinarak modern tema tum sekmelere uygulanir hale getirildi.
- Sprint 5 `EASY-034` tamamlandi: uzun sidebar formlarinda adim bazli akis netlestirildi; ROC tarafinda advanced/plot bloklari accordion yapisina alindi, Cut points ve Sample size formlarina yonlendirici adim/hint metinleri eklendi.
- Sprint 5 `EASY-035` tamamlandi: kullaniciya donen validasyon/hata mesajlari eylem odakli hale getirildi (`data_input_utils`, `mROC`, sample size guard mesajlari). `mod_sample_size` tarafinda hata durumlari icin okunabilir yonlendirme satirlari eklendi ve cikti/download akisinda sert hata yerine acik mesaj donusu saglandi.
- Sprint 5 `EASY-036` tamamlandi: mobil responsive duzenlemeler `ui.R` seviyesinde devreye alindi. Sidebar form kontrolleri mobilde tam-genislik calisacak sekilde duzenlendi, tab navigasyonu wrap destegi kazandi, data table yatay scroll davranisi eklendi ve manuel/icerik gorselleri responsive hale getirildi.
- Sprint 5 `EASY-037` tamamlandi: erisilebilirlik icin klavye ve etiket iyilestirmeleri uygulandi. `ui.R` seviyesinde skip-link + belirgin focus stili eklendi, bos etiketli secimler icin ekran-okuyucu etiketleri tanimlandi, temel gorsellere `alt` metinleri verildi ve upload hata mesaji `role=alert` + `aria-live` ile duyurulur hale getirildi.
- Sprint 5 `EASY-038` tamamlandi: UI kararlarini standartlastiran yonerge dokumani yayinlandi (`docs/ui-guidelines.md`). Tema/token kullanimi, form/mesaj kurallari, responsive beklentiler, erisilebilirlik taban cizgisi ve PR checklist'i yazili hale getirildi.
- Sprint 6 `EASY-039` tamamlandi: deployment hedef mimarisi `docs/deployment-target-architecture.md` ile kabul edildi. Container-first topoloji, staging/prod ayrimi, immutable image promote modeli ve rollback sinirlari netlestirildi.
- Sprint 6 `EASY-049` tamamlandi: Rule Refactor Review checkpoint'i uygulandi. `keep/deprecate/revise` kararlari `rules/current/rule_refactor_review_2026-04-17.md` ile kayit altina alindi; OP-009 deprecated yapildi, OP-011 aktive edildi, CR-003 steady-state icin revize edildi.
- Sprint 6 `EASY-040` tamamlandi: container/calistirma recetesi standardize edildi (`Dockerfile`, `docker-compose*.yml`, `.env.example`, `scripts/deploy_compose.sh`, `docs/container-runtime-recipe.md`). Staging icin build+run, production icin rebuildsiz promote akisi yazili hale getirildi.
- Sprint 6 `EASY-041` tamamlandi: ortam degiskeni ve secret yonetimi standardize edildi (`docs/env-secret-management.md`, `scripts/validate_env.sh`, `.env.staging.example`, `.env.production.example`, `.env.secrets.example`). Deploy scriptine env validation kapisi eklendi.
- Sprint 6 `EASY-042` tamamlandi: loglama ve hata izleme taban cizgisi devreye alindi (`R/logging_utils.R`, `docs/observability.md`). Session acilis/kapanis, upload parse hatalari ve sample size hesaplama hatalari structured log olarak izlenir hale getirildi.
- Sprint 6 `EASY-043` tamamlandi: health-check/readiness kontrati devreye alindi (`scripts/healthcheck.R`, compose healthcheck ayari, `docs/healthchecks.md`). Deploy scripti container `healthy` durumunu bekleyecek sekilde guncellendi.
- Sprint 6 `EASY-044` tamamlandi: deployment/runbook dokumanlari finalize edildi (`docs/deployment.md`, `docs/runbook.md`). Staging/prod deploy, verification ve rollback operasyon adimlari tek bir standarda baglandi.
- Sprint 7 `EASY-045` tamamlandi: release checklist finalize edildi (`docs/release-checklist.md`). Production release oncesi zorunlu gate, kanit ve go/no-go onay adimlari yazili hale getirildi.
- Sprint 7 `EASY-046` tamamlandi: release adayi (`easyroc-release:rc-20260417193256`) staging'de build+healthcheck ile dogrulandi ve production ortamina promote edildi. Release kaniti `docs/release-evidence-rc-20260417193256.md` dosyasina eklendi.

## Netlesmemis Kararlar

- Scope etiket sozlugunun sabitlenme seviyesi (kisa liste mi, serbest etiket mi).
- Kural/workplan metadata lint kontrolunun otomatiklestirilip otomatiklestirilmeyecegi.

## Acik Riskler

- Refactor surecinde davranis esdegerligi kaybi riski.
- Test kapsamini yeterli hizda artiramama riski.
- Workplan ve kural dokumanlarinda drift riski.

## Bir Sonraki Onerilen Adim

1. `docs/post-release-monitoring-rc-20260417193256.md` icindeki 60 dk checkpoint'i tamamla ve EASY-047'yi kapat.
2. Repository branch protection ayarinda `CI / lint` ve `CI / test` check'lerini required olarak isaretle.
3. `EASY-048` icin changelog ve kapanis raporu girislerini release kanitlariyla birlikte hazirla.
