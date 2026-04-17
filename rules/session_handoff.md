# Oturum Devri - easyROC

Last updated: 2026-04-17
Status: active

Bu dosya oturumlar arasi baglam devri icin kullanilir.

## Guncel Odak

- R-first modernizasyon planinin (workplan + sprint plani) kural tabanli yurutulmesi.
- Monolitik yapiyi kontrollu ve testli sekilde moduler hedef mimariye tasima.

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

## Netlesmemis Kararlar

- Scope etiket sozlugunun sabitlenme seviyesi (kisa liste mi, serbest etiket mi).
- Kural/workplan metadata lint kontrolunun otomatiklestirilip otomatiklestirilmeyecegi.

## Acik Riskler

- Refactor surecinde davranis esdegerligi kaybi riski.
- Test kapsamini yeterli hizda artiramama riski.
- Workplan ve kural dokumanlarinda drift riski.

## Bir Sonraki Onerilen Adim

1. Sprint 4 icin `EASY-028` adimina gecip domain fonksiyonlari icin `testthat` kapsamini arttir.
2. `EASY-029` kapsaminda kritik UI akislari icin `shinytest2` test senaryolarini cikar.
3. `EASY-031` icin CI pipeline tanimini (test + lint zorunlulugu) netlestir.
