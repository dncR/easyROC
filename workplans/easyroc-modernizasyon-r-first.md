Workplan ID: WP-EASYROC-MOD-001
Status: active
Scope: architecture, refactor, testing, deployment, governance
Owner: team
Last updated: 2026-04-16

# easyROC R-First Modernizasyon İş Planı

Oluşturulma tarihi: 2026-04-16  
Yaklaşım: R-first (Shiny ekosistemi içinde modernizasyon, davranış eşdeğerliğini koruyarak)

## 1. Amaç ve Hedefler

- Mevcut `ui.R + server.R` monolit yapısını modüler ve sürdürülebilir bir mimariye taşımak.
- İstatistiksel çıktılarda mevcut sistem ile davranış eşdeğerliğini korumak.
- Bağımlılık, test, dağıtım ve gözlemlenebilirlik süreçlerini üretim seviyesine çıkarmak.
- Kod tabanını yeni özellik eklemeye uygun hale getirmek.

## 2. Başarı Kriterleri (KPI)

- Tüm kritik kullanıcı akışları için otomasyon testi kapsamı: en az `%80` (kritik akış bazında).
- Çekirdek istatistik fonksiyonları için birim test kapsamı: en az `%85`.
- Manuel kurulum yerine tek komutla kurulum/çalıştırma dokümantasyonu.
- Üretim sürümüne çıkış sonrası ilk 30 günde kritik hata (P0/P1): `0`.
- Ana ekran yüklenme süresi ve temel hesaplama yanıt sürelerinde mevcut sürüme göre regresyon olmaması.

## 3. Kapsam

### Dahil

- Kod mimarisinin modülerleştirilmesi (UI modülleri + server modülleri).
- İstatistik motorunun ayrıştırılması ve testlenmesi.
- Modern bağımlılık yönetimi (`renv`).
- Test altyapısı (`testthat`, `shinytest2`).
- CI hattı (lint + test + build doğrulama).
- UI modernizasyonu (`bslib`, responsive düzen, erişilebilirlik iyileştirmeleri).
- Dağıtım standardizasyonu (container ve/veya sunucu dağıtım reçetesi).

### Hariç

- İstatistiksel yöntemlerin bilimsel olarak yeniden tanımlanması.
- Ürünün işlev kapsamını büyük ölçüde değiştirecek yeni analiz modülleri.
- Çok dilli arayüz (istersen ayrı faz olarak planlanabilir).

## 4. Hedef Mimari (R-First)

### Katmanlar

- `app.R` (ince giriş noktası)
- `R/mod_*` (özellik bazlı modüller)
- `R/services/*` (hesaplama orkestrasyonu, input doğrulama)
- `R/domain/*` (ROC, pAUC, cut-off, sample size çekirdek mantık)
- `R/adapters/*` (dosya okuma, dış paket adaptörleri)
- `tests/testthat` (birim testler)
- `tests/testthat/_snaps` ve `tests/shiny` (UI/regresyon testleri)

### Önerilen Modül Bölünmesi

- `mod_data_upload`
- `mod_roc_analysis`
- `mod_partial_auc`
- `mod_cut_points`
- `mod_sample_size`
- `mod_downloads`
- `mod_docs_about`

## 5. Fazlar ve Zaman Planı (12-14 Hafta)

## Faz 0: Başlangıç ve Baz Çizgi (Hafta 1)

### İşler

- Proje kickoff ve kapsam netleştirme.
- Mevcut sistemde kritik akışların envanteri.
- Baz performans ölçümü ve mevcut çıktıların referans dataset ile snapshot alınması.
- Hata envanteri (P0/P1/P2) oluşturulması.

### Teslimatlar

- `docs/baseline.md`
- Referans çıktı seti (ROC stats, pAUC, cut-off, sample size)
- Öncelikli teknik borç listesi

### Çıkış Kriteri

- Kritik akışlar ve mevcut davranış açıkça dokümante edilmiş olmalı.

## Faz 1: Stabilizasyon ve Hızlı Düzeltmeler (Hafta 2-3)

### İşler

- Kırılgan ve hatalı noktaların düzeltilmesi:
  - Faktör/karakter `status` uyumsuzlukları.
  - `:::` kullanımının kaldırılması (public API kullanımına geçiş).
  - Yanlış/yanıltıcı input kontrolleri.
  - Ölü kod ve yarım bırakılmış akışların temizlenmesi.
- Hata üretmeye açık dosya yükleme ve input doğrulama kuralları.

### Teslimatlar

- Stabilizasyon PR’ları
- Geriye dönük uyumluluk notları (`docs/compatibility.md`)

### Çıkış Kriteri

- P0/P1 sınıfı bilinen bloklayıcıların kapatılması.

## Faz 2: Mimari Refactor (Hafta 4-7)

### İşler

- `ui.R/server.R` yapısını modül tabanlı yapıya taşıma.
- Domain fonksiyonlarının UI’dan ayrıştırılması.
- Shared utility ve service katmanı oluşturulması.
- Tekrar eden kod bloklarının merkezileştirilmesi (plot options, download handlers).
- Konfigürasyon yönetimi (`config` benzeri yaklaşım veya sade env tabanlı yapı).

### Teslimatlar

- `app.R` + modüler `R/` yapısı
- Mimari diyagram (`docs/architecture.md`)
- Migration notları (`docs/migration-phase2.md`)

### Çıkış Kriteri

- Tüm mevcut özellikler modüler yapıda çalışır durumda olmalı.

## Faz 3: Bağımlılık ve Test Altyapısı (Hafta 8-9)

### İşler

- `renv` ile bağımlılık kilitleme.
- `testthat` birim testleri (çekirdek hesaplamalar).
- `shinytest2` akış testleri (yükleme, ROC, pAUC, cut-off, sample size).
- Lint ve statik kalite kontrolleri.

### Teslimatlar

- `renv.lock`
- Test dosyaları ve CI pipeline
- Test raporu (`docs/test-report.md`)

### Çıkış Kriteri

- CI’da testlerin yeşil olması, kritik akışlarda regresyon olmaması.

## Faz 4: UI/UX Modernizasyonu (Hafta 10-11)

### İşler

- `bslib` ile modern ve responsive tema.
- Uzun formların kademeli ve anlaşılır hale getirilmesi.
- Yardım metinleri ve hata mesajlarının sadeleştirilmesi.
- Erişilebilirlik iyileştirmeleri (label, kontrast, klavye akışı).

### Teslimatlar

- Güncel UI bileşenleri
- UI kararları dokümanı (`docs/ui-guidelines.md`)

### Çıkış Kriteri

- Mobil ve masaüstünde kritik akışların sorunsuz tamamlanması.

## Faz 5: Dağıtım, Operasyon ve Güvenlik (Hafta 12-13)

### İşler

- Üretim dağıtım standardı (container veya hedef sunucu reçetesi).
- Ortam değişkenleri ve gizli bilgi yönetimi.
- Uygulama loglama ve temel health check uçları.
- Basit operasyon runbook’u hazırlanması.

### Teslimatlar

- Deployment rehberi (`docs/deployment.md`)
- Operasyon runbook (`docs/runbook.md`)

### Çıkış Kriteri

- Staging ortamında uçtan uca doğrulama ve release adayının hazır olması.

## Faz 6: Yayın ve Hypercare (Hafta 14)

### İşler

- Prod release.
- İlk 2 hafta yakın takip (hata, performans, kullanıcı geri bildirimi).
- Gerekli hotfix ve küçük iyileştirmeler.

### Teslimatlar

- `CHANGELOG.md` güncellemesi
- Post-release değerlendirme raporu

### Çıkış Kriteri

- Hypercare sonunda kritik açık issue kalmaması.

## 6. İş Kırılım Yapısı (Epic Bazlı)

- `EPIC-1`: Stabilizasyon ve bugfix
- `EPIC-2`: Modüler mimari dönüşümü
- `EPIC-3`: Test ve kalite güvence
- `EPIC-4`: UI/UX modernizasyonu
- `EPIC-5`: Dağıtım ve operasyon
- `EPIC-6`: Dokümantasyon ve bilgi transferi

Her epic için iş kalemleri `P0/P1/P2` önceliğiyle takip edilmeli.

## 7. Riskler ve Önlemler

| Risk | Etki | Olasılık | Önlem |
|---|---|---|---|
| Refactor sırasında davranış değişimi | Yüksek | Orta | Snapshot + regresyon testleri, fazlı geçiş |
| Paket sürüm uyumsuzlukları | Orta | Yüksek | `renv.lock`, staging doğrulaması |
| UI değişimlerinde kullanıcı adaptasyon sorunu | Orta | Orta | Kademeli UI rollout, kısa kullanım rehberi |
| Tek seferde büyük PR’lar | Yüksek | Orta | Küçük ve bağımsız PR stratejisi |
| Performans regresyonu | Yüksek | Orta | Baz metrik + release öncesi performans karşılaştırması |

## 8. Çalışma Düzeni ve Yönetişim

- Sprint süresi: 2 hafta
- Haftalık teknik durum toplantısı: 1 kez
- Sprint sonunda demo + retro
- PR kuralları:
  - Küçük, bağımsız ve testli PR
  - En az 1 onay
  - CI yeşil olmadan merge yok

## 9. Kabul Kriterleri (Final)

- Tüm mevcut ana özellikler yeni mimaride çalışıyor olmalı.
- İstatistiksel çıktıların referans set ile uyumluluğu doğrulanmalı.
- CI/CD hattı aktif ve zorunlu olmalı.
- Kurulum, geliştirme ve dağıtım dokümantasyonu tamamlanmalı.
- Kod tabanı yeni geliştiricinin devralabileceği netlikte olmalı.

## 10. İlk 2 Sprint İçin Somut Başlangıç Backlog’u

## Sprint 1

- P0: `status` ve `event` eşleştirme hatalarının düzeltilmesi
- P0: `:::` kullanımının temizlenmesi
- P1: input doğrulama ve hata mesajlarının netleştirilmesi
- P1: baseline snapshot test iskeletinin kurulması

## Sprint 2

- P0: `app.R` giriş yapısı ve ilk modül (`data upload`) çıkarımı
- P0: ROC analiz modülünün ayrıştırılması
- P1: ortak download servisinin oluşturulması
- P1: mimari dokümantasyon taslağı

---

Bu plan, riski azaltmak için “önce stabilizasyon, sonra mimari dönüşüm” sıralamasıyla hazırlanmıştır. İstersen bir sonraki adımda bu planı sprint bazlı issue listesine (gün/görev/tahmin) çevirebilirim.
