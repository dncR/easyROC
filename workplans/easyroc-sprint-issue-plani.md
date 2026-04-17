Workplan ID: WP-EASYROC-SPRINT-001
Status: active
Scope: planning, sprint, issue-tracking, delivery, governance
Owner: team
Last updated: 2026-04-17

# easyROC Sprint ve Issue Planı (R-First)

Referans plan: `workplans/easyroc-modernizasyon-r-first.md`  
Plan başlangıcı: 2026-04-20 (öneri)  
Sprint süresi: 2 hafta (Sprint 0 ve Sprint 7: 1 hafta)

## 1. Planlama Kuralları

- Öncelik: `P0 > P1 > P2`
- Puanlama: Fibonacci (`1, 2, 3, 5, 8`)
- Her issue için zorunlu alanlar: kapsam, kabul kriteri, test notu, bağımlılık
- DoD (Definition of Done):
  - Kod review tamam
  - Testler yeşil
  - Dokümantasyon güncel
  - Geriye dönük uyumluluk kontrol edildi

## 2. Sprint Takvimi (Öneri)

| Sprint | Süre | Hedef |
|---|---|---|
| Sprint 0 | 1 hafta | Baseline ve envanter |
| Sprint 1 | 2 hafta | Stabilizasyon ve kritik bugfix |
| Sprint 2 | 2 hafta | Modüler mimariye geçiş (temel) |
| Sprint 3 | 2 hafta | Modüler mimari tamamlama |
| Sprint 4 | 2 hafta | Test, kalite ve CI |
| Sprint 5 | 2 hafta | UI/UX modernizasyonu |
| Sprint 6 | 2 hafta | Dağıtım, operasyon, güvenlik |
| Sprint 7 | 1 hafta | Release ve hypercare başlangıcı |

## 3. Sprint 0 (1 Hafta)

Sprint hedefi: Mevcut sistemin baz çizgisini ölçmek ve teknik riskleri görünür kılmak.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [x] | EASY-001 | Kritik kullanıcı akışlarını çıkar | P0 | 3 | Yok | Akış listesi dokümante edildi |
| [x] | EASY-002 | Referans dataset/çıktı snapshot seti üret | P0 | 5 | EASY-001 | ROC/pAUC/cut-off/sample size referans çıktıları kayıtlı |
| [x] | EASY-003 | Baz performans ölçümü yap | P1 | 3 | EASY-001 | Baseline metrikleri dokümana işlendi |
| [x] | EASY-004 | Teknik borç envanteri (P0/P1/P2) oluştur | P0 | 3 | EASY-001 | Önceliklendirilmiş backlog hazır |
| [x] | EASY-005 | Branch ve PR kurallarını netleştir | P1 | 2 | Yok | Katkı kuralları repo’da yazılı |

Sprint çıkış kriteri: `docs/baseline.md` ve başlangıç backlog’u tamam.

## 4. Sprint 1 (2 Hafta)

Sprint hedefi: Üretimi etkileyebilecek kırılgan noktaları kapatmak.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [x] | EASY-006 | `status/event` karakter-faktör uyum düzeltmesi | P0 | 5 | EASY-004 | Karakter/faktör durumlarda hata yok |
| [x] | EASY-007 | `:::` kullanımını public API ile değiştir | P0 | 5 | EASY-004 | Non-exported çağrı kalmadı |
| [x] | EASY-008 | Hatalı input kontrollerini düzelt | P0 | 3 | EASY-004 | Yanlış koşullar güncellendi |
| [x] | EASY-009 | Ölü/yarım akışları temizle | P1 | 3 | EASY-004 | Kullanılmayan yol kalmadı |
| [x] | EASY-010 | Dosya yükleme doğrulama katmanı ekle | P1 | 5 | EASY-008 | Hatalı dosyalar anlamlı mesajla yakalanıyor |
| [x] | EASY-011 | Kritik bugfix birim testlerini ekle | P0 | 5 | EASY-006, EASY-007 | Bugfix’ler testle korunuyor |
| [x] | EASY-012 | Geriye uyumluluk notlarını yaz | P2 | 2 | EASY-006..011 | `docs/compatibility.md` güncel |

Sprint çıkış kriteri: Bilinen P0/P1 bugların kapanması.

## 5. Sprint 2 (2 Hafta)

Sprint hedefi: Modüler mimari iskeletini ayağa kaldırmak.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [x] | EASY-013 | `app.R` giriş noktası oluştur | P0 | 3 | EASY-012 | Uygulama `app.R` üzerinden çalışıyor |
| [x] | EASY-014 | Modül klasör yapısını kur (`mod_*`) | P0 | 3 | EASY-013 | Standart yapı repo’da hazır |
| [x] | EASY-015 | `mod_data_upload` çıkarımı | P0 | 5 | EASY-014 | Veri yükleme modülü bağımsız çalışıyor |
| [x] | EASY-016 | Ortak reactive state yapısını tanımla | P0 | 5 | EASY-015 | Modüller arası veri akışı net |
| [x] | EASY-017 | `mod_downloads` temelini çıkar | P1 | 3 | EASY-014 | Tek noktadan indirme orkestrasyonu |
| [x] | EASY-018 | Mimari diyagram taslağı | P1 | 2 | EASY-014 | `docs/architecture.md` taslak hazır |
| [x] | EASY-019 | Refactor smoke testleri | P0 | 3 | EASY-015, EASY-016 | Ana akış bozulmadan çalışıyor |

Sprint çıkış kriteri: Veri yükleme + uygulama kabuğu modüler yapıda stabil.

## 6. Sprint 3 (2 Hafta)

Sprint hedefi: Ana işlevleri modüllere taşımak.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [x] | EASY-020 | `mod_roc_analysis` çıkarımı | P0 | 8 | EASY-016 | ROC stats/coordinates/comparison çalışıyor |
| [x] | EASY-021 | `mod_partial_auc` çıkarımı | P0 | 5 | EASY-020 | pAUC sonuçları eşdeğer |
| [x] | EASY-022 | `mod_cut_points` çıkarımı | P0 | 8 | EASY-016 | Cut-off tabı modüler çalışıyor |
| [x] | EASY-023 | `mod_sample_size` çıkarımı | P1 | 3 | EASY-016 | Sample size hesapları modülde |
| [x] | EASY-024 | Plot options ortak servisleştirme | P1 | 5 | EASY-020, EASY-022 | Tekrarlı kod azaltıldı |
| [x] | EASY-025 | Legacy `ui.R/server.R` bağımlılık temizliği | P1 | 3 | EASY-020..023 | Monolitik bağımlılıklar kaldırıldı |
| [x] | EASY-026 | Faz-2 migration notu | P2 | 2 | EASY-025 | `docs/migration-phase2.md` hazır |

Sprint çıkış kriteri: Tüm ana sekmeler modül yapısında çalışır.

## 7. Sprint 4 (2 Hafta)

Sprint hedefi: Test/kalite hattını üretim standardına çıkarmak.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [x] | EASY-027 | `renv` kurulumu ve lockfile oluştur | P0 | 3 | EASY-026 | `renv.lock` olusturuldu ve repo takibine eklendi |
| [ ] | EASY-028 | `testthat` ile domain testleri | P0 | 8 | EASY-020..023 | Çekirdek fonksiyon test kapsamı arttı |
| [ ] | EASY-029 | `shinytest2` kritik akış testleri | P0 | 8 | EASY-020..023 | Kritik UI akışları otomasyon altında |
| [ ] | EASY-030 | Lint ve statik kontrol ekle | P1 | 3 | EASY-027 | CI’da lint adımı aktif |
| [ ] | EASY-031 | GitHub Actions CI pipeline | P0 | 5 | EASY-028..030 | PR’da test/lint zorunlu |
| [ ] | EASY-032 | Test raporu üret | P1 | 2 | EASY-028..031 | `docs/test-report.md` yayımlandı |

Sprint çıkış kriteri: CI yeşil, kritik regresyon yok.

## 8. Sprint 5 (2 Hafta)

Sprint hedefi: UI/UX ve erişilebilirlik modernizasyonu.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [ ] | EASY-033 | `bslib` tabanlı tema altyapısı | P0 | 5 | EASY-031 | Modern tema tüm sekmelerde aktif |
| [ ] | EASY-034 | Form akışlarını sadeleştir | P1 | 5 | EASY-033 | Uzun input blokları daha anlaşılır |
| [ ] | EASY-035 | Mesajlar ve validasyon UX iyileştirmesi | P1 | 3 | EASY-033 | Hata mesajları eylem odaklı |
| [ ] | EASY-036 | Mobil responsive düzenleme | P0 | 5 | EASY-033 | Mobilde kritik akış tamamlanabiliyor |
| [ ] | EASY-037 | Erişilebilirlik iyileştirmeleri | P1 | 3 | EASY-033 | Label/kontrast/klavye akışı iyileşti |
| [ ] | EASY-038 | UI yönerge dokümanı | P2 | 2 | EASY-033..037 | `docs/ui-guidelines.md` tamam |

Sprint çıkış kriteri: Yeni UI ile kritik akışlarda kullanılabilirlik onayı.

## 9. Sprint 6 (2 Hafta)

Sprint hedefi: Dağıtım ve operasyonel hazırlığın tamamlanması.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [ ] | EASY-039 | Deployment hedef mimarisini kesinleştir | P0 | 3 | EASY-031 | Staging/prod stratejisi net |
| [ ] | EASY-049 | Rule Refactor Review Checkpoint (Post-modernizasyon tetikleyici kapısı) | P0 | 3 | EASY-039 | Sprint 6 başında kural seti gözden geçirildi; geçiş-dönemi kurallar için `keep/deprecate/revise` kararı yazılı hale getirildi; sonuçlar `rules/current/*` ve `rules/current/rules_changelog.md` ile senkronlandı |
| [ ] | EASY-040 | Container/çalıştırma reçetesi standardizasyonu | P0 | 5 | EASY-039 | Tekrarlanabilir deploy adımları var |
| [ ] | EASY-041 | Ortam değişkeni ve gizli bilgi yönetimi | P0 | 3 | EASY-039 | Secret yönetimi dokümante |
| [ ] | EASY-042 | Loglama ve hata izleme iyileştirmeleri | P1 | 5 | EASY-040 | Operasyonel teşhis mümkün |
| [ ] | EASY-043 | Health-check ve readiness kontrolleri | P1 | 3 | EASY-040 | Sistem sağlık doğrulaması var |
| [ ] | EASY-044 | Runbook ve deployment dokümantasyonu | P0 | 3 | EASY-040..043 | `docs/deployment.md` ve `docs/runbook.md` hazır |

Sprint çıkış kriteri: Staging’de release adayı doğrulandı ve Rule Refactor Review checkpoint tamamlandı.

## 10. Sprint 7 (1 Hafta)

Sprint hedefi: Üretim yayını ve kontrollü hypercare.

| Done | ID | Issue | Öncelik | SP | Bağımlılık | Kabul Kriteri |
|---|---|---|---:|---:|---|---|
| [ ] | EASY-045 | Release checklist finalizasyonu | P0 | 2 | EASY-044 | Checklist tamam ve onaylı |
| [ ] | EASY-046 | Production release | P0 | 3 | EASY-045 | Sürüm canlıya alındı |
| [ ] | EASY-047 | Post-release izleme ve hızlı düzeltme | P0 | 5 | EASY-046 | Kritik hata yok, varsa hotfix tamam |
| [ ] | EASY-048 | Changelog ve kapanış raporu | P1 | 2 | EASY-046 | `CHANGELOG.md` + post-release raporu güncel |

Sprint çıkış kriteri: Hypercare başlangıcında P0/P1 açık issue olmaması.

## 11. Issue Şablonu (Kullanıma Hazır)

```md
## Amaç

## Kapsam

## Kabul Kriterleri
- [ ]
- [ ]

## Test Planı
- Birim test:
- Entegrasyon/UI testi:

## Bağımlılıklar

## Notlar
```

## 12. Takip Metrikleri

- Sprint tamamlanan SP / planlanan SP
- Sprint hedef tamamlama oranı
- Açık P0/P1 issue sayısı
- Regresyon test başarısı
- Prod hata oranı (release sonrası)
