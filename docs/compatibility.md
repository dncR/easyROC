# easyROC Compatibility Notes (Sprint 1)

Last updated: 2026-04-17
Owner: team
Related Sprint Issues: EASY-006, EASY-007, EASY-008, EASY-009, EASY-010, EASY-011, EASY-012

## 1) Scope

Bu dokuman Sprint 1 stabilizasyon degisikliklerinin geriye uyumluluk etkisini kayit altina alir.

Kontrol kapsaminda:

- Domain fonksiyon davranisi
- Parametre/API uyumlulugu
- UI akislarina etkiler
- Test guvencesi

## 2) Compatibility Matrix

| Issue | Degisiklik Ozeti | Etki Tipi | Geriye Uyumluluk Durumu | Aksiyon |
|---|---|---|---|---|
| EASY-006 | `status/event` karakter-faktor uyumlulugu duzeltildi | Davranis duzeltmesi | Compatible (bugfix) | Yok |
| EASY-007 | `:::` cagrilari public API kullanimina tasindi | Ic implementasyon | Compatible | Yok |
| EASY-008 | Hatali input dogrulama kosullari duzeltildi | Davranis duzeltmesi | Compatible (daha erken/temiz hata) | Yok |
| EASY-009 | UI'da artik sunulmayan "paste data" ile ilgili olumsuz kodlar temizlendi | Olumsuz kod temizligi | Compatible (kullaniciya acik akista degisiklik yok) | Yok |
| EASY-010 | Dosya yukleme dogrulama katmani eklendi, anlamli hata mesajlari gosteriliyor | UX + dogrulama sikilastirma | Compatible (invalid girdide daha erken durdurma) | Yok |
| EASY-011 | Kritik bugfix alanlari test altina alindi | Test altyapisi | Compatible | Yok |

## 3) API ve Davranis Notlari

### 3.1 mROC arguman uyumlulugu

- `event` ve `eventValue` birlikte desteklenir.
- Mevcut cagri sekilleri korunmustur.

### 3.2 Status tipleri

- `numeric`, `factor`, `character` tipleri icin daha tutarli isleme alinmistir.
- Hedeflenen etki: onceki hata veren durumlarin bugfix ile calismasi.

### 3.3 Dosya yukleme

- Gecersiz/uyumsuz dosyalar artik anlamli hata mesaji ile reddedilir.
- Gecerli dosya akisinda beklenen tablo davranisi korunur.
- Dosya boyutu limiti (`30MB`) davranisi korunmustur.

## 4) Potentially Visible Changes

Asagidaki farklar kullanici tarafinda gorulebilir, ancak bunlar breaking degildir:

- Gecersiz upload durumlarinda artik sessizce devam etmek yerine acik hata mesaji gosterilir.
- Delimiter/header uyumsuzluklari daha erken yakalanir.

## 5) Test Evidence

Sprint 1 sonu itibariyla asagidaki test bloklari yesildir:

- `tests/testthat/test-mroc-input-validation.R`
- `tests/testthat/test-pauc-and-status-utils.R`
- `tests/testthat/test-data-input-utils.R`

Calistirma komutu:

- `Rscript tests/testthat.R`

## 6) Open Items

- Sprint 2+ refactor surecinde API degisimi olursa bu dosya revize edilmelidir.
- Faz-2 migration notlari olustugunda (`docs/migration-phase2.md`) bu dokumanla capraz referans verilmelidir.
