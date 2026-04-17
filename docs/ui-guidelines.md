# easyROC UI Guidelines

Last updated: 2026-04-17
Owner: team
Status: published
Related Sprint Issues: EASY-033, EASY-034, EASY-035, EASY-036, EASY-037, EASY-038

## 1) Scope

Bu dokuman, easyROC arayuzunun tutarli, erisilebilir ve bakimi kolay sekilde gelistirilmesi icin uygulama kurallarini tanimlar.

Kapsam:

- `ui.R` icindeki sidebar/main panel bilesenleri
- Form akislari, yardim metinleri, hata/validasyon mesajlari
- Responsive davranis kurallari
- Erisilebilirlik (A11y) taban cizgisi

## 2) Design Direction

- Arayuz temasi `bslib::bs_theme()` uzerinden yonetilir.
- Renk sistemi sadedir: guven veren mavi-yesil palet, yuksek okunabilirlik.
- Esas hedef: istatistiksel akislari teknik olmayan kullanici icin adim adim tamamlanabilir yapmak.

## 3) Theme and Tokens

Tema tanimi `ui.R` icindeki `easyroc_theme()` fonksiyonunda tutulur.

Temel tokenlar:

- `primary`: ana aksiyon rengi
- `secondary`: ikincil vurgu
- `success/info/warning/danger`: durum renkleri
- `bg/fg`: zemin ve metin kontrasti

Kural:

- Yeni renk ekleme yerine mevcut tokenlar kullanilmali.
- Kontrast dusuren acik-gri metinlerden kacinilmali.

## 4) Layout Rules

- Ana yapi: `fluidPage` + `sidebarLayout`.
- Sol panel (`.easyroc-sidebar`): giris/ayar/form akislari.
- Sag panel: sonuc tablolari, grafikler ve cikti alanlari.

Form organizasyonu:

- Uzun akislarda adim basliklari (`h5("1. ...")`) zorunludur.
- Gerekli durumlarda `bslib::accordion` ile bilgi yogunlugu asamali gosterilmelidir.
- Bos label kullanilmamali; gorunur label istenmiyorsa ekran okuyucuya ozel label verilmelidir.

## 5) Form and Input Patterns

Tercih edilen kaliplar:

- Secim alanlari: `selectizeInput` / `selectInput`
- Ikili durumlar: `checkboxInput`
- Sayisal parametreler: `numericInput` / `sliderInput`
- Alt grup secimleri: net ve eylem odakli secenek adlari

Kural:

- Input `id` degisiklikleri davranis degisikligi sayilir; test guncellemesi olmadan yapilmamalidir.
- Form etiketleri "ne secilecegini" acik anlatmalidir (`Marker`, `Cut-off method`, vb.).

## 6) Message and Validation UX

Mesaj dili:

- Kisa, eylem odakli, cozum onerir format:
  - Ne oldu?
  - Neden olabilir?
  - Kullanici ne yapmali?

Ornek yapi:

- "Only one column was detected. Delimiter selection may be incorrect. Try a different delimiter."

Kural:

- Teknik hata mesajlari dogrudan son kullaniciya aynen gosterilmez.
- Kullaniciya donen tum hata metinleri yeniden yazilarak yonlendirici hale getirilir.

## 7) Accessibility Baseline

Uygulanan taban kurallar:

- Skip link: klavye ile dogrudan ana icerige gecis
- Belirgin focus gorunurlugu (`:focus-visible`)
- Ekran okuyucuya ozel label (`visually-hidden`)
- Kritik hata mesajlarinda `role="alert"` + `aria-live`
- Temel gorsellerde `alt` metni

Devam eden kural:

- Yeni eklenen tum gorseller `alt` metni ile gelmelidir.
- Bos label ile yeni input eklenmemelidir.

## 8) Responsive Rules

Mobil hedef: `max-width: 768px`

Beklenen davranis:

- Sidebar form elemanlari tam genislik
- Tab navigasyonu satira sarilabilir
- DataTable konteyneri yatay kaydirma destekli
- Download butonlari mobilde blok gorunumlu
- Icerik gorselleri `max-width: 100%`, `height: auto`

## 9) Do / Don't

Do:

- Adim bazli form akisi kullan.
- Ayni isi yapan kontrolleri tek yerde topla.
- Yardim metnini kisa tut, eyleme yonlendir.
- Mobil ve klavye kullanimiyla kontrol et.

Don't:

- Bos label birakma.
- Yalniz renkle anlam tasima.
- Yeni stil kurali eklerken mevcut responsive davranisi bozma.
- Uzun metinli teknik hata ciktisini dogrudan UI'a basma.

## 10) UI Change Checklist (PR)

- [ ] Input etiketleri acik ve tutarli
- [ ] Hata/uyari metinleri eylem odakli
- [ ] Klavye focus gorunur
- [ ] Mobilde kritik akis tamamlanabilir
- [ ] Yeni gorsellerde `alt` metni var
- [ ] `Rscript --vanilla scripts/lint.R` gecti
- [ ] `Rscript --vanilla scripts/static_checks.R` gecti
- [ ] `Rscript --vanilla tests/testthat.R` gecti

