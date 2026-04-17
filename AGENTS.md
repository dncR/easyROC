# Agent Calisma Sozlesmesi (easyROC)

Last updated: 2026-04-17
Status: active

Bu dosya easyROC deposundaki AI agent calisma protokolunun ana kaynagidir.
Template kopyalamasi degil, easyROC'a uyarlanmis yaln bir yonetim modelidir.

## Kural Onceligi

Dokumanlar arasinda celiski olursa:

1. `AGENTS.md`
2. `rules/current/core_rules.md`
3. `rules/current/operational_rules.md`
4. `rules/project_rules_entrypoint.md`
5. `workplans/` altinda `Status: active` ve gorevle `Scope` eslesen workplan dokumanlari
6. `rules/archive/` altindaki legacy referanslar

## Zorunlu Oturum Baslangic Protokolu

Her yeni oturumda:

1. `AGENTS.md` oku.
2. `rules/project_rules_entrypoint.md` oku.
3. Entry-point icinde tanimli sirayi uygula:
   - `rules/current/core_rules.md`
   - `rules/current/operational_rules.md`
   - `rules/session_handoff.md`
4. Kod degisikliginden once `workplans/` altinda `Status: active` + gorevle Scope eslesen planlari oku.

Uygulamaya gecmeden once kisa bir "Rule Summary" ver.

## Workplan Metadata Protokolu

`workplans/` altindaki her planin basinda su alanlar olmalidir:

- Workplan ID
- Status (`active`, `proposed`, `on_hold`, `completed`, `archived`)
- Scope (virgulle ayrilmis etiketler)
- Owner
- Last updated

Secim kurali:

1. Sadece `Status: active` planlar adaydir.
2. `Scope` eslesmesi kucuk harfe normalize edilmis etiket kesisimi ile yapilir.
3. Birden fazla eslesen plan varsa tumu okunur.
4. Eslesen aktif plan yoksa bu durum kisa not ile belirtilir.

## Rule Yasam Dongusu

Her kural kaydinda asgari alanlar:

- Rule ID
- Status (`active`, `frozen`, `deprecated`, `proposed`)
- Owner
- Last Review
- Change Policy (`strict` veya `flexible`)

## Degistirme Politikasi

- `strict` kurallari acik kullanici onayi olmadan degistirme.
- `flexible` kurallar proje ihtiyacina gore guncellenebilir.
- Her kural degisikliginde su dosyalari birlikte guncelle:
  - `rules/current/rule_registry.md`
  - `rules/current/rules_changelog.md`

## easyROC Guvence Kapilari

- Istatistiksel hesap mantigi degisirse test ve referans cikti karsilastirmasi zorunlu.
- Kritik akislarda regresyon riski varsa kod degisikligi tek basina birakilmaz; test/dokuman birlikte guncellenir.
- Acik talep olmadan yikici git veya dosya komutlari kullanilmaz.
- Agent bu depoda otomatik `git commit` veya `git push` islemi yapmaz; commit/push adimini kullanici manuel olarak calistirir.
- Her gorev sonunda background process hijyeni uygulanir: bu oturumda baslatilan kilitli/gereksiz prosesler (ortak kullanilmiyorsa) raporlanir ve manuel kill komutu sunulur. Varsayilan manuel kapanistir; otomatik kill ancak acik kullanici talebi veya kural degisikligi ile uygulanir.

## Post-Modernizasyon Cerceve Notu

- Bu kural seti modernizasyon fazini guvence altina almak icin tasarlanmistir.
- Detayli "post-modernizasyon steady-state" kural icerigi simdiden kesinlestirilmez.
- Zorunlu tetikleyici: modernizasyon ilerleme durumu `%70+` oldugunda veya Sprint 6 basinda bir "Rule Refactor Review" yapilir.
- Tetikleyici uygulanma kaydi: Sprint 6 `EASY-049` (2026-04-17) tamamlandi; kararlar `rules/current/rule_refactor_review_2026-04-17.md` dokumaninda tutulur.
- Bu gozden gecirmede:
  - Gecis-donemi kurallari `deprecated`/`active` olarak yeniden siniflanir.
  - Kalici operasyon kurallari netlestirilir.
  - `core_rules`, `operational_rules`, `rule_registry`, `rules_changelog` birlikte guncellenir.

## Kapsam Disi Notu

- `tmp/` altindaki dosyalar template referansidir.
- easyROC icin baglayici kural kaynagi degildir.
