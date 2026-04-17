# easyROC Branch ve PR Kurallari

Last updated: 2026-04-17
Owner: team

## 1) Temel Ilke

- Agent bu depoda `git commit` veya `git push` calistirmaz.
- Commit/push islemleri kullanici tarafinda manuel yurutulur.
- Bu politika `CR-007` ile strict kuraldir.

## 2) Branch Isimlendirme

Onerilen format:

- `feature/<kisa-tanim>`
- `fix/<kisa-tanim>`
- `refactor/<kisa-tanim>`
- `docs/<kisa-tanim>`

Issue baglantili calisma varsa:

- `fix/easy-006-status-event-handling`
- `refactor/easy-020-roc-module`

## 3) PR Boyutu ve Kapsam

- Tek PR = tek amac
- Buyuk degisiklikleri kucuk dilimlere bol
- Kod + test + ilgili dokuman ayni PR'da guncellenir

## 4) PR Kontrol Listesi

- [ ] Kapsam issue/workplan ile uyumlu
- [ ] Kritik degisiklikler icin test eklendi/guncellendi
- [ ] Dokuman guncellendi (`workplans`, `docs`, gerekiyorsa `rules`)
- [ ] Regresyon riski notu eklendi
- [ ] CI adimlari (varsa) gecti
- [ ] `docs/process/quality_gates.md` altindaki lint + static check komutlari lokalde yesil

## 5) Merge Kosullari

- En az 1 teknik review
- Kritik bugfixlerde ilgili baseline cikti karsilastirma notu
- Kural/protokol degisikliklerinde `rule_registry` + `rules_changelog` guncellemesi

## 6) Yasakli Akislar

- Acik onay olmadan destructive git komutlari
- Test ve dokumani ayri PR'a birakma
- "WIP ama merge edelim" tarzinda kontrolsuz birlestirme
