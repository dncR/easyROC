# Cekirdek Kurallar (Strict) - easyROC

Last updated: 2026-04-17
Status: active

Bu kurallar stabil ve degisimi kontrollu kurallardir.

| Rule ID | Rule | Status | Change Policy | Owner | Last Review |
| --- | --- | --- | --- | --- | --- |
| CR-001 | easyROC modernizasyonunda istatistiksel davranis esdegerligi korunur; domain hesap mantigi degisikligi test ve acik onay olmadan kalici hale getirilmez. | active | strict | team | 2026-04-16 |
| CR-002 | Acik talep olmadan yikici git/dosya komutlari (`reset --hard`, geri alinamaz silme vb.) uygulanmaz. | active | strict | team | 2026-04-16 |
| CR-003 | Monolitik yapida kalici yeni genisleme yerine moduler hedef mimari (`app.R`, `R/mod_*`, `R/domain/*`) korunur; acil hotfix disinda teknik borcu buyuten eklemeler yapilmaz. | active | strict | team | 2026-04-16 |
| CR-004 | Paket veya calisma ortami degisiklikleri surumlenebilir ve tekrarlanabilir olmak zorundadir; bagimlilik yonetimi dokumante edilir. | active | strict | team | 2026-04-16 |
| CR-005 | Davranis degisikligi olusturan her teknik degisiklikte ilgili test ve dokumantasyon ayni degisiklik setinde guncellenir. | active | strict | team | 2026-04-16 |
| CR-006 | `tmp/` klasoru template referansidir; easyROC icin baglayici kural kaynagi olarak kullanilmaz. | active | strict | team | 2026-04-16 |
| CR-007 | Agent bu depoda otomatik `git commit`/`git push` yapmaz; versiyonlama adimi kullanici tarafindan manuel yurutulur. | active | strict | team | 2026-04-17 |

## Notlar

- `strict` bir kuralla celisen talepte, uygulama adimina gecmeden once explicit onay alin.
