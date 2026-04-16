# Operasyonel Kurallar (Flexible) - easyROC

Last updated: 2026-04-17
Status: active

Bu kurallar surece gore guncellenebilir.

| Rule ID | Rule | Status | Change Policy | Owner | Last Review |
| --- | --- | --- | --- | --- | --- |
| OP-001 | Her goreve baslarken `AGENTS.md` ve giris akisina gore ilgili kural/workplan dosyalari okunur. | active | flexible | team | 2026-04-16 |
| OP-002 | `workplans/` altindaki planlarda metadata alanlari (`Workplan ID`, `Status`, `Scope`, `Owner`, `Last updated`) korunur. | active | flexible | team | 2026-04-16 |
| OP-003 | Kod veya mimari akis degisirse ilgili workplan ve teknik dokuman ayni degisiklikte guncellenir. | active | flexible | team | 2026-04-16 |
| OP-004 | Domain hesaplama fonksiyonlarina dokunuldugunda en az birim test seviyesi ve gerekiyorsa regresyon kontrolu eklenir/guncellenir. | active | flexible | team | 2026-04-16 |
| OP-005 | Buyuk degisiklikler kucuk ve inceleme dostu PR dilimlerine bolunur; tek seferde buyuk kontrolsuz patchlerden kacilinir. | active | flexible | team | 2026-04-16 |
| OP-006 | Oturum sonunda acik risk, sonraki adim ve blokajlar `rules/session_handoff.md` dosyasinda guncel tutulur (gerekiyorsa). | active | flexible | team | 2026-04-16 |
| OP-007 | Kural dosyalarinda degisiklik yapildiysa `rule_registry` ve `rules_changelog` dosyalari ayni patchte guncellenir. | active | flexible | team | 2026-04-16 |
| OP-008 | Sprint/issue odakli ilerlemede workplanlar ile issue kimlikleri birlikte izlenir ve kapanis kriteri net yazilir. | active | flexible | team | 2026-04-16 |
| OP-009 | Post-modernizasyon kural icerigi simdiden detaylandirilmaz; modernizasyon `%70+` seviyesine geldiginde veya Sprint 6 basinda zorunlu "Rule Refactor Review" yapilir ve sonucunda kural seti yeniden siniflandirilir. | active | flexible | team | 2026-04-17 |

## Degisim Is Akisi

1. Ilgili dosyayi guncelle
2. `rules/current/rule_registry.md` kaydini guncelle
3. `rules/current/rules_changelog.md` kaydi ekle
