# Rules Sistemi (easyROC)

Last updated: 2026-04-16
Status: active

Bu klasor easyROC kural yonetiminin ana kaynagidir.

## Dizin Yapisi

- `current/core_rules.md`: daha stabil ve `strict` kurallar
- `current/operational_rules.md`: surece gore guncellenebilen `flexible` kurallar
- `project_rules_entrypoint.md`: aktif okuma giris dosyasi
- `current/rule_registry.md`: kural envanteri
- `current/rules_changelog.md`: kural degisiklik gunlugu
- `current/rule_template.md`: yeni kural sablonu
- `session_handoff.md`: oturumlar arasi baglam devri
- `archive/`: eski/legacy referanslar

## Okuma Sirasi

1. `/AGENTS.md`
2. `rules/project_rules_entrypoint.md`
3. `rules/current/core_rules.md`
4. `rules/current/operational_rules.md`
5. `rules/session_handoff.md`
6. Goreve uygun `workplans/` dosyalari (`Status: active` + `Scope` eslesmesi)

## Bakim Kurali

Kural degisikligi yaptiginda:

1. Hedef dosyayi guncelle
2. `rules/current/rule_registry.md` guncelle
3. `rules/current/rules_changelog.md` kaydi ekle
