# Kural Degisiklik Gunlugu - easyROC

Bu dosya kural sistemindeki anlamli degisiklikleri izler.

## 2026-04-17

- Eklendi: `rules/current/rule_refactor_review_2026-04-17.md` dokumani ile EASY-049 Rule Refactor Review kararlari (keep/deprecate/revise) yazili hale getirildi.
- Degisti: `core_rules.md` icinde CR-003 steady-state mimari disiplini kapsayacak sekilde revize edildi.
- Degisti: `operational_rules.md` icinde OP-009 durumu `deprecated` yapildi (tetikleyici tamamlandi) ve OP-011 kalici review cadence kurali eklendi.
- Degisti: `rule_registry.md` CR-003/OP-009/OP-011 kayitlari EASY-049 sonucuyla senkronlandi.
- Eklendi: `operational_rules.md` icine OP-010 kurali eklendi; her gorev sonunda background process kontrolu, gereksiz/kilitli proseslerin raporlanmasi ve manuel kill komutu sunulmasi varsayilan hale getirildi. Acik kullanici talebi veya kural degisikligi ile otomatik kill moduna gecilebilecegi notu eklendi.
- Degisti: `rule_registry.md` OP-010 kaydi ile guncellendi.
- Eklendi: `AGENTS.md` altina "Post-Modernizasyon Cerceve Notu" eklendi; "simdi cerceve sonra icerik" yaklasimi resmi olarak tanimlandi.
- Eklendi: `operational_rules.md` icine OP-009 kurali eklendi; modernizasyon `%70+` veya Sprint 6 basinda zorunlu "Rule Refactor Review" tetikleyicisi tanimlandi.
- Degisti: `rule_registry.md` OP-009 kaydi ile guncellendi.
- Eklendi: CR-007 ile agent tarafinda otomatik `git commit`/`git push` yasagi strict kural olarak tanimlandi.
- Degisti: `AGENTS.md`, `core_rules.md` ve `rule_registry.md` bu politika ile hizalandi.

## 2026-04-16

- Eklendi: easyROC icin kok dizinde `AGENTS.md` olusturuldu.
- Eklendi: `rules/` altinda yaln kural sistemi kuruldu (`core`, `operational`, `registry`, `changelog`, `template`, `session_handoff`).
- Eklendi: kural onceligi, oturum baslangic protokolu ve workplan metadata standardi tanimlandi.
- Degisti: `workplans/easyroc-modernizasyon-r-first.md` dosyasina metadata basligi eklendi ve kural standardina alindi.
- Degisti: `workplans/easyroc-sprint-issue-plani.md` dosyasina metadata basligi eklendi ve kural standardina alindi.
