# easyROC Release Checklist (EASY-045)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-045

## 1) Scope

Bu dokuman production release oncesi zorunlu kalite, operasyon ve onay adimlarini tek bir kontrol listesi halinde toplar.

## 2) Checklist Usage

- Her release adayi icin checklist kopyasi acilir.
- Tum `P0 gate` maddeleri tamamlanmadan production release yapilmaz.
- Kanit baglantilari (log, CI run, dokuman linki) checklist'e eklenir.

## 3) Pre-Release Gates (P0)

- [ ] CI gates green (`CI / lint`, `CI / test`)
- [ ] `Rscript --vanilla scripts/lint.R` gecti
- [ ] `Rscript --vanilla scripts/static_checks.R` gecti
- [ ] `Rscript --vanilla tests/testthat.R` gecti (skip varsa nedeni kayitli)
- [ ] Env validation gecti:
  - [ ] `scripts/validate_env.sh .env.staging staging`
  - [ ] `scripts/validate_env.sh .env.production production .env.secrets.production`
- [ ] Staging deploy gecti:
  - [ ] `scripts/deploy_compose.sh staging .env.staging`
  - [ ] container health `healthy`
- [ ] Staging kritik akis smoke tamamlandi:
  - [ ] Data upload
  - [ ] ROC statistics
  - [ ] Partial AUC
  - [ ] Cut points
  - [ ] Sample size
- [ ] Warn/Error log taramasi kabul edilebilir seviyede:
  - [ ] `docker compose ... logs easyroc | rg "level=(WARN|ERROR)"`
- [ ] Rollback adimi dry-run olarak dogrulandi (tag geri cekme senaryosu)

## 4) Go/No-Go Approval

- [ ] Teknik onay (owner)
- [ ] Operasyon onayi
- [ ] Product/stakeholder onayi

Release karari:

- [ ] GO
- [ ] NO-GO (gerekce kaydi yapildi)

## 5) Release Metadata

- Planned release date:
- Candidate image/tag:
- Staging validation window:
- Production window:
- Responsible engineer:

## 6) Evidence Links

- CI run:
- Staging deploy log:
- Health status ciktisi:
- Smoke test notu:
- Risk/known issue notu:

## 7) Post-Release Hand-off (EASY-046 -> EASY-047)

- [ ] Production deploy tamamlandi
- [ ] Ilk 60 dk izleme tamamlandi
- [ ] Incident yoksa EASY-047 izleme planina gecildi
- [ ] Incident varsa runbook adimlari ile kayit acildi

## 8) Related Docs

- `docs/deployment.md`
- `docs/runbook.md`
- `docs/healthchecks.md`
- `docs/observability.md`
- `docs/env-secret-management.md`

