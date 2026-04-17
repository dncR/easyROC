# easyROC Env ve Secret Yonetimi (EASY-041)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-041

## 1) Scope

Bu dokuman easyROC deployment topolojisi icin ortam degiskeni ve secret yonetimi standardini tanimlar.
Hedef: staging/prod ortamlarinda konfig ve secret drift riskini azaltmak.

## 2) Variable Inventory

| Variable | Required | Secret | Source | Notes |
| --- | --- | --- | --- | --- |
| `EASYROC_IMAGE` | yes | no | env file | Container image repository |
| `EASYROC_IMAGE_TAG` | yes | no | env file / CI output | Staging'de dogrulanan tag production'a promote edilir |
| `EASYROC_HOST_PORT` | yes | no | env file | Host port binding |
| `R_CONFIG_ACTIVE` | yes | no | env file | `staging` veya `production` |
| `EASYROC_LOG_LEVEL` | no | no | env file | `DEBUG/INFO/WARN/ERROR` |
| `EASYROC_LOG_FILE` | no | no | env file | Opsiyonel log file sink |
| `EASYROC_HEALTHCHECK_WAIT_SECONDS` | no | no | env file | Deploy script health wait timeout (sn) |
| `SHINY_HOST` | optional | no | compose default | Varsayilan: `0.0.0.0` |
| `SHINY_PORT` | optional | no | compose default | Varsayilan: `3838` |

Not:

- Mevcut easyROC uygulama runtime'i icin zorunlu bir app-level secret degiskeni yoktur.
- Gelecekte eklenecek secret'lar env dosyasina hard-code edilmez, secret store veya ayri secret env dosyasi ile enjekte edilir.

## 3) File Convention

- Public template dosyalari:
  - `.env.example`
  - `.env.staging.example`
  - `.env.production.example`
  - `.env.secrets.example`
- Local/private dosyalar (repo disi):
  - `.env.staging`
  - `.env.production`
  - `.env.secrets.*`

Git guvencesi:

- `.env` ve `.env.*` dosyalari ignore edilir.
- Sadece `*.example` template dosyalari repoda tutulur.

## 4) Validation Gate

Deploy oncesi env dogrulama zorunludur:

```bash
scripts/validate_env.sh .env.staging staging
scripts/validate_env.sh .env.production production .env.secrets.production
```

`scripts/validate_env.sh` kontrolleri:

- required variable varligi
- `R_CONFIG_ACTIVE` ortam uyumu
- port formati
- production'da `EASYROC_IMAGE_TAG=local` yasagi
- production env dosyasinda secret benzeri anahtarlarin tutulmama kontrolu

## 5) Deploy Integration

Deploy scripti validation gate'i otomatik uygular:

```bash
scripts/deploy_compose.sh staging .env.staging
scripts/deploy_compose.sh production .env.production .env.secrets.production
```

Akis:

1. Env validation
2. Compose up
3. Service status (`docker compose ... ps`)

## 6) Secret Policy

- Secret'lar kaynak kodda, compose yaml icinde veya public env template dosyalarinda tutulmaz.
- Production secret'lari platform secret manager'dan enjekte edilir.
- Gecici local test secret'lari sadece ignore edilen secret env dosyasinda tutulur.
- Secret degisikliginde en az yillik veya olay-bazli rotation uygulanir.

## 7) Incident Checklist

Secret sizintisi suphe durumunda:

1. Ilgili secret'i derhal rotate et
2. Eski secret'i revoke et
3. Son deployment artifact ve env kaynaklarini denetle
4. Olay kaydini runbook/changelog uzerinden dokumante et
