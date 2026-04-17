# easyROC Deployment Guide (EASY-044)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-044

## 1) Scope

Bu dokuman easyROC uygulamasinin staging ve production ortamlari icin standart deployment akisini tanimlar.

## 2) Prerequisites

- Docker Engine + Docker Compose
- Repo icinde guncel kaynak kod
- Uygun env dosyalari:
  - `.env.staging`
  - `.env.production`
  - opsiyonel: `.env.secrets.*`

Hazirlik icin template dosyalar:

- `.env.staging.example`
- `.env.production.example`
- `.env.secrets.example`

## 3) Staging Deployment

1. Env dosyasini hazirla:

```bash
cp .env.staging.example .env.staging
```

2. Staging deploy komutunu calistir:

```bash
scripts/deploy_compose.sh staging .env.staging
```

Beklenen sonuc:

- `easyroc` container ayakta
- health durumu `healthy`
- `docker compose ... ps` ciktisinda servis gorunur

## 4) Production Deployment (Promote)

1. Staging'de dogrulanan image tag'i `.env.production` dosyasina gir:

```bash
cp .env.production.example .env.production
```

2. Production deploy:

```bash
scripts/deploy_compose.sh production .env.production .env.secrets.production
```

Not:

- Production deploy adiminda rebuild yapilmaz.
- Ayni image tag/digest staging -> production promote edilir.

## 5) Verification Checklist

Deploy sonrasi asgari kontroller:

1. Container health:
```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml ps
```
2. Warn/Error log taramasi:
```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs easyroc | rg "level=(WARN|ERROR)"
```
3. Kritik akis smoke:
- Data upload
- ROC statistics
- Partial AUC
- Cut points
- Sample size

## 6) Rollback

Rollback hedefi: son stabil image tag/digest'e geri donmek.

1. `.env.production` dosyasinda `EASYROC_IMAGE_TAG` degerini onceki stabil tag'e cek.
2. Deploy komutunu tekrar calistir:

```bash
scripts/deploy_compose.sh production .env.production .env.secrets.production
```

3. Health/log/smoke kontrollerini yeniden uygula.

## 7) Related Docs

- `docs/deployment-target-architecture.md`
- `docs/container-runtime-recipe.md`
- `docs/env-secret-management.md`
- `docs/observability.md`
- `docs/healthchecks.md`
- `docs/runbook.md`

