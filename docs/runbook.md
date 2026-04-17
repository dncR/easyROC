# easyROC Operations Runbook (EASY-044)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-044

## 1) Scope

Bu runbook, easyROC staging/production operasyonunda gunluk isletim ve incident mudahale adimlarini listeler.

## 2) Service Inventory

- Uygulama servisi: `easyroc`
- Runtime: Docker Compose
- Port: `EASYROC_HOST_PORT` (default `3838`)
- Health probe: `scripts/healthcheck.R`

## 3) Day-1 Operations

Staging deploy:

```bash
scripts/deploy_compose.sh staging .env.staging
```

Production deploy:

```bash
scripts/deploy_compose.sh production .env.production .env.secrets.production
```

## 4) Day-2 Operations

Durum kontrolu:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml ps
```

Canli log:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs -f easyroc
```

Hata odakli log:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs easyroc | rg "level=(WARN|ERROR)"
```

## 5) Standard Incident Playbooks

### A) Health check `unhealthy`

1. Son loglari al:
```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs --tail 120 easyroc
```
2. Env dogrulama komutunu calistir:
```bash
scripts/validate_env.sh .env.staging staging
```
3. Servisi yeniden baslat:
```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml restart easyroc
```
4. Hala sagliksizsa rollback uygula (`docs/deployment.md`).

### B) Yuksek WARN/ERROR hacmi

1. Problemli event pattern'lerini ayikla (`upload_parse_failed`, `sample_size_calculation_failed` vb.).
2. Son degisiklik/deploy tag bilgisini dogrula.
3. Gerekirse `EASYROC_LOG_LEVEL` gecici olarak `DEBUG`e cek ve tekrar deploy et.
4. Kok neden analizi tamamlaninca seviye tekrar `INFO`a dondur.

### C) Secret sizintisi suphe durum

1. Secret rotate et ve eskisini revoke et.
2. Env kaynaklarini denetle (`.env.*` dosyalari + secret store kayitlari).
3. Olay kaydini dokumante et (incident raporu + changelog).

## 6) Escalation

Asagidaki durumlarda acil eskalasyon:

- Production'da 15 dakikayi asan kesinti
- Ardisik deploy denemelerinde health check fail
- Secret sizintisi suphe veya teyidi

## 7) Post-Incident Checklist

1. Impact ve root cause kaydi yazildi.
2. Kalici aksiyon maddesi sprint issue'suna donusturuldu.
3. Gerekliyse bu runbook/deployment dokumani guncellendi.

## 8) Related Docs

- `docs/deployment.md`
- `docs/deployment-target-architecture.md`
- `docs/observability.md`
- `docs/healthchecks.md`
- `docs/env-secret-management.md`
- `docs/release-checklist.md`
