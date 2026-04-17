# easyROC Container Runtime Recipe (EASY-040)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-040

## 1) Scope

Bu dokuman, `docs/deployment-target-architecture.md` icindeki container-first kararini calisir bir receteye cevirir.

## 2) Uretilen Artefaktlar

- `Dockerfile`
- `.dockerignore`
- `docker-compose.yml`
- `docker-compose.staging.yml`
- `docker-compose.production.yml`
- `.env.example`
- `.env.staging.example`
- `.env.production.example`
- `.env.secrets.example`
- `scripts/deploy_compose.sh`
- `scripts/validate_env.sh`
- `scripts/healthcheck.R`

## 3) Hazirlik

1. Env dosyasi olustur:

```bash
cp .env.staging.example .env.staging
```

2. Gerekirse `EASYROC_IMAGE`, `EASYROC_IMAGE_TAG`, `EASYROC_HOST_PORT`, `EASYROC_LOG_LEVEL`, `EASYROC_HEALTHCHECK_WAIT_SECONDS` degerlerini duzenle.
3. (Opsiyonel) secret degiskenleri icin ayri dosya kullan:

```bash
cp .env.secrets.example .env.secrets.staging
```

## 4) Staging Deploy (Build + Run)

Staging ortami image'i build ederek ayaga kaldirir:

```bash
scripts/deploy_compose.sh staging .env.staging
```

Bu komut altta su compose setini kullanir:

- `docker-compose.yml`
- `docker-compose.staging.yml`

## 5) Production Deploy (Promote Ayni Image)

Production adiminda ayni image tag/digest kullanilir, rebuild edilmez:

```bash
cp .env.production.example .env.production
# .env.production icinde EASYROC_IMAGE_TAG degerini staging'de dogrulanan tag'e cekin
scripts/deploy_compose.sh production .env.production .env.secrets.production
```

Bu komut altta su compose setini kullanir:

- `docker-compose.yml`
- `docker-compose.production.yml`

## 6) Temel Operasyon Komutlari

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs -f
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml ps
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml down
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs easyroc | rg "level=(WARN|ERROR)"
```

## 7) Guvence Notlari

- Container non-root user (`appuser`) ile calistirilir.
- Bagimliliklar `renv.lock` uzerinden restore edilir.
- Production deploy'da rebuild kapali oldugu icin immutable promote modeli korunur.
- Deploy oncesi env validation `scripts/validate_env.sh` ile zorunlu olarak uygulanir.
- Deploy sonrasi readiness kapisi `scripts/deploy_compose.sh` ile container `healthy` durumuna gecisi bekler.
