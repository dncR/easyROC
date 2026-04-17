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
- `scripts/deploy_compose.sh`

## 3) Hazirlik

1. Env dosyasi olustur:

```bash
cp .env.example .env.staging
```

2. Gerekirse `EASYROC_IMAGE`, `EASYROC_IMAGE_TAG`, `EASYROC_HOST_PORT` degerlerini duzenle.

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
cp .env.example .env.production
# .env.production icinde EASYROC_IMAGE_TAG degerini staging'de dogrulanan tag'e cekin
scripts/deploy_compose.sh production .env.production
```

Bu komut altta su compose setini kullanir:

- `docker-compose.yml`
- `docker-compose.production.yml`

## 6) Temel Operasyon Komutlari

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs -f
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml ps
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml down
```

## 7) Guvence Notlari

- Container non-root user (`appuser`) ile calistirilir.
- Bagimliliklar `renv.lock` uzerinden restore edilir.
- Production deploy'da rebuild kapali oldugu icin immutable promote modeli korunur.

