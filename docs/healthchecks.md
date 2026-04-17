# easyROC Health Check ve Readiness (EASY-043)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-043

## 1) Scope

Bu dokuman easyROC container calisma modelinde liveness/readiness dogrulama akislarini tanimlar.

## 2) Health Check Mekanizmasi

- Runtime probe scripti: `scripts/healthcheck.R`
- Compose healthcheck komutu:

```yaml
healthcheck:
  test: ["CMD", "Rscript", "--vanilla", "scripts/healthcheck.R", "readiness"]
```

Probe kontrolleri:

- `readiness` modunda kritik dosyalarin varligi (`app.R`, `ui.R`, `server.R`, `R/logging_utils.R`)
- `127.0.0.1:${SHINY_PORT}` uzerinden HTTP probe
- HTTP 2xx/3xx/4xx yanitlari "servis ayakta" sinyali olarak kabul edilir; 5xx veya yanit yoksa probe fail olur

## 3) Deploy Sirasinda Readiness Bekleme

`scripts/deploy_compose.sh` artik `up` sonrasinda container health durumunu bekler:

- `healthy` => deploy basarili
- `unhealthy` => deploy fail + son loglar
- timeout => fail + son loglar

Zaman asimi env ile ayarlanir:

- `EASYROC_HEALTHCHECK_WAIT_SECONDS` (default: 120)

## 4) Manual Komutlar

Container health durumunu gormek:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml ps
```

Probe scriptini manuel calistirmak:

```bash
Rscript --vanilla scripts/healthcheck.R readiness
Rscript --vanilla scripts/healthcheck.R liveness
```

## 5) Operasyonel Notlar

- Health-check sinyali EASY-042 log sinyalleri ile birlikte yorumlanmalidir.
- Health-check fail durumunda once `docker compose ... logs easyroc` ile son hata olaylari incelenir.

