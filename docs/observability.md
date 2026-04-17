# easyROC Observability Guide (EASY-042)

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-042

## 1) Scope

Bu dokuman easyROC icin loglama ve hata izleme taban cizgisini tanimlar.
Amac: staging/production ortaminda olay tespiti ve kok neden analizi icin yeterli sinyali saglamak.

## 2) Logging Model

- Uygulama structured text log uretir (`key=value` satir formati).
- Log cikisi varsayilan olarak `stderr` uzerindedir (container log aggregatori tarafindan toplanir).
- Opsiyonel olarak `EASYROC_LOG_FILE` ile ek dosya sink tanimlanabilir.

Uygulama log seviyeleri:

- `DEBUG`
- `INFO`
- `WARN`
- `ERROR`

Varsayilan seviye: `INFO`

## 3) Config Variables

| Variable | Required | Default | Notes |
| --- | --- | --- | --- |
| `EASYROC_LOG_LEVEL` | no | `INFO` | Min log severity |
| `EASYROC_LOG_FILE` | no | empty | Opsiyonel local file sink |

Bu degiskenler compose/env katmaninda tanimlanir ve `scripts/validate_env.sh` tarafindan dogrulanir.

## 4) Event Catalog (Initial)

| Event | Level | Source | Description |
| --- | --- | --- | --- |
| `session_started` | INFO | `server.R` | Shiny session acilisi |
| `session_ended` | INFO | `server.R` | Shiny session kapanisi |
| `example_dataset_loaded` | INFO | `mod_data_upload` | Ornek dataset secimi |
| `upload_parsed` | INFO | `mod_data_upload` | Upload parse basarili |
| `upload_parse_failed` | WARN | `mod_data_upload` | Upload parse/validation hatasi |
| `sample_size_calculation_failed` | ERROR | `mod_sample_size` | Sample size hesaplama hatasi |

## 5) Operational Queries (Container)

Canli log izleme:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs -f easyroc
```

Yalniz hata/warn filtreleme:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs easyroc | rg "level=(WARN|ERROR)"
```

Session bazli takip:

```bash
docker compose --env-file .env.staging -f docker-compose.yml -f docker-compose.staging.yml logs easyroc | rg "session="
```

## 6) Guardrails

- Secret benzeri veriler log context'ine yazilmaz.
- Production ortami icin log seviyesi `INFO` veya daha yuksek gurultu azaltici bir seviye olmalidir.
- `EASYROC_LOG_FILE` kullanilsa da birincil kaynak container stdout/stderr'dir.

## 7) Next Step Link

- EASY-043: health-check/readiness metrikleri devreye alinarak log + health sinyali birlikte izlenecek.

EASY-043 update (2026-04-17):

- Health-check/readiness kontrati `docs/healthchecks.md` ve `scripts/healthcheck.R` ile devreye alinmistir.
