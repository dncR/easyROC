# easyROC Release Evidence - rc-20260417193256

Last updated: 2026-04-17  
Status: completed  
Owner: team  
Related Sprint Issue: EASY-046

## 1) Release Metadata

- Candidate image: `easyroc-release:rc-20260417193256`
- Staging deploy env: `/tmp/easyroc-release-staging.env`
- Production deploy env: `/tmp/easyroc-release-production.env`
- Secrets env: `/tmp/easyroc-release-secrets.env`

## 2) Pre-Release Gate Evidence

- `Rscript --vanilla scripts/lint.R` -> passed
- `Rscript --vanilla scripts/static_checks.R` -> passed
- `Rscript --vanilla tests/testthat.R` -> passed (known shinytest2/chrome skip behavior remains)
- `scripts/validate_env.sh /tmp/easyroc-release-staging.env staging` -> passed
- `scripts/validate_env.sh /tmp/easyroc-release-production.env production /tmp/easyroc-release-secrets.env` -> passed

## 3) Staging Deploy Evidence

- Deploy command:
  - `scripts/deploy_compose.sh staging /tmp/easyroc-release-staging.env /tmp/easyroc-release-secrets.env`
- Result:
  - Container `easyroc-staging` reached `healthy`
  - HTTP smoke on `http://127.0.0.1:3838` returned app HTML (`easyROC` title)

## 4) Production Promote Evidence

- Promote command:
  - `scripts/deploy_compose.sh production /tmp/easyroc-release-production.env /tmp/easyroc-release-secrets.env`
- Result:
  - Container `easyroc-production` reached `healthy`
  - Host mapping used for local validation: `3839 -> 3838`
  - HTTP smoke on `http://127.0.0.1:3839` returned app HTML (`easyROC` title)

## 5) Log Review Notes

- No hard startup failure observed after final image fix.
- Observed warnings (non-blocking for this release):
  - `shiny::dataTableOutput()` deprecation warnings (recommend migration to `DT::DTOutput()` in follow-up)
  - `Shiny Server v0.3.4 or later is required; please upgrade!` warning text from app startup context

## 6) Technical Fix Applied During Release

Deployment gating uncovered containerization gaps. Final release image fix included:

- Docker system dependencies added: `curl`, `pkg-config`, `zlib1g-dev`
- Build-time restore pinned to system library:
  - `RENV_PATHS_LIBRARY=/usr/local/lib/R/site-library`
- Runtime command simplified:
  - Removed explicit `source('renv/activate.R')` from container `CMD`
  - Added `RENV_CONFIG_AUTOLOADER_ENABLED=FALSE` to avoid runtime bootstrap drift

These fixes are tracked in `Dockerfile`.

## 7) Post-Release Monitoring Link (EASY-047)

- Monitoring report:
  - `docs/post-release-monitoring-rc-20260417193256.md`
- Monitoring outcome:
  - EASY-047 completed (`2026-04-18 02:07 +03`), no P0/P1 incident, no hotfix required
