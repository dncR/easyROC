# easyROC Post-Release Monitoring - rc-20260417193256 (EASY-047)

Last updated: 2026-04-17 22:53 +03  
Status: in_progress  
Owner: team  
Related Sprint Issue: EASY-047

## 1) Monitoring Window

- Production container start (UTC): `2026-04-17T19:46:09Z`
- Production container start (+03): `2026-04-17 22:46:09 +03`
- Monitoring window target: first 60 minutes after production promote
- This snapshot capture time (+03): `2026-04-17 22:53`

## 2) Health and Runtime Signals

- Compose status: `easyroc-production` is `healthy`
- Restart count: `0`
- Port mapping: `3839 -> 3838`
- HTTP smoke: app HTML/title returned successfully from `http://127.0.0.1:3839`

## 3) Log Review (Critical Pattern Scan)

Critical pattern scan performed with:

- `ERROR`
- `FATAL`
- `Traceback`
- `upload_parse_failed`
- `sample_size_calculation_failed`

Result:

- No matching critical pattern found.

Observed non-critical warnings:

- `shiny::dataTableOutput()` deprecation warnings
- `Warning in shiny::runApp(...): Shiny Server v0.3.4 or later is required; please upgrade!`

## 4) Incident / Hotfix Decision

- Incident status: none
- Hotfix required: no
- Continue monitoring until the 60-minute window closes.

## 5) Next Checkpoint

At `2026-04-17 23:46 +03`:

1. Re-run production `ps` / health checks.
2. Re-run log critical-pattern scan.
3. If still no P0/P1 incident, close EASY-047 and proceed to EASY-048.
